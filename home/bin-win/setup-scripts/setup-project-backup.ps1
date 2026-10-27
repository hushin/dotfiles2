# setup-project-backup.ps1
# Setup automatic backup for project directories using restic
# The restic password is stored in 1Password and fetched via the 1Password CLI.

param(
    [string]$OpVault = "Private",
    [string]$OpItem = "restic"
)

# Windows PowerShell 5.1 でネイティブコマンドの stderr をリダイレクトしたときに
# 例外にならないよう、明示的に Continue にしておく。
$ErrorActionPreference = "Continue"

# Configuration
$BackupDrive = "E:"
$SourcePath = "$env:USERPROFILE\projects"
$BackupPath = "$BackupDrive\restic-backup\projects"
$ConfigPath = "$env:USERPROFILE\.config\restic"
$LogPath = "$env:USERPROFILE\.logs\restic"
$BackupScriptPath = "$ConfigPath\quick-project-backup.ps1"
$HelperPath = "$ConfigPath\restic-1password.ps1"
$OpRef = "op://$OpVault/$OpItem/password"
$OpRefFile = "$ConfigPath\1password-ref.txt"

# Check if restic is installed
if (-not (Get-Command restic -ErrorAction SilentlyContinue)) {
    # ref. https://github.com/restic/restic/issues/5108
    Write-Host "restic が見つかりません。管理者権限で PowerShell を開き、以下のコマンドを実行してください:" -ForegroundColor Yellow
    Write-Host "winget install restic.restic" -ForegroundColor Cyan
    exit 1
}

# Ensure required directories exist
Write-Host "Creating required directories..."
New-Item -ItemType Directory -Path $ConfigPath -Force | Out-Null
New-Item -ItemType Directory -Path $LogPath -Force | Out-Null
New-Item -ItemType Directory -Path $SourcePath -Force | Out-Null
New-Item -ItemType Directory -Path $BackupPath -Force | Out-Null

# Set up the restic password in 1Password
if (-not (Test-Path -LiteralPath $HelperPath)) {
    Write-Error "1Password helper not found: $HelperPath (run 'chezmoi apply' first)"
    exit 1
}
. $HelperPath

try {
    Assert-OnePasswordSignedIn
}
catch {
    Write-Error $_.Exception.Message
    exit 1
}

if (-not (Test-ResticOnePasswordItem -Vault $OpVault -Item $OpItem)) {
    Write-Host "1Password に項目 '$OpItem' が見つかりません (vault: $OpVault)。" -ForegroundColor Yellow
    $create = Read-Host "パスワードを自動生成して 1Password に作成しますか? (y/N)"
    if ($create -notmatch '^[Yy]$') {
        Write-Host "1Password で '$OpVault' vault に '$OpItem' (Password) を作成してから再実行してください。"
        exit 1
    }

    try {
        New-ResticOnePasswordItem -Vault $OpVault -Item $OpItem
    }
    catch {
        Write-Error $_.Exception.Message
        exit 1
    }
}

try {
    $null = Get-ResticPasswordFromOnePassword -Reference $OpRef
}
catch {
    Write-Error $_.Exception.Message
    exit 1
}

# 1Password の vault / item 名に日本語が含まれても壊れないよう UTF-8 (BOM なし) で保存する
if (Test-Path -LiteralPath $OpRefFile) {
    $existingRef = [System.IO.File]::ReadAllText($OpRefFile, [System.Text.UTF8Encoding]::new($false)).Trim()
    if ($existingRef -and $existingRef -ne $OpRef) {
        Write-Host ""
        Write-Host "WARNING: 1Password reference を変更します: $existingRef -> $OpRef" -ForegroundColor Yellow
        Write-Host "既存の restic リポジトリは旧 reference のパスワードで作成されています。" -ForegroundColor Yellow
        Write-Host "旧パスワードが password.txt に残っている場合のみ migrate-restic-password.ps1 で移行できます。" -ForegroundColor Yellow
        Write-Host ""
    }
}
[System.IO.File]::WriteAllText($OpRefFile, $OpRef, [System.Text.UTF8Encoding]::new($false))
Write-Host "1Password reference saved at: $OpRefFile"

# 旧パスワードファイルが残っている場合は、移行が完了するまで削除しない
$LegacyPasswordFile = "$ConfigPath\password.txt"
if (Test-Path -LiteralPath $LegacyPasswordFile) {
    Write-Host ""
    Write-Host "WARNING: 旧パスワードファイルが見つかりました: $LegacyPasswordFile" -ForegroundColor Yellow
    Write-Host "既存の restic リポジトリは旧パスワードで作成されています。" -ForegroundColor Yellow
    Write-Host "migrate-restic-password.ps1 で 1Password のパスワードへ移行してから削除してください。" -ForegroundColor Yellow
    Write-Host ""
}

# Create task scheduler task
Write-Host "Creating scheduled task..."
$TaskName = "ProjectBackup"
$TaskDescription = "Automatic backup of project directories using restic"

# Remove existing task if it exists
$ExistingTask = Get-ScheduledTask -TaskName $TaskName -ErrorAction SilentlyContinue
if ($ExistingTask) {
    Unregister-ScheduledTask -TaskName $TaskName -Confirm:$false
    Write-Host "Removed existing task: $TaskName"
}

# Create new task
$Action = New-ScheduledTaskAction -Execute "conhost.exe" -Argument "--headless powershell -NoProfile -ExecutionPolicy Bypass -File `"$BackupScriptPath`""
$Trigger = New-ScheduledTaskTrigger -Once -At (Get-Date) -RepetitionInterval (New-TimeSpan -Minutes 30)
$Settings = New-ScheduledTaskSettingsSet -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries -StartWhenAvailable
$Principal = New-ScheduledTaskPrincipal -UserId $env:USERNAME -LogonType Interactive

Register-ScheduledTask -TaskName $TaskName -Action $Action -Trigger $Trigger -Settings $Settings -Principal $Principal -Description $TaskDescription
Write-Host "Scheduled task '$TaskName' created successfully"

# Test restic installation
Write-Host "Testing restic installation..."
try {
    $resticVersion = & restic version 2>&1
    if ($LASTEXITCODE -eq 0) {
        Write-Host "Restic is installed: $($resticVersion)"
    } else {
        Write-Host "WARNING: Restic is not installed or not in PATH"
        Write-Host "Please install restic from: https://restic.net/"
    }
} catch {
    Write-Host "WARNING: Restic is not installed or not in PATH"
    Write-Host "Please install restic from: https://restic.net/"
}

Write-Host ""
Write-Host "Setup completed successfully!"
Write-Host "- Backup script: $BackupScriptPath"
Write-Host "- 1Password reference: $OpRef"
Write-Host "- Log directory: $LogPath"
Write-Host "- Backup location: $BackupPath"
Write-Host "- Scheduled task: $TaskName (runs every 30 minutes)"
Write-Host ""
Write-Host "Next steps:"
Write-Host "1. Install restic if not already installed"
Write-Host "2. 1Password をロック解除しておく (ロック中はバックアップが失敗します)"
Write-Host "3. Run a test backup manually: $BackupScriptPath"
