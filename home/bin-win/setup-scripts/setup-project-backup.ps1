# setup-project-backup.ps1
# Setup automatic backup for project directories using restic
# The restic password is stored in 1Password and fetched via the 1Password CLI.

param(
    # 未指定なら 1Password の vault を自動選択する（1 つだけなら自動、複数なら選択）
    [string]$OpVault = $env:RESTIC_OP_VAULT,
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

# vault 名を解決する。1Password の既定以外の vault 名 (Personal など) でも
# 動くように、存在しない vault を指定していた場合は分かりやすく案内する。
# vault 一覧の取得がサインイン確認も兼ねる（op の呼び出し回数を減らす）。
try {
    $vaultNames = @(Get-OnePasswordVaultNames)
}
catch {
    Write-Error $_.Exception.Message
    exit 1
}

if ($vaultNames.Count -eq 0) {
    Write-Error "1Password に vault が見つかりません。1Password アプリで vault を作成してください。"
    exit 1
}

if ([string]::IsNullOrWhiteSpace($OpVault)) {
    if ($vaultNames.Count -eq 1) {
        $OpVault = $vaultNames[0]
    }
    else {
        Write-Host "1Password の vault を選択してください:"
        for ($i = 0; $i -lt $vaultNames.Count; $i++) {
            Write-Host "  $($i + 1). $($vaultNames[$i])"
        }
        $selection = Read-Host "番号 (1-$($vaultNames.Count))"
        if ($selection -notmatch '^\d+$' -or [int]$selection -lt 1 -or [int]$selection -gt $vaultNames.Count) {
            Write-Error "vault が選択されませんでした。利用可能な vault: $($vaultNames -join ', ')"
            exit 1
        }
        $OpVault = $vaultNames[[int]$selection - 1]
    }
}
elseif ($vaultNames -notcontains $OpVault) {
    Write-Error "vault '$OpVault' が見つかりません。利用可能な vault: $($vaultNames -join ', ')"
    exit 1
}

$OpRef = "op://$OpVault/$OpItem/password"
Write-Host "Using 1Password item: $OpRef"

try {
    $itemExists = Test-ResticOnePasswordItem -Vault $OpVault -Item $OpItem
}
catch {
    Write-Error $_.Exception.Message
    exit 1
}

if (-not $itemExists) {
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
    $password = Get-ResticPasswordFromOnePassword -Reference $OpRef
}
catch {
    Write-Error $_.Exception.Message
    exit 1
}

# バックアップ時に 1Password の認証ダイアログが出ないよう、DPAPI で暗号化してキャッシュする
try {
    $cacheFile = Set-ResticPasswordCache -Password $password
    Write-Host "Password cached (DPAPI): $cacheFile"
}
catch {
    Write-Error "パスワードキャッシュを作成できませんでした: $($_.Exception.Message)"
    exit 1
}

# 1Password の vault / item 名に日本語が含まれても壊れないよう UTF-8 (BOM なし) で保存する
if (Test-Path -LiteralPath $OpRefFile) {
    $existingRef = [System.IO.File]::ReadAllText($OpRefFile, [System.Text.UTF8Encoding]::new($false)).Trim()
    if ($existingRef -and $existingRef -ne $OpRef) {
        Write-Host ""
        Write-Host "WARNING: 1Password reference を変更します: $existingRef -> $OpRef" -ForegroundColor Yellow
        Write-Host "既存の restic リポジトリは旧 reference のパスワードで作成されています。" -ForegroundColor Yellow
        Write-Host "パスワードが異なる場合、バックアップは失敗します。" -ForegroundColor Yellow
        Write-Host ""
    }
}
[System.IO.File]::WriteAllText($OpRefFile, $OpRef, [System.Text.UTF8Encoding]::new($false))
Write-Host "1Password reference saved at: $OpRefFile"

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
Write-Host "- Password cache (DPAPI): $cacheFile"
Write-Host "- Log directory: $LogPath"
Write-Host "- Backup location: $BackupPath"
Write-Host "- Scheduled task: $TaskName (runs every 30 minutes)"
Write-Host ""
Write-Host "Next steps:"
Write-Host "1. Install restic if not already installed"
Write-Host "2. Run a test backup manually: $BackupScriptPath"
Write-Host ""
Write-Host "Note: バックアップは DPAPI キャッシュを使うため、1Password の認証ダイアログは出ません。"
Write-Host "      1Password 側でパスワードを変更したら sync-restic-password.ps1 を実行してください。"
