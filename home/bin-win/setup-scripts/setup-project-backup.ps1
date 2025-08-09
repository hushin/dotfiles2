# setup-project-backup.ps1
# Setup automatic backup for project directories using restic

# Configuration
$BackupDrive = "E:"
$ResticPassword = "password"
$SourcePath = "$env:USERPROFILE\projects"
$BackupPath = "$BackupDrive\restic-backup\projects"
$ConfigPath = "$env:USERPROFILE\.config\restic"
$LogPath = "$env:USERPROFILE\.logs\restic"
$BackupScriptPath = "$ConfigPath\quick-project-backup.ps1"

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

# Create password file
$PasswordFile = "$ConfigPath\password.txt"
$ResticPassword | Out-File -FilePath $PasswordFile -Encoding UTF8
Write-Host "Password file created at: $PasswordFile"

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
$Action = New-ScheduledTaskAction -Execute "PowerShell.exe" -Argument "-File `"$BackupScriptPath`""
$Trigger = New-ScheduledTaskTrigger -Once -At (Get-Date) -RepetitionInterval (New-TimeSpan -Hours 1)
$Settings = New-ScheduledTaskSettingsSet -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries -StartWhenAvailable
$Principal = New-ScheduledTaskPrincipal -UserId $env:USERNAME -LogonType Interactive

Register-ScheduledTask -TaskName $TaskName -Action $Action -Trigger $Trigger -Settings $Settings -Principal $Principal -Description $TaskDescription
Write-Host "Scheduled task '$TaskName' created successfully"

# Test restic installation
Write-Host "Testing restic installation..."
try {
    $resticVersion = & restic version 2>&1
    if ($LASTEXITCODE -eq 0) {
        Write-Host "Restic is installed: $($resticVersion[0])"
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
Write-Host "- Password file: $PasswordFile"
Write-Host "- Log directory: $LogPath"
Write-Host "- Backup location: $BackupPath"
Write-Host "- Scheduled task: $TaskName (runs every hour)"
Write-Host ""
Write-Host "Next steps:"
Write-Host "1. Install restic if not already installed"
Write-Host "2. Update the password in: $PasswordFile"
Write-Host "3. Run a test backup manually: $BackupScriptPath"
