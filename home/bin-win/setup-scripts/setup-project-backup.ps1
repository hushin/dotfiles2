# Project Backup Setup with restic
# This script sets up automated backup for $env:USERPROFILE/projects/ directory

$ErrorActionPreference = "Stop"

# Configuration
$BackupDrive = "E:"
$ResticPassword = "password"
$SourcePath = "$env:USERPROFILE\projects"
$BackupPath = "$BackupDrive\restic-backup\projects"
$ConfigPath = "$env:USERPROFILE\.config\restic"
$LogPath = "$env:USERPROFILE\.logs\restic"

Write-Host "=== Project Backup Setup ===" -ForegroundColor Green

# Check if restic is installed
if (-not (Get-Command restic -ErrorAction SilentlyContinue)) {
    # ref. https://github.com/restic/restic/issues/5108
    Write-Host "restic が見つかりません。管理者権限で PowerShell を開き、以下のコマンドを実行してください:" -ForegroundColor Yellow
    Write-Host "winget install restic.restic" -ForegroundColor Cyan
    exit 1
}

# Check if backup drive exists
$BackupDriveRoot = Split-Path $BackupPath -Qualifier
if (-not (Test-Path $BackupDriveRoot)) {
    Write-Error "Backup drive '$BackupDriveRoot' is not accessible. Please check the drive exists and is mounted."
}

# Create necessary directories
@($ConfigPath, $LogPath) | ForEach-Object {
    if (-not (Test-Path $_)) {
        try {
            New-Item -ItemType Directory -Path $_ -Force | Out-Null
            Write-Host "Created directory: $_" -ForegroundColor Blue
        } catch {
            Write-Error "Failed to create directory '$_': $_"
        }
    }
}

# Create backup directory on separate drive
if (-not (Test-Path $BackupPath)) {
    try {
        New-Item -ItemType Directory -Path $BackupPath -Force | Out-Null
        Write-Host "Created backup directory: $BackupPath" -ForegroundColor Blue
    } catch {
        Write-Error "Failed to create backup directory '$BackupPath': $_"
    }
}

# Set up environment file
$EnvFile = "$ConfigPath\projects-backup.env"
if (-not (Test-Path $EnvFile)) {

    @"
RESTIC_REPOSITORY=$BackupPath
RESTIC_PASSWORD=$ResticPassword
"@ | Out-File -FilePath $EnvFile -Encoding UTF8

    Write-Host "Created environment file: $EnvFile" -ForegroundColor Blue
}

# Initialize repository if it doesn't exist
if (-not (Test-Path "$BackupPath\config")) {
    Write-Host "Initializing restic repository..." -ForegroundColor Yellow

    $env:RESTIC_REPOSITORY = $BackupPath
    $env:RESTIC_PASSWORD = (Get-Content $EnvFile | Where-Object { $_ -match "^RESTIC_PASSWORD=" }) -replace "^RESTIC_PASSWORD=", ""

    restic init
    if ($LASTEXITCODE -eq 0) {
        Write-Host "Repository initialized successfully" -ForegroundColor Green
    } else {
        Write-Error "Failed to initialize repository"
    }
}

Write-Host "Setting up scheduled task..." -ForegroundColor Yellow

# Create backup script
$BackupScript = "$ConfigPath\run-projects-backup.ps1"
@"
# Load environment
Get-Content "$ConfigPath\projects-backup.env" | ForEach-Object {
    if (`$_ -match "^([^=]+)=(.*)$") {
        [Environment]::SetEnvironmentVariable(`$matches[1], `$matches[2])
    }
}

# Run backup
`$LogFile = "$LogPath\backup-`$(Get-Date -Format 'yyyy-MM-dd').log"
`$ErrorLogFile = "$LogPath\backup-error-`$(Get-Date -Format 'yyyy-MM-dd').log"

Write-Output "`$(Get-Date): Starting backup..." | Tee-Object -FilePath `$LogFile -Append

try {
    # Backup with progress
    restic backup "$SourcePath" --verbose 2>&1 | Tee-Object -FilePath `$LogFile -Append

    if (`$LASTEXITCODE -eq 0) {
        Write-Output "`$(Get-Date): Backup completed successfully" | Tee-Object -FilePath `$LogFile -Append
    } else {
        Write-Output "`$(Get-Date): Backup failed with exit code `$LASTEXITCODE" | Tee-Object -FilePath `$ErrorLogFile -Append
    }

    # Cleanup old snapshots (keep 30 daily, 12 weekly, 12 monthly)
    Write-Output "`$(Get-Date): Cleaning up old snapshots..." | Tee-Object -FilePath `$LogFile -Append
    restic forget --keep-daily 30 --keep-weekly 12 --keep-monthly 12 --prune 2>&1 | Tee-Object -FilePath `$LogFile -Append

} catch {
    Write-Output "`$(Get-Date): Error: `$_" | Tee-Object -FilePath `$ErrorLogFile -Append
}
"@ | Out-File -FilePath $BackupScript -Encoding UTF8

# Register scheduled task (daily at 9 PM)
$TaskName = "ProjectBackup"
$Action = New-ScheduledTaskAction -Execute "PowerShell.exe" -Argument "-ExecutionPolicy Bypass -File `"$BackupScript`""
$Trigger = New-ScheduledTaskTrigger -Daily -At "9:00PM"
$Settings = New-ScheduledTaskSettingsSet -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries

try {
    Unregister-ScheduledTask -TaskName $TaskName -Confirm:$false -ErrorAction SilentlyContinue
    Register-ScheduledTask -TaskName $TaskName -Action $Action -Trigger $Trigger -Settings $Settings -Description "Daily backup of projects directory using restic"
    Write-Host "Scheduled task created: $TaskName (runs daily at 9:00 PM)" -ForegroundColor Green
} catch {
    Write-Warning "Failed to create scheduled task: $_"
    Write-Host "You can run the backup manually: $BackupScript" -ForegroundColor Yellow
}

Write-Host "`n=== Setup Complete ===" -ForegroundColor Green
Write-Host "Configuration files:"
Write-Host "  - Environment: $EnvFile" -ForegroundColor Cyan
Write-Host "  - Backup script: $ConfigPath\run-projects-backup.ps1" -ForegroundColor Cyan
Write-Host "  - Logs: $LogPath" -ForegroundColor Cyan
Write-Host "`nRepository: $BackupPath" -ForegroundColor Cyan
Write-Host "`nManual commands:"
Write-Host "  - Run backup: & '$ConfigPath\run-projects-backup.ps1'" -ForegroundColor Yellow
Write-Host "  - List snapshots: restic -r '$BackupPath' snapshots" -ForegroundColor Yellow
Write-Host "  - Restore: restic -r '$BackupPath' restore latest --target C:\restore-path" -ForegroundColor Yellow
