# Project Backup Script - Automated execution
param(
    [string]$Tag = ""
)

# Configuration
$BackupDrive = "E:"
$SourcePath = "$env:USERPROFILE\projects"
$BackupPath = "$BackupDrive\restic-backup\projects"
$ConfigPath = "$env:USERPROFILE\.config\restic"
$LogPath = "$env:USERPROFILE\.logs\restic"
$PasswordFile = "$ConfigPath\password.txt"

# Log file
$LogFile = "$LogPath\backup-$(Get-Date -Format 'yyyy-MM-dd').log"

function Write-Log {
    param([string]$Message)
    $Timestamp = Get-Date -Format 'yyyy-MM-dd HH:mm:ss'
    $LogEntry = "[$Timestamp] $Message"
    Write-Host $LogEntry
    $LogEntry | Out-File -FilePath $LogFile -Append -Encoding UTF8
}

Write-Log "=== Project Backup Started ==="

# Check if source path exists
if (-not (Test-Path $SourcePath)) {
    Write-Log "ERROR: Source path does not exist: $SourcePath"
    exit 1
}

# Get all project directories
$ProjectDirs = Get-ChildItem -Path $SourcePath -Directory | Where-Object { $_.Name -notmatch '^\.' }

if ($ProjectDirs.Count -eq 0) {
    Write-Log "WARNING: No project directories found in $SourcePath"
    exit 0
}

Write-Log "Found $($ProjectDirs.Count) project directories"

foreach ($ProjectDir in $ProjectDirs) {
    $ProjectName = $ProjectDir.Name
    $ProjectSourcePath = $ProjectDir.FullName
    $ProjectBackupPath = "$BackupPath\$ProjectName"

    Write-Log "Processing project: $ProjectName"

    # Set environment variables for restic
    $env:RESTIC_PASSWORD_FILE = $PasswordFile
    $env:RESTIC_REPOSITORY = $ProjectBackupPath

    # Initialize repository if it doesn't exist
    if (-not (Test-Path "$ProjectBackupPath\config")) {
        Write-Log "Initializing repository for $ProjectName"
        $initResult = & restic init 2>&1
        if ($LASTEXITCODE -ne 0) {
            Write-Log "ERROR: Failed to initialize repository for ${ProjectName}: $initResult"
            continue
        }
        Write-Log "Repository initialized for $ProjectName"
    }

    # Create backup
    $BackupArgs = @("backup", $ProjectSourcePath, "--verbose")
    if ($Tag) {
        $BackupArgs += "--tag", $Tag
    }

    Write-Log "Creating backup for $ProjectName..."
    $backupResult = & restic @BackupArgs 2>&1
    if ($LASTEXITCODE -eq 0) {
        Write-Log "Backup completed for $ProjectName"
    } else {
        Write-Log "ERROR: Backup failed for ${ProjectName}: $backupResult"
        continue
    }

    # Cleanup old backups
    Write-Log "Cleaning up old backups for $ProjectName..."
    $forgetResult = & restic forget --keep-daily 15 --keep-weekly 10 --prune 2>&1
    if ($LASTEXITCODE -eq 0) {
        Write-Log "Cleanup completed for $ProjectName"
    } else {
        Write-Log "WARNING: Cleanup failed for ${ProjectName}: $forgetResult"
    }
}

Write-Log "=== Project Backup Completed ==="
