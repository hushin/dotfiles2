# Project Backup Script - Automated execution
param(
    [string]$Tag = ""
)

# restic は --verbose 時に stderr へ出力する。stderr をリダイレクトして
# 取り込むため、呼び出し元の $ErrorActionPreference に左右されないようにする。
$ErrorActionPreference = "Continue"

# Configuration
$BackupDrive = "E:"
$SourcePath = "$env:USERPROFILE\projects"
$BackupPath = "$BackupDrive\restic-backup\projects"
$ConfigPath = "$env:USERPROFILE\.config\restic"
$LogPath = "$env:USERPROFILE\.logs\restic"
$HelperPath = "$ConfigPath\restic-1password.ps1"

# Log file
$LogFile = "$LogPath\backup-$(Get-Date -Format 'yyyy-MM-dd').log"

function Write-Log {
    param([string]$Message)
    if (-not (Test-Path -LiteralPath $LogPath)) {
        New-Item -ItemType Directory -Path $LogPath -Force | Out-Null
    }
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

# Load the restic password from 1Password
if (-not (Test-Path -LiteralPath $HelperPath)) {
    Write-Log "ERROR: 1Password helper not found: $HelperPath (run 'chezmoi apply' first)"
    exit 1
}
. $HelperPath

# manage-project-backup.ps1 から呼ばれた場合に備え、呼び出し元の値を保存しておく
$previousResticPassword = $env:RESTIC_PASSWORD
$previousResticPasswordFile = $env:RESTIC_PASSWORD_FILE
$previousResticPasswordCommand = $env:RESTIC_PASSWORD_COMMAND

try {
    $passwordSource = Set-ResticPasswordEnvironment
}
catch {
    Write-Log "ERROR: $($_.Exception.Message)"
    exit 1
}

if ($passwordSource -eq "legacy") {
    Write-Log "WARNING: 1Password 未設定のため旧 password.txt を使用します。setup-project-backup.ps1 と migrate-restic-password.ps1 を実行してください。"
}

Write-Log "Found $($ProjectDirs.Count) project directories"

foreach ($ProjectDir in $ProjectDirs) {
    $ProjectName = $ProjectDir.Name
    $ProjectSourcePath = $ProjectDir.FullName
    $ProjectBackupPath = "$BackupPath\$ProjectName"

    Write-Log "Processing project: $ProjectName"

    # Set environment variables for restic
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

    # Create backup with relative path (change to project directory first)
    $BackupArgs = @("backup", ".", "--verbose")
    if ($Tag) {
        $BackupArgs += "--tag", $Tag
    }

    Write-Log "Creating backup for $ProjectName..."
    # Change to project directory to create backup with relative paths
    Push-Location -Path $ProjectSourcePath
    try {
        $backupResult = & restic @BackupArgs 2>&1
    } finally {
        Pop-Location
    }
    if ($LASTEXITCODE -eq 0) {
        Write-Log "Backup completed for $ProjectName"
    } else {
        Write-Log "ERROR: Backup failed for ${ProjectName}: $backupResult"
        if ($LASTEXITCODE -eq 12) {
            Write-Log "HINT: パスワードが一致しません。旧 password.txt で作成したリポジトリは migrate-restic-password.ps1 で移行してください。"
        }
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

if ($null -ne $previousResticPassword) {
    $env:RESTIC_PASSWORD = $previousResticPassword
} else {
    Remove-Item Env:\RESTIC_PASSWORD -ErrorAction SilentlyContinue
}
if ($null -ne $previousResticPasswordFile) {
    $env:RESTIC_PASSWORD_FILE = $previousResticPasswordFile
} else {
    Remove-Item Env:\RESTIC_PASSWORD_FILE -ErrorAction SilentlyContinue
}
if ($null -ne $previousResticPasswordCommand) {
    $env:RESTIC_PASSWORD_COMMAND = $previousResticPasswordCommand
} else {
    Remove-Item Env:\RESTIC_PASSWORD_COMMAND -ErrorAction SilentlyContinue
}
Write-Log "=== Project Backup Completed ==="
