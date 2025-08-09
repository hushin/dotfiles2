# Quick Project Backup Script
# Simple one-command backup execution

param(
    [string]$Comment = ""
)

$ErrorActionPreference = "Stop"

# Configuration
$ConfigPath = "$env:USERPROFILE\.config\restic"
$EnvFile = "$ConfigPath\projects-backup.env"

Write-Host "=== Quick Project Backup ===" -ForegroundColor Green

# Load environment
if (Test-Path $EnvFile) {
    Get-Content $EnvFile | ForEach-Object {
        if ($_ -match "^([^=]+)=(.*)$") {
            [Environment]::SetEnvironmentVariable($matches[1], $matches[2])
        }
    }
} else {
    Write-Error "Backup not configured. Run setup-project-backup.ps1 first."
}

# Check if restic is available
if (-not (Get-Command restic -ErrorAction SilentlyContinue)) {
    Write-Error "restic not found. Run setup-project-backup.ps1 first."
}

$SourcePath = "$env:USERPROFILE\projects"
$StartTime = Get-Date

Write-Host "Starting backup of: $SourcePath" -ForegroundColor Yellow
Write-Host "Time: $($StartTime.ToString('yyyy-MM-dd HH:mm:ss'))" -ForegroundColor Blue

if ($Comment) {
    Write-Host "Comment: $Comment" -ForegroundColor Cyan
}

try {
    if ($Comment) {
        restic backup $SourcePath --verbose --tag "comment:$Comment"
    } else {
        restic backup $SourcePath --verbose
    }

    if ($LASTEXITCODE -eq 0) {
        $EndTime = Get-Date
        $Duration = $EndTime - $StartTime
        Write-Host "`nBackup completed successfully!" -ForegroundColor Green
        Write-Host "Duration: $($Duration.ToString('hh\:mm\:ss'))" -ForegroundColor Blue

        # Show latest snapshot info
        Write-Host "`nLatest snapshot:" -ForegroundColor Cyan
        restic snapshots --last 1
    } else {
        Write-Error "Backup failed with exit code $LASTEXITCODE"
    }
} catch {
    Write-Error "Backup error: $_"
}
