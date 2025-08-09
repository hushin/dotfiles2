# Project Backup Management Script
# Provides easy commands to manage restic backups

param(
    [string]$Command = "",
    [string]$SnapshotId = "latest",
    [string]$RestorePath = "",
    [switch]$DryRun
)

$ErrorActionPreference = "Stop"

# Configuration
$BackupDrive = "E:"
$SourcePath = "$env:USERPROFILE\projects"
$BackupPath = "$BackupDrive\restic-backup\projects"
$ConfigPath = "$env:USERPROFILE\.config\restic"
$LogPath = "$env:USERPROFILE\.logs\restic"
$EnvFile = "$ConfigPath\projects-backup.env"

# Load environment variables
if (Test-Path $EnvFile) {
    Get-Content $EnvFile | ForEach-Object {
        if ($_ -match "^([^=]+)=(.*)$") {
            [Environment]::SetEnvironmentVariable($matches[1], $matches[2])
        }
    }
} else {
    Write-Error "Environment file not found: $EnvFile. Run setup-project-backup.ps1 first."
}

# Verify repository exists
if (-not (Test-Path "$BackupPath\config")) {
    Write-Error "Repository not found: $BackupPath. Run setup-project-backup.ps1 first."
}

Write-Host "=== Project Backup Management ===" -ForegroundColor Green
Write-Host "Repository: $BackupPath" -ForegroundColor Cyan
Write-Host "Source: $SourcePath" -ForegroundColor Cyan
Write-Host ""

# If no command provided, show fuzzy selection menu
if (-not $Command) {
    $Commands = @(
        @{Name="backup"; Description="Create new backup"},
        @{Name="list"; Description="List all snapshots"},
        @{Name="restore"; Description="Restore files from snapshot"},
        @{Name="check"; Description="Check repository integrity"},
        @{Name="stats"; Description="Show repository statistics"},
        @{Name="cleanup"; Description="Clean up old snapshots"},
        @{Name="mount"; Description="Mount repository as drive (requires WinFsp)"}
    )
    
    Write-Host "Select a command:" -ForegroundColor Yellow
    for ($i = 0; $i -lt $Commands.Count; $i++) {
        Write-Host "  $($i + 1). $($Commands[$i].Name) - $($Commands[$i].Description)" -ForegroundColor Cyan
    }
    
    do {
        $Selection = Read-Host "`nEnter command number or name (1-$($Commands.Count))"
        
        # Try to parse as number
        if ([int]::TryParse($Selection, [ref]$null) -and [int]$Selection -ge 1 -and [int]$Selection -le $Commands.Count) {
            $Command = $Commands[[int]$Selection - 1].Name
            break
        }
        # Try to match by name (fuzzy match)
        else {
            $MatchedCommands = $Commands | Where-Object { $_.Name -like "*$Selection*" }
            if ($MatchedCommands.Count -eq 1) {
                $Command = $MatchedCommands[0].Name
                break
            }
            elseif ($MatchedCommands.Count -gt 1) {
                Write-Host "Multiple matches found:" -ForegroundColor Yellow
                $MatchedCommands | ForEach-Object { Write-Host "  - $($_.Name)" -ForegroundColor Cyan }
            }
            else {
                Write-Host "No matching command found. Please try again." -ForegroundColor Red
            }
        }
    } while ($true)
    
    Write-Host "`nSelected command: $Command" -ForegroundColor Green
}

switch ($Command) {
    "backup" {
        Write-Host "Starting backup..." -ForegroundColor Yellow
        $LogFile = "$LogPath\manual-backup-$(Get-Date -Format 'yyyy-MM-dd-HHmm').log"

        if ($DryRun) {
            Write-Host "DRY RUN MODE - No changes will be made" -ForegroundColor Magenta
            restic backup $SourcePath --dry-run --verbose
        } else {
            restic backup $SourcePath --verbose 2>&1 | Tee-Object -FilePath $LogFile
            if ($LASTEXITCODE -eq 0) {
                Write-Host "Backup completed successfully" -ForegroundColor Green
                Write-Host "Log saved to: $LogFile" -ForegroundColor Blue
            } else {
                Write-Error "Backup failed. Check log: $LogFile"
            }
        }
    }

    "list" {
        Write-Host "Available snapshots:" -ForegroundColor Yellow
        restic snapshots --compact
    }

    "restore" {
        if (-not $RestorePath) {
            $RestorePath = Read-Host "Enter restore destination path"
        }

        if (-not (Test-Path $RestorePath)) {
            New-Item -ItemType Directory -Path $RestorePath -Force
        }

        Write-Host "Restoring snapshot '$SnapshotId' to '$RestorePath'..." -ForegroundColor Yellow

        if ($DryRun) {
            Write-Host "DRY RUN MODE - No files will be restored" -ForegroundColor Magenta
            restic restore $SnapshotId --target $RestorePath --dry-run --verbose
        } else {
            restic restore $SnapshotId --target $RestorePath --verbose
            if ($LASTEXITCODE -eq 0) {
                Write-Host "Restore completed successfully" -ForegroundColor Green
                Write-Host "Files restored to: $RestorePath" -ForegroundColor Blue
            } else {
                Write-Error "Restore failed"
            }
        }
    }

    "check" {
        Write-Host "Checking repository integrity..." -ForegroundColor Yellow
        restic check --verbose

        if ($LASTEXITCODE -eq 0) {
            Write-Host "Repository check passed" -ForegroundColor Green
        } else {
            Write-Error "Repository check failed"
        }
    }

    "stats" {
        Write-Host "Repository statistics:" -ForegroundColor Yellow
        restic stats --mode restore-size
        Write-Host ""
        restic stats --mode files-by-contents
    }

    "cleanup" {
        Write-Host "Cleaning up old snapshots..." -ForegroundColor Yellow
        Write-Host "Retention policy: 30 daily, 12 weekly, 12 monthly" -ForegroundColor Blue

        if ($DryRun) {
            Write-Host "DRY RUN MODE - No snapshots will be deleted" -ForegroundColor Magenta
            restic forget --keep-daily 30 --keep-weekly 12 --keep-monthly 12 --dry-run --verbose
        } else {
            restic forget --keep-daily 30 --keep-weekly 12 --keep-monthly 12 --prune --verbose
            if ($LASTEXITCODE -eq 0) {
                Write-Host "Cleanup completed successfully" -ForegroundColor Green
            } else {
                Write-Error "Cleanup failed"
            }
        }
    }

    "mount" {
        if (-not (Get-Command winfsp-x64 -ErrorAction SilentlyContinue)) {
            Write-Warning "WinFsp is required for mounting. Install from: https://winfsp.dev/"
            return
        }

        $MountPoint = "R:\"
        Write-Host "Mounting repository to $MountPoint..." -ForegroundColor Yellow
        Write-Host "Press Ctrl+C to unmount" -ForegroundColor Blue

        restic mount $MountPoint
    }
}

Write-Host "`n=== Available Commands ===" -ForegroundColor Green
Write-Host "  backup       - Create new backup" -ForegroundColor Cyan
Write-Host "  list         - List all snapshots" -ForegroundColor Cyan
Write-Host "  restore      - Restore files from snapshot" -ForegroundColor Cyan
Write-Host "  check        - Check repository integrity" -ForegroundColor Cyan
Write-Host "  stats        - Show repository statistics" -ForegroundColor Cyan
Write-Host "  cleanup      - Clean up old snapshots" -ForegroundColor Cyan
Write-Host "  mount        - Mount repository as drive (requires WinFsp)" -ForegroundColor Cyan
Write-Host "`nAdd -DryRun to preview changes without making them" -ForegroundColor Yellow
