# manage-project-backup.ps1
# Interactive management tool for project backups using restic

# Configuration
$BackupDrive = "E:"
$SourcePath = "$env:USERPROFILE\projects"
$BackupPath = "$BackupDrive\restic-backup\projects"
$ConfigPath = "$env:USERPROFILE\.config\restic"

$PasswordFile = "$ConfigPath\password.txt"

# Check if fzf is available
function Test-FzfAvailable {
    try {
        $null = & fzf --version 2>$null
        return $true
    } catch {
        return $false
    }
}

function Show-Menu {
    $options = @(
        "backup - Create manual backup"
        "backup-with-tag - Create manual backup with tag"
        "list-projects - List available project repositories"
        "snapshots - View snapshots for a project"
        "check - Check repository integrity"
        "restore - Restore files from backup"
        "mount - Mount repository as filesystem"
        "prune - Cleanup and prune repository"
        "stats - Show repository statistics"
        "unlock - Unlock repository (if locked)"
        "exit - Exit"
    )

    if (Test-FzfAvailable) {
        $selected = $options | fzf --prompt="Select action: " --height=15
        return $selected.Split(' - ')[0]
    } else {
        Write-Host "Available actions:"
        for ($i = 0; $i -lt $options.Count; $i++) {
            Write-Host "$($i + 1). $($options[$i])"
        }
        $choice = Read-Host "Select action (1-$($options.Count))"
        if ($choice -match '^\d+$' -and [int]$choice -ge 1 -and [int]$choice -le $options.Count) {
            return $options[[int]$choice - 1].Split(' - ')[0]
        } else {
            return "invalid"
        }
    }
}

function Select-Project {
    param([string]$Action = "Select project")

    if (-not (Test-Path $BackupPath)) {
        Write-Host "Backup path does not exist: $BackupPath"
        return $null
    }

    $projects = Get-ChildItem -Path $BackupPath -Directory | Where-Object { Test-Path "$($_.FullName)\config" }

    if ($projects.Count -eq 0) {
        Write-Host "No backup repositories found in: $BackupPath"
        return $null
    }

    if (Test-FzfAvailable) {
        $selected = $projects.Name | fzf --prompt="$Action`: "
        return $selected
    } else {
        Write-Host "Available projects:"
        for ($i = 0; $i -lt $projects.Count; $i++) {
            Write-Host "$($i + 1). $($projects[$i].Name)"
        }
        $choice = Read-Host "$Action (1-$($projects.Count))"
        if ($choice -match '^\d+$' -and [int]$choice -ge 1 -and [int]$choice -le $projects.Count) {
            return $projects[[int]$choice - 1].Name
        } else {
            return $null
        }
    }
}

function Set-ResticEnv {
    param([string]$ProjectName)
    $env:RESTIC_PASSWORD_FILE = $PasswordFile
    $env:RESTIC_REPOSITORY = "$BackupPath\$ProjectName"
}

function Invoke-ManualBackup {
    param([string]$Tag = "")

    $backupScriptPath = "$ConfigPath\quick-project-backup.ps1"
    if (-not (Test-Path $backupScriptPath)) {
        Write-Host "Backup script not found: $backupScriptPath"
        Write-Host "Please run setup-project-backup.ps1 first"
        return
    }

    if ($Tag) {
        & $backupScriptPath -Tag $Tag
    } else {
        & $backupScriptPath
    }
}

function Show-Snapshots {
    $project = Select-Project "Select project to view snapshots"
    if (-not $project) { return }

    Set-ResticEnv $project
    Write-Host "Snapshots for project: $project"
    Write-Host "Repository: $env:RESTIC_REPOSITORY"
    Write-Host ""

    & restic snapshots
}

function Invoke-Check {
    $project = Select-Project "Select project to check"
    if (-not $project) { return }

    Set-ResticEnv $project
    Write-Host "Checking repository integrity for: $project"
    Write-Host "Repository: $env:RESTIC_REPOSITORY"
    Write-Host ""

    & restic check --read-data
}

function Invoke-Restore {
    $project = Select-Project "Select project to restore from"
    if (-not $project) { return }

    Set-ResticEnv $project
    Write-Host "Available snapshots for: $project"

    # Get snapshots in JSON format for parsing
    try {
        $snapshotsJson = & restic snapshots --json 2>$null
        if ($LASTEXITCODE -ne 0) {
            Write-Host "Failed to get snapshots"
            return
        }

        $snapshots = @($snapshotsJson | ConvertFrom-Json | Sort-Object time -Descending)
        if (-not $snapshots -or $snapshots.Count -eq 0) {
            Write-Host "No snapshots found"
            return
        }
    } catch {
        Write-Host "Failed to parse snapshots: $($_.Exception.Message)"
        return
    }

    # Display snapshots for reference
    & restic snapshots --compact
    Write-Host ""

    # Prepare snapshot options for fzf
    $snapshotOptions = @()
    foreach ($snapshot in $snapshots) {
        $timeFormatted = ([DateTime]::Parse($snapshot.time)).ToString("yyyy/MM/dd HH:mm:ss")
        $shortId = $snapshot.short_id
        $tags = if ($snapshot.tags) { $snapshot.tags -join "," } else { "" }
        $hostname = $snapshot.hostname
        $tagsDisplay = if ($tags) { " [$tags]" } else { "" }
        
        # Format size from total_bytes_processed
        $sizeDisplay = ""
        if ($snapshot.summary -and $snapshot.summary.total_bytes_processed) {
            $bytes = $snapshot.summary.total_bytes_processed
            if ($bytes -ge 1GB) {
                $sizeDisplay = " ({0:N2} GB)" -f ($bytes / 1GB)
            } elseif ($bytes -ge 1MB) {
                $sizeDisplay = " ({0:N2} MB)" -f ($bytes / 1MB)
            } elseif ($bytes -ge 1KB) {
                $sizeDisplay = " ({0:N2} KB)" -f ($bytes / 1KB)
            } else {
                $sizeDisplay = " ($bytes B)"
            }
        }

        $snapshotOptions += "$shortId - $timeFormatted $hostname$tagsDisplay$sizeDisplay"
    }

    # Select snapshot
    $selectedSnapshot = $null
    if (Test-FzfAvailable) {
        $selected = $snapshotOptions | fzf --prompt="Select snapshot: " --height=15
        if ($selected) {
            $selectedSnapshot = $selected.Split(' - ')[0]
        }
    } else {
        Write-Host "Available snapshots:"
        for ($i = 0; $i -lt $snapshotOptions.Count; $i++) {
            Write-Host "$($i + 1). $($snapshotOptions[$i])"
        }
        $choice = Read-Host "Select snapshot (1-$($snapshotOptions.Count))"
        if ($choice -match '^\d+$' -and [int]$choice -ge 1 -and [int]$choice -le $snapshotOptions.Count) {
            $selectedSnapshot = $snapshotOptions[[int]$choice - 1].Split(' - ')[0]
        }
    }

    if (-not $selectedSnapshot) {
        Write-Host "No snapshot selected"
        return
    }

    # Get restore destination path with default
    $defaultRestorePath = "$env:TEMP\restic-restore-$project-$(Get-Date -Format 'yyyyMMdd-HHmmss')"
    $restorePathInput = Read-Host "Enter restore destination path (default: $defaultRestorePath)"
    $restorePath = if ($restorePathInput) { $restorePathInput } else { $defaultRestorePath }

    # Create restore directory if it doesn't exist
    if (-not (Test-Path $restorePath)) {
        New-Item -ItemType Directory -Path $restorePath -Force | Out-Null
    }

    # Build snapshot reference with path (snapshot_id:path format)
    $snapshotRef = "${selectedSnapshot}"

    Write-Host ""
    Write-Host "Restoring snapshot reference: $snapshotRef"
    Write-Host "Destination: $restorePath"

    $restoreResult = & restic restore $snapshotRef --target $restorePath 2>&1
    if ($LASTEXITCODE -eq 0) {
        Write-Host "Restore completed successfully!"
        Write-Host "Opening restore directory..."
        Start-Process -FilePath $restorePath
    } else {
        Write-Host "Restore failed: $restoreResult"
    }
}

function Invoke-Mount {
    $project = Select-Project "Select project to mount"
    if (-not $project) { return }

    Set-ResticEnv $project
    $mountPath = Read-Host "Enter mount point path"
    if (-not $mountPath) {
        Write-Host "No mount path provided"
        return
    }

    if (-not (Test-Path $mountPath)) {
        New-Item -ItemType Directory -Path $mountPath -Force | Out-Null
    }

    Write-Host "Mounting repository to: $mountPath"
    Write-Host "Press Ctrl+C to unmount"
    & restic mount $mountPath
}

function Invoke-Prune {
    $project = Select-Project "Select project to prune"
    if (-not $project) { return }

    Set-ResticEnv $project
    Write-Host "Pruning repository for: $project"
    Write-Host "This will remove unreferenced data and may take some time..."
    $confirm = Read-Host "Continue? (y/N)"

    if ($confirm -eq 'y' -or $confirm -eq 'Y') {
        & restic forget --keep-daily 15 --keep-weekly 10 --prune
    } else {
        Write-Host "Prune cancelled"
    }
}

function Show-Stats {
    $project = Select-Project "Select project to show stats"
    if (-not $project) { return }

    Set-ResticEnv $project
    Write-Host "Statistics for project: $project"
    Write-Host "Repository: $env:RESTIC_REPOSITORY"
    Write-Host ""

    & restic stats
}

function Invoke-Unlock {
    $project = Select-Project "Select project to unlock"
    if (-not $project) { return }

    Set-ResticEnv $project
    Write-Host "Unlocking repository for: $project"
    & restic unlock
}

function Show-ProjectList {
    if (-not (Test-Path $BackupPath)) {
        Write-Host "Backup path does not exist: $BackupPath"
        return
    }

    $projects = Get-ChildItem -Path $BackupPath -Directory | Where-Object { Test-Path "$($_.FullName)\config" }

    if ($projects.Count -eq 0) {
        Write-Host "No backup repositories found in: $BackupPath"
        return
    }

    Write-Host "Available project repositories:"
    Write-Host ""

    foreach ($project in $projects) {
        $projectName = $project.Name
        Set-ResticEnv $projectName

        Write-Host "Project: $projectName"
        Write-Host "  Repository: $($project.FullName)"

        try {
            $snapshots = & restic snapshots --json 2>$null | ConvertFrom-Json
            if ($snapshots -and $snapshots.Count -gt 0) {
                $latest = $snapshots | Sort-Object time -Descending | Select-Object -First 1
                Write-Host "  Latest backup: $($latest.time) ($(($snapshots | Measure-Object).Count) snapshots)"
            } else {
                Write-Host "  Latest backup: No snapshots found"
            }
        } catch {
            Write-Host "  Latest backup: Unable to read snapshots"
        }

        Write-Host ""
    }
}

# Main execution
Write-Host "Project Backup Management Tool"
Write-Host "================================"
Write-Host ""

# Check if password file exists
if (-not (Test-Path $PasswordFile)) {
    Write-Host "Password file not found: $PasswordFile"
    Write-Host "Please run setup-project-backup.ps1 first"
    exit 1
}

# Check if restic is installed
try {
    $null = & restic version 2>$null
} catch {
    Write-Host "Restic is not installed or not in PATH"
    Write-Host "Please install restic from: https://restic.net/"
    exit 1
}

# Main loop
while ($true) {
    $action = Show-Menu

    switch ($action) {
        "backup" { Invoke-ManualBackup }
        "backup-with-tag" {
            $tag = Read-Host "Enter tag for backup"
            Invoke-ManualBackup -Tag $tag
        }
        "list-projects" { Show-ProjectList }
        "snapshots" { Show-Snapshots }
        "check" { Invoke-Check }
        "restore" { Invoke-Restore }
        "mount" { Invoke-Mount }
        "prune" { Invoke-Prune }
        "stats" { Show-Stats }
        "unlock" { Invoke-Unlock }
        "exit" { exit 0 }
        "invalid" { Write-Host "Invalid selection" }
        default {
            if (-not $action) { break }
            Write-Host "Unknown action: $action"
        }
    }

    if ($action -ne "exit" -and $action) {
        Write-Host ""
        Read-Host "Press Enter to continue..."
        Write-Host ""
    }
}
