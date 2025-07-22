# Set UTF-8 encoding for Git
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8

# Create temporary log file
$logFile = New-TemporaryFile
$logPath = $logFile.FullName
"GHQ Repository Check - $(Get-Date)" | Out-File -FilePath $logPath
"================================" | Out-File -FilePath $logPath -Append

# Save current location
$originalLocation = Get-Location

# Main loop
foreach ($repo in (ghq list)) {
    $repoPath = "$(ghq root)/$($repo -replace ':', '/')"
    Set-Location $repoPath

    $branch = git branch --show-current 2>$null
    $changes = git status --porcelain 2>$null
    $unpushed = git log --branches --not --remotes --oneline --decorate-refs=refs/heads --decorate=short 2>$null

    if ($changes -or $unpushed) {
        Write-Host "`n$repo [$branch]" -ForegroundColor Yellow -NoNewline
        Write-Host " [$branch]" -ForegroundColor Cyan
        "`n$repo [$branch]" | Out-File -FilePath $logPath -Append

        if ($changes) {
            Write-Host "  📝 Changes:" -ForegroundColor White
            $changes | ForEach-Object { Write-Host "    $_" }
            "  Changes:" | Out-File -FilePath $logPath -Append
            $changes | ForEach-Object { "    $_" } | Out-File -FilePath $logPath -Append
        }

        if ($unpushed) {
            Write-Host "  ⬆️  Unpushed commits:" -ForegroundColor White
            $unpushed | ForEach-Object { Write-Host "    $_" }
            "  Unpushed commits:" | Out-File -FilePath $logPath -Append
            $unpushed | ForEach-Object { "    $_" } | Out-File -FilePath $logPath -Append
        }

        Write-Host "`n  Actions: [c]ommit, [p]ush, [b]oth, [s]kip, [q]uit?" -ForegroundColor White
        $action = Read-Host "  >"

        switch ($action) {
            'c' {
                if ($changes) {
                    $msg = Read-Host "  Commit message"
                    git add -A
                    git commit -m $msg
                    $result = "  ✅ Committed: $msg"
                    Write-Host $result -ForegroundColor Green
                    $result | Out-File -FilePath $logPath -Append
                }
                else {
                    $result = "  ⚠️  No changes to commit"
                    Write-Host $result -ForegroundColor Yellow
                    $result | Out-File -FilePath $logPath -Append
                }
            }
            'p' {
                if ($unpushed) {
                    # Check if remote is set
                    $remoteCheck = git remote -v 2>$null
                    if (-not ($remoteCheck -match "origin")) {
                        $result = "  ⚠️  No remote repository set"
                        Write-Host $result -ForegroundColor Yellow
                        $result | Out-File -FilePath $logPath -Append

                        $createRepo = Read-Host "  Create GitHub repository? [y/N]"
                        if ($createRepo -match '^[Yy]$') {
                            # Extract repo name from current directory
                            $parentDir = Split-Path -Parent $PWD.Path | Split-Path -Leaf
                            $currentDir = Split-Path -Leaf $PWD.Path
                            $repoName = "$parentDir/$currentDir"

                            gh repo create $repoName --source=. --private
                            $result = "  ✅ Created GitHub repository: $repoName"
                            Write-Host $result -ForegroundColor Green
                            $result | Out-File -FilePath $logPath -Append
                        }
                        else {
                            $result = "  ⏭️  Skipped repository creation"
                            Write-Host $result -ForegroundColor Gray
                            $result | Out-File -FilePath $logPath -Append
                            continue
                        }
                    }
                    git push
                    $result = "  ✅ Pushed to remote"
                    Write-Host $result -ForegroundColor Green
                    $result | Out-File -FilePath $logPath -Append
                }
                else {
                    $result = "  ⚠️  Nothing to push"
                    Write-Host $result -ForegroundColor Yellow
                    $result | Out-File -FilePath $logPath -Append
                }
            }
            'b' {
                if ($changes) {
                    $msg = Read-Host "  Commit message"
                    git add -A
                    git commit -m $msg
                    $result = "  ✅ Committed: $msg"
                    Write-Host $result -ForegroundColor Green
                    $result | Out-File -FilePath $logPath -Append
                }

                # Check if remote is set
                $remoteCheck = git remote -v 2>$null
                if (-not ($remoteCheck -match "origin")) {
                    $result = "  ⚠️  No remote repository set"
                    Write-Host $result -ForegroundColor Yellow
                    $result | Out-File -FilePath $logPath -Append

                    $createRepo = Read-Host "  Create GitHub repository? [y/N]"
                    if ($createRepo -match '^[Yy]$') {
                        # Extract repo name from current directory
                        $parentDir = Split-Path -Parent $PWD.Path | Split-Path -Leaf
                        $currentDir = Split-Path -Leaf $PWD.Path
                        $repoName = "$parentDir/$currentDir"

                        gh repo create $repoName --source=. --private
                        $result = "  ✅ Created GitHub repository: $repoName"
                        Write-Host $result -ForegroundColor Green
                        $result | Out-File -FilePath $logPath -Append
                    }
                    else {
                        $result = "  ⏭️  Skipped repository creation"
                        Write-Host $result -ForegroundColor Gray
                        $result | Out-File -FilePath $logPath -Append
                        continue
                    }
                }

                git push
                $result = "  ✅ Pushed to remote"
                Write-Host $result -ForegroundColor Green
                $result | Out-File -FilePath $logPath -Append
            }
            's' {
                $result = "  ⏭️  Skipped"
                Write-Host $result -ForegroundColor Gray
                $result | Out-File -FilePath $logPath -Append
            }
            'q' {
                $result = "`n🛑 Quit by user"
                Write-Host $result -ForegroundColor Red
                $result | Out-File -FilePath $logPath -Append
                Write-Host "`n📄 Log saved to: $logPath" -ForegroundColor Cyan
                Set-Location $originalLocation
                exit 0
            }
            default {
                $result = "  ❓ Invalid option, skipping"
                Write-Host $result -ForegroundColor Red
                $result | Out-File -FilePath $logPath -Append
            }
        }
    }
}

# Return to original location
Set-Location $originalLocation

Write-Host "`n✨ All repositories checked!" -ForegroundColor Green
"✨ All repositories checked!" | Out-File -FilePath $logPath -Append
Write-Host "📄 Log saved to: $logPath" -ForegroundColor Cyan
