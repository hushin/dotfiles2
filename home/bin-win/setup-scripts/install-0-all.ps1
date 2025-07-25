$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path

Write-Host "=== install-emacs ===" -ForegroundColor Green
& "$scriptDir\install-emacs.ps1"

Write-Host "=== install-work ===" -ForegroundColor Green
& "$scriptDir\install-work.ps1"

Write-Host "=== install-creative ===" -ForegroundColor Green
& "$scriptDir\install-creative.ps1"

Write-Host "=== install-hobby ===" -ForegroundColor Green
& "$scriptDir\install-hobby.ps1"

