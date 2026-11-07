Write-Host "npm のグローバル bin を PATH に追加します" -ForegroundColor Green

function Add-ToUserPath {
    param(
        [string]$Path
    )

    $currentPath = [Environment]::GetEnvironmentVariable("PATH", "User")
    if ($currentPath -notlike "*$Path*") {
        [Environment]::SetEnvironmentVariable("PATH", "$currentPath;$Path", "User")
        Write-Host "PATHに追加しました: $Path" -ForegroundColor Cyan
    } else {
        Write-Host "PATHに既に存在します: $Path" -ForegroundColor Yellow
    }
}

# ~/.npmrc で設定している prefix と揃えること
# (home/dot_npmrc.tmpl: AppData/Roaming/npm)
Add-ToUserPath -Path "$env:APPDATA\npm"
