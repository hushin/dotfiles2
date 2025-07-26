Write-Host "ユーザー環境変数を設定します" -ForegroundColor Green

# HOME環境変数を設定
[Environment]::SetEnvironmentVariable("HOME", $env:USERPROFILE, "User")
Write-Host "HOME環境変数を設定しました: $env:USERPROFILE" -ForegroundColor Cyan

Write-Host "ユーザー環境変数PATHを追加します" -ForegroundColor Green

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

$binWinPath = "$HOME\bin-win"
Add-ToUserPath -Path $binWinPath

$miseShimPath = "$env:USERPROFILE\AppData\Local\mise\shims"
Add-ToUserPath -Path $miseShimPath
