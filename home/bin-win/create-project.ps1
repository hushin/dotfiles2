param(
    [Parameter(Mandatory=$true)]
    [string]$ProjectName
)

$currentDate = Get-Date -Format "yyyyMM"
$projectDir = "$env:USERPROFILE\projects\${currentDate}-${ProjectName}"

if (Test-Path $projectDir) {
    Write-Host "プロジェクト '$projectDir' は既に存在します。" -ForegroundColor Yellow
} else {
    New-Item -ItemType Directory -Path $projectDir -Force | Out-Null
    Write-Host "プロジェクト '$projectDir' を作成しました。" -ForegroundColor Green
}

Set-Location $projectDir
Write-Host "ディレクトリを移動しました: $projectDir" -ForegroundColor Blue
