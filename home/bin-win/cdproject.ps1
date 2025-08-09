$projectsDir = "$env:USERPROFILE\projects"

if (-not (Test-Path $projectsDir)) {
    Write-Host "プロジェクトディレクトリ '$projectsDir' が存在しません。" -ForegroundColor Red
    return
}

$projects = Get-ChildItem -Path $projectsDir -Directory | Select-Object -ExpandProperty Name

if ($projects.Count -eq 0) {
    Write-Host "プロジェクトが見つかりません。" -ForegroundColor Yellow
    return
}

$selectedProject = $projects | fzf --prompt "プロジェクトを選択: "

if (-not $selectedProject) {
    Write-Host "プロジェクトが選択されませんでした。" -ForegroundColor Yellow
    return
}

$targetDir = Join-Path $projectsDir $selectedProject
Set-Location $targetDir
Write-Host "ディレクトリを移動しました: $targetDir" -ForegroundColor Blue
