# Claude Code のインストール確認とインストール
if (-not (Get-Command claude -ErrorAction SilentlyContinue)) {
    Write-Host "Claude Code がインストールされていません。インストールを開始します..." -ForegroundColor Yellow
    irm https://claude.ai/install.ps1 | iex
    Write-Host "Claude Code のインストールが完了しました。" -ForegroundColor Green
} else {
    Write-Host "Claude Code は既にインストールされています。" -ForegroundColor Green
}

$env:Path = [System.Environment]::GetEnvironmentVariable("Path", "Machine") + ";" +
    [System.Environment]::GetEnvironmentVariable("Path", "User")

# Google Workspace CLI (gws)
if (-not (Get-Command gws -ErrorAction SilentlyContinue) -and (Get-Command npm -ErrorAction SilentlyContinue)) {
    Write-Host "Installing @googleworkspace/cli (gws)..." -ForegroundColor Yellow
    npm install -g --allow-scripts=@googleworkspace/cli @googleworkspace/cli
}
