# Claude Code のインストール確認とインストール
if (-not (Get-Command claude -ErrorAction SilentlyContinue)) {
    Write-Host "Claude Code がインストールされていません。インストールを開始します..." -ForegroundColor Yellow
    irm https://claude.ai/install.ps1 | iex
    Write-Host "Claude Code のインストールが完了しました。" -ForegroundColor Green
} else {
    Write-Host "Claude Code は既にインストールされています。" -ForegroundColor Green
}
