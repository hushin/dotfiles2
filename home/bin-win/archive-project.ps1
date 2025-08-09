#!/usr/bin/env pwsh

<#
.SYNOPSIS
    プロジェクトをfuzzy検索してNASにアーカイブする
.DESCRIPTION
    $env:USERPROFILE/projects/ 以下のプロジェクトをfzfで検索選択し、
    \\DS218Plus\home\archive-projects にコピーしてアーカイブする
#>

param(
    [switch]$DryRun = $false
)

$ProjectsPath = "$env:USERPROFILE\projects"
$ArchivePath = "\\DS218Plus\home\archive-projects"

# プロジェクトディレクトリの存在確認
if (-not (Test-Path $ProjectsPath)) {
    Write-Error "プロジェクトディレクトリが見つかりません: $ProjectsPath"
    exit 1
}

# アーカイブ先の存在確認
if (-not $DryRun -and -not (Test-Path $ArchivePath)) {
    Write-Error "アーカイブ先が見つかりません: $ArchivePath"
    Write-Host "NASがマウントされているか確認してください"
    exit 1
}

# プロジェクト一覧を取得
$Projects = Get-ChildItem -Path $ProjectsPath -Directory | Select-Object -ExpandProperty Name

if ($Projects.Count -eq 0) {
    Write-Host "プロジェクトが見つかりません"
    exit 0
}

Write-Host "アーカイブするプロジェクトを選択してください (Ctrl+C でキャンセル):"
Write-Host ""

# fzfでプロジェクトを選択
$SelectedProject = $Projects | fzf --prompt="Project to archive> " --height=40% --reverse

if (-not $SelectedProject) {
    Write-Host "キャンセルされました"
    exit 0
}

$SourcePath = Join-Path $ProjectsPath $SelectedProject
$DestinationPath = Join-Path $ArchivePath $SelectedProject

# 確認
Write-Host ""
Write-Host "選択されたプロジェクト: $SelectedProject" -ForegroundColor Yellow
Write-Host "コピー元: $SourcePath" -ForegroundColor Cyan
Write-Host "コピー先: $DestinationPath" -ForegroundColor Cyan

if ($DryRun) {
    Write-Host ""
    Write-Host "DRY RUN モードです。実際のコピーは行いません。" -ForegroundColor Green
    exit 0
}

Write-Host ""
$Confirm = Read-Host "アーカイブを実行しますか? (y/N)"

if ($Confirm -ne 'y' -and $Confirm -ne 'Y') {
    Write-Host "キャンセルされました"
    exit 0
}

# アーカイブ先に同名のプロジェクトが既に存在するかチェック
if (Test-Path $DestinationPath) {
    Write-Host ""
    Write-Warning "アーカイブ先に同名のプロジェクトが既に存在します: $DestinationPath"
    $Overwrite = Read-Host "上書きしますか? (y/N)"

    if ($Overwrite -ne 'y' -and $Overwrite -ne 'Y') {
        Write-Host "キャンセルされました"
        exit 0
    }

    Write-Host "既存のアーカイブを削除しています..."
    Remove-Item -Path $DestinationPath -Recurse -Force
}

# コピー実行
try {
    Write-Host ""
    Write-Host "アーカイブを開始します..." -ForegroundColor Green
    Copy-Item -Path $SourcePath -Destination $DestinationPath -Recurse -Force

    Write-Host "✓ アーカイブが完了しました!" -ForegroundColor Green
    Write-Host "アーカイブ先: $DestinationPath"

    # 元のプロジェクトを削除するか確認
    Write-Host ""
    $DeleteOriginal = Read-Host "元のプロジェクトを削除しますか? (y/N)"

    if ($DeleteOriginal -eq 'y' -or $DeleteOriginal -eq 'Y') {
        Remove-Item -Path $SourcePath -Recurse -Force
        Write-Host "✓ 元のプロジェクトを削除しました" -ForegroundColor Green
    } else {
        Write-Host "元のプロジェクトは保持されます: $SourcePath" -ForegroundColor Yellow
    }

} catch {
    Write-Error "アーカイブに失敗しました: $($_.Exception.Message)"
    exit 1
}
