# chezmoi add symlink script (PowerShell版)
# Usage: .\chezmoi-add-symlink.ps1 <source_file> <app_name> [target_path]
# Example: .\chezmoi-add-symlink.ps1 settings.json VSCode $env:APPDATA\Code\User\settings.json

param(
    [Parameter(Mandatory = $true, Position = 0)]
    [string]$SourceFile,

    [Parameter(Mandatory = $true, Position = 1)]
    [string]$AppName,

    [Parameter(Position = 2)]
    [string]$TargetPath = ""
)

# エラーハンドリング設定
$ErrorActionPreference = "Stop"

# カラー出力用の定数
$Colors = @{
    Red    = 'Red'
    Green  = 'Green'
    Yellow = 'Yellow'
    Blue   = 'Blue'
}

# ヘルパー関数
function Write-LogInfo {
    param([string]$Message)
    Write-Host "[INFO] $Message" -ForegroundColor $Colors.Blue
}

function Write-LogSuccess {
    param([string]$Message)
    Write-Host "[SUCCESS] $Message" -ForegroundColor $Colors.Green
}

function Write-LogWarning {
    param([string]$Message)
    Write-Host "[WARNING] $Message" -ForegroundColor $Colors.Yellow
}

function Write-LogError {
    param([string]$Message)
    Write-Host "[ERROR] $Message" -ForegroundColor $Colors.Red
}

function Show-Usage {
    @"
Usage: .\chezmoi-add-symlink.ps1 <source_file> <app_name> [target_path]

Arguments:
  source_file   対象ファイルのパス（相対パス・絶対パス可）
  app_name      アプリケーション名（シンボリックリンク先のディレクトリ名）
  target_path   ファイルの本来の場所（省略時は現在のパスを使用）

Examples:
  .\chezmoi-add-symlink.ps1 settings.json VSCode `$env:APPDATA\Code\User\settings.json
  .\chezmoi-add-symlink.ps1 ..\config\app.conf MyApp
  .\chezmoi-add-symlink.ps1 C:\Users\user\.vimrc Vim `$env:USERPROFILE\.vimrc

このスクリプトは以下の処理を行います：
1. 指定されたファイルをchezmoi source配下のsymlink-srcディレクトリにコピー
2. 元の場所にシンボリックリンクテンプレートを作成
3. chezmoi applyを実行してシンボリックリンクを適用
"@
}

# chezmoiのインストール確認
function Test-ChezmoiInstalled {
    try {
        $null = Get-Command chezmoi -ErrorAction Stop
        return $true
    }
    catch {
        Write-LogError "chezmoiがインストールされていません"
        return $false
    }
}

# 絶対パス取得
function Get-AbsolutePath {
    param([string]$Path)

    if ([System.IO.Path]::IsPathRooted($Path)) {
        return $Path
    }
    else {
        return Join-Path (Get-Location) $Path | Resolve-Path
    }
}

# chezmoi形式のパスに変換
function ConvertTo-ChezmoiPath {
    param([string]$AbsolutePath)

    $homeDir = $env:USERPROFILE

    # ホームディレクトリ配下でない場合はエラー
    if (-not $AbsolutePath.StartsWith($homeDir, [System.StringComparison]::OrdinalIgnoreCase)) {
        Write-LogError "ファイルはホームディレクトリ配下にある必要があります: $AbsolutePath"
        exit 1
    }

    # ホームディレクトリ部分を除去
    $relativePath = $AbsolutePath.Substring($homeDir.Length).TrimStart('\')

    # パス要素を分割してchezmoi形式に変換
    $pathComponents = $relativePath.Split([System.IO.Path]::DirectorySeparatorChar, [System.IO.Path]::AltDirectorySeparatorChar)
    $chezmoiPath = ""

    foreach ($component in $pathComponents) {
        if (-not [string]::IsNullOrEmpty($component)) {
            # 隠しファイル/ディレクトリの場合はprivate_dot_プレフィックスを追加
            if ($component.StartsWith('.')) {
                $component = "private_dot_" + $component.Substring(1)
            }
            if ([string]::IsNullOrEmpty($chezmoiPath)) {
                $chezmoiPath = $component
            } else {
                $chezmoiPath = Join-Path $chezmoiPath $component
            }
        }
    }

    return $chezmoiPath
}

# メイン処理
function Invoke-ProcessFile {
    param(
        [string]$SourceFile,
        [string]$AppName,
        [string]$TargetPath
    )

    # ソースファイルの絶対パス取得
    try {
        $absSourcePath = Get-AbsolutePath $SourceFile
    }
    catch {
        Write-LogError "ソースファイルのパス解決でエラー: $_"
        exit 1
    }

    # ソースファイルの存在確認
    if (-not (Test-Path $absSourcePath -PathType Leaf)) {
        Write-LogError "ソースファイルが存在しません: $absSourcePath"
        exit 1
    }

    # ターゲットパスの決定
    if ([string]::IsNullOrEmpty($TargetPath)) {
        $TargetPath = $absSourcePath
        Write-LogInfo "ターゲットパスが指定されていないため、ソースパスを使用: $TargetPath"
    }
    else {
        try {
            $TargetPath = Get-AbsolutePath $TargetPath
        }
        catch {
            Write-LogError "ターゲットパスの解決でエラー: $_"
            exit 1
        }
    }

    # chezmoiソースディレクトリ取得
    try {
        $chezmoiSourceDir = (chezmoi source-path).Trim()
    }
    catch {
        Write-LogError "chezmoi source-pathの実行でエラー: $_"
        exit 1
    }

    # symlink-srcディレクトリ構造作成
    $symlinkSrcDir = Join-Path $chezmoiSourceDir "symlink-src" $AppName
    Write-LogInfo "シンボリックリンクソースディレクトリを作成: $symlinkSrcDir"

    try {
        New-Item -ItemType Directory -Path $symlinkSrcDir -Force | Out-Null
    }
    catch {
        Write-LogError "ディレクトリ作成エラー: $_"
        exit 1
    }

    # ソースファイルをsymlink-srcディレクトリにコピー
    $filename = [System.IO.Path]::GetFileName($absSourcePath)
    $symlinkTarget = Join-Path $symlinkSrcDir $filename

    Write-LogInfo "ファイルをコピー: $absSourcePath -> $symlinkTarget"
    try {
        Copy-Item $absSourcePath $symlinkTarget -Force
    }
    catch {
        Write-LogError "ファイルコピーエラー: $_"
        exit 1
    }

    # ターゲットパスをchezmoi形式に変換
    $chezmoiTargetPath = ConvertTo-ChezmoiPath $TargetPath

    # chezmoiソース内にディレクトリ構造作成
    $chezmoiTargetDir = Split-Path $chezmoiTargetPath -Parent
    if ([string]::IsNullOrEmpty($chezmoiTargetDir)) {
        $chezmoiDir = $chezmoiSourceDir
    } else {
        $chezmoiDir = Join-Path $chezmoiSourceDir $chezmoiTargetDir
    }
    Write-LogInfo "chezmoiディレクトリ構造を作成: $chezmoiDir"

    try {
        New-Item -ItemType Directory -Path $chezmoiDir -Force | Out-Null
    }
    catch {
        Write-LogError "chezmoiディレクトリ作成エラー: $_"
        exit 1
    }

    # シンボリックリンクテンプレート作成
    $templateName = "symlink_" + [System.IO.Path]::GetFileName($chezmoiTargetPath) + ".tmpl"
    $templatePath = Join-Path $chezmoiDir $templateName

    Write-LogInfo "シンボリックリンクテンプレートを作成: $templatePath"
    $templateContent = "{{ .chezmoi.sourceDir }}/symlink-src/$AppName/$filename"

    try {
        Set-Content -Path $templatePath -Value $templateContent -NoNewline -Encoding UTF8
    }
    catch {
        Write-LogError "テンプレートファイル作成エラー: $_"
        exit 1
    }

    Write-LogSuccess "設定完了！"
    Write-LogInfo "以下のファイルが作成されました："
    Write-LogInfo "  - シンボリックリンクソース: $symlinkTarget"
    Write-LogInfo "  - テンプレート: $templatePath"

    # chezmoi apply実行確認
    $response = Read-Host "chezmoi applyを実行しますか？ (y/N)"
    if ($response -match '^[Yy]$') {
        Write-LogInfo "chezmoi applyを実行中..."
        try {
            $output = chezmoi apply -v 2>&1
            if ($LASTEXITCODE -eq 0) {
                Write-LogSuccess "chezmoi applyが完了しました"
                Write-LogInfo "これで $TargetPath は $symlinkTarget へのシンボリックリンクになりました"
            }
            else {
                Write-LogError "chezmoi applyでエラーが発生しました: $output"
                exit 1
            }
        }
        catch {
            Write-LogError "chezmoi apply実行エラー: $_"
            exit 1
        }
    }
    else {
        Write-LogInfo "後で手動で 'chezmoi apply' を実行してください"
    }
}

# メイン処理開始
function Main {
    # ヘルプ表示
    if ($SourceFile -eq "-h" -or $SourceFile -eq "--help") {
        Show-Usage
        exit 0
    }

    # chezmoi インストール確認
    if (-not (Test-ChezmoiInstalled)) {
        exit 1
    }

    # ファイル処理実行
    Invoke-ProcessFile $SourceFile $AppName $TargetPath
}

# スクリプト実行
try {
    Main
}
catch {
    Write-LogError "予期しないエラーが発生しました: $_"
    exit 1
}
