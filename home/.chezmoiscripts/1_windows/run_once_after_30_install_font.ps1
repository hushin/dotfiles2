# GitHubフォント自動インストールスクリプト (全自動版)

# Self-elevate the script if required
if (-Not ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole] 'Administrator')) {
  if ([int](Get-CimInstance -Class Win32_OperatingSystem | Select-Object -ExpandProperty BuildNumber) -ge 6000) {
    $CommandLine = "-NoExit -File `"" + $MyInvocation.MyCommand.Path + "`" " + $MyInvocation.UnboundArguments
    Start-Process -Wait -FilePath PowerShell.exe -Verb Runas -ArgumentList $CommandLine
    Exit
  }
}

# フォント定義
$fonts = @(
  @{
    Name    = "HackGen"
    Repo    = "yuru7/HackGen"
    Pattern = "*HackGen*.zip"
  },
  @{
    Name    = "UDEV Gothic"
    Repo    = "yuru7/udev-gothic"
    Pattern = "*UDEVGothic*.zip"
  },
  @{
    Name    = "Bizin Gothic"
    Repo    = "yuru7/bizin-gothic"
    Pattern = "*BizinGothic*.zip"
  }
)

function Install-FontFromGitHub {
  param(
    [string]$FontName,
    [string]$RepoName,
    [string]$ZipPattern
  )

  Write-Host "=== $FontName のインストール開始 ===" -ForegroundColor Cyan

  # 既存インストールチェック
  $regPath = "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Fonts"
  $existingFonts = Get-ItemProperty -Path $regPath -ErrorAction SilentlyContinue
  $fontNameNoSpace = $FontName -replace '\s+', ''
  $isInstalled = $existingFonts.PSObject.Properties | Where-Object {
    $nameNoSpace = ($_.Name -replace '\s+', '')
    $valueNoSpace = ($_.Value -replace '\s+', '')
    $nameNoSpace -like "*$fontNameNoSpace*" -or $valueNoSpace -like "*$fontNameNoSpace*"
  }

  if ($isInstalled) {
    Write-Host "$FontName は既にインストール済みです。スキップします。" -ForegroundColor Green
    return $true
  }

  # 一時ディレクトリ作成
  $tempDir = [System.IO.Path]::GetTempPath() + [System.Guid]::NewGuid().ToString()
  New-Item -ItemType Directory -Path $tempDir -Force | Out-Null

  try {
    # GitHub APIでリリース情報取得
    $apiUrl = "https://api.github.com/repos/$RepoName/releases/latest"
    Write-Host "リリース情報を取得中..." -ForegroundColor Yellow

    $release = Invoke-RestMethod -Uri $apiUrl
    Write-Host "最新バージョン: $($release.tag_name)" -ForegroundColor Green

    # フォントアセット検索
    $fontAssets = $release.assets | Where-Object { $_.name -like $ZipPattern }

    if (-not $fontAssets) {
      Write-Host "エラー: フォントファイルが見つかりません" -ForegroundColor Red
      return $false
    }

    $installCount = 0

    foreach ($asset in $fontAssets) {
      Write-Host "ダウンロード中: $($asset.name)" -ForegroundColor Yellow
      $zipPath = Join-Path $tempDir $asset.name

      # ダウンロード
      Invoke-WebRequest -Uri $asset.browser_download_url -OutFile $zipPath

      # ZIP展開
      Write-Host "展開中..." -ForegroundColor Yellow
      $extractPath = Join-Path $tempDir "extracted_$([System.Guid]::NewGuid().ToString())"
      Expand-Archive -Path $zipPath -DestinationPath $extractPath -Force

      # フォントファイル検索・インストール
      $fontFiles = Get-ChildItem -Path $extractPath -Recurse -Include "*.ttf", "*.otf" |
      Where-Object { $_.Name -notlike "*LICENSE*" }

      Write-Host "フォントファイル数: $($fontFiles.Count)" -ForegroundColor Green

      foreach ($fontFile in $fontFiles) {
        Write-Host "インストール中: $($fontFile.Name)" -ForegroundColor White

        try {
          # システムフォントフォルダにコピー
          $destination = Join-Path $env:WINDIR "Fonts\$($fontFile.Name)"
          Copy-Item -Path $fontFile.FullName -Destination $destination -Force

          # レジストリに登録
          $fontDisplayName = [System.IO.Path]::GetFileNameWithoutExtension($fontFile.Name)
          $regPath = "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Fonts"
          $regName = if ($fontFile.Extension -eq ".ttf") {
            "$fontDisplayName (TrueType)"
          }
          else {
            "$fontDisplayName (OpenType)"
          }

          Set-ItemProperty -Path $regPath -Name $regName -Value $fontFile.Name -Force
          $installCount++

        }
        catch {
          Write-Host "エラー: $($fontFile.Name) のインストールに失敗 - $($_.Exception.Message)" -ForegroundColor Red
        }
      }
    }

    Write-Host "$FontName インストール完了: $installCount ファイル" -ForegroundColor Green
    return $true

  }
  catch {
    Write-Host "エラー: $FontName のインストールに失敗 - $($_.Exception.Message)" -ForegroundColor Red
    return $false
  }
  finally {
    # 一時ファイル削除
    if (Test-Path $tempDir) {
      Remove-Item -Path $tempDir -Recurse -Force -ErrorAction SilentlyContinue
    }
  }
}

# メイン処理
Write-Host "GitHubフォント自動インストールスクリプト" -ForegroundColor Magenta
Write-Host "=======================================" -ForegroundColor Magenta
Write-Host "すべてのフォントを自動インストールします..." -ForegroundColor Cyan

# フォントキャッシュクリア
Write-Host "フォントキャッシュをクリア中..." -ForegroundColor Yellow
try {
  Stop-Service -Name "FontCache" -Force -ErrorAction SilentlyContinue
  $fontCachePath = "$env:WINDIR\ServiceProfiles\LocalService\AppData\Local\FontCache"
  if (Test-Path $fontCachePath) {
    Remove-Item -Path "$fontCachePath\*" -Force -Recurse -ErrorAction SilentlyContinue
  }
  Start-Service -Name "FontCache" -ErrorAction SilentlyContinue
}
catch {
  Write-Host "警告: フォントキャッシュのクリアに失敗しました" -ForegroundColor Yellow
}

# インストール実行
$successCount = 0
foreach ($font in $fonts) {
  $result = Install-FontFromGitHub -FontName $font.Name -RepoName $font.Repo -ZipPattern $font.Pattern
  if ($result) { $successCount++ }
  Write-Host "" # 空行
}

# 結果表示
Write-Host "=======================================" -ForegroundColor Magenta
Write-Host "インストール完了: $successCount/$($fonts.Count) フォント" -ForegroundColor Green

if ($successCount -gt 0) {
  Write-Host "注意: フォントを完全に反映させるには再起動またはログオフが推奨されます" -ForegroundColor Yellow
}

Write-Host "スクリプト実行完了" -ForegroundColor Green
