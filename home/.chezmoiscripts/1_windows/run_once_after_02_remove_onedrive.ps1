# OneDrive完全アンインストール・無効化スクリプト + フォルダー復元
# Windows 11 対応 (PowerShell版)

# Self-elevate the script if required
if (-Not ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole] 'Administrator')) {
  if ([int](Get-CimInstance -Class Win32_OperatingSystem | Select-Object -ExpandProperty BuildNumber) -ge 6000) {
    $CommandLine = "-NoExit -File `"" + $MyInvocation.MyCommand.Path + "`" " + $MyInvocation.UnboundArguments
    Start-Process -Wait -FilePath PowerShell.exe -Verb Runas -ArgumentList $CommandLine
    Exit
  }
}

Write-Host "=======================================================" -ForegroundColor Green
Write-Host "OneDrive 完全アンインストール・無効化 + フォルダー復元スクリプト" -ForegroundColor Green
Write-Host "Windows 11 対応 (PowerShell版)" -ForegroundColor Green
Write-Host "=======================================================" -ForegroundColor Green
Write-Host ""

# エラーハンドリング用関数
function Write-Status {
  param(
    [string]$Message,
    [string]$Status = "INFO"
  )

  switch ($Status) {
    "SUCCESS" { Write-Host "✓ $Message" -ForegroundColor Green }
    "ERROR" { Write-Host "✗ $Message" -ForegroundColor Red }
    "WARNING" { Write-Host "⚠ $Message" -ForegroundColor Yellow }
    "INFO" { Write-Host "ℹ $Message" -ForegroundColor Cyan }
  }
}

# フォルダー復元用関数
function Restore-UserFolder {
  param(
    [string]$FolderName,
    [string]$RegistryValue,
    [string]$DefaultPath
  )

  Write-Status "$FolderName フォルダーを標準の場所に復元中..."

  try {
    # レジストリから現在のパスを取得
    $shellFoldersPath = "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders"
    $userShellFoldersPath = "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders"

    $currentPath = Get-ItemProperty -Path $shellFoldersPath -Name $RegistryValue -ErrorAction SilentlyContinue

    if ($currentPath -and $currentPath.$RegistryValue -like "*OneDrive*") {
      Write-Status "$FolderName がOneDriveに同期されています。復元処理を開始します..." "INFO"

      # 標準のフォルダーが存在しない場合は作成
      if (-not (Test-Path $DefaultPath)) {
        New-Item -Path $DefaultPath -ItemType Directory -Force | Out-Null
        Write-Status "$FolderName フォルダーを作成しました: $DefaultPath" "SUCCESS"
      }

      # OneDriveフォルダーからファイルを移動（存在する場合）
      $oneDrivePath = $currentPath.$RegistryValue
      if (Test-Path $oneDrivePath) {
        Write-Status "$FolderName のファイルをOneDriveから標準の場所に移動中..." "INFO"

        try {
          $items = Get-ChildItem -Path $oneDrivePath -Force -ErrorAction SilentlyContinue
          foreach ($item in $items) {
            $destinationPath = Join-Path $DefaultPath $item.Name

            # 同名ファイルが存在する場合のハンドリング
            if (Test-Path $destinationPath) {
              $counter = 1
              $baseName = [System.IO.Path]::GetFileNameWithoutExtension($item.Name)
              $extension = [System.IO.Path]::GetExtension($item.Name)

              do {
                $newName = if ($extension) { "${baseName}_${counter}${extension}" } else { "${baseName}_${counter}" }
                $destinationPath = Join-Path $DefaultPath $newName
                $counter++
              } while (Test-Path $destinationPath)

              Write-Status "重複ファイルを $newName として移動します" "WARNING"
            }

            Move-Item -Path $item.FullName -Destination $destinationPath -Force -ErrorAction SilentlyContinue
          }
          Write-Status "$FolderName のファイル移動が完了しました" "SUCCESS"
        }
        catch {
          Write-Status "$FolderName のファイル移動中にエラーが発生しました: $($_.Exception.Message)" "WARNING"
        }
      }

      # レジストリを標準のパスに更新
      Set-ItemProperty -Path $shellFoldersPath -Name $RegistryValue -Value $DefaultPath -Type String
      Set-ItemProperty -Path $userShellFoldersPath -Name $RegistryValue -Value $DefaultPath -Type String

      Write-Status "$FolderName を標準の場所に復元しました: $DefaultPath" "SUCCESS"
    }
    else {
      Write-Status "$FolderName は既に標準の場所にあります" "INFO"
    }
  }
  catch {
    Write-Status "$FolderName の復元中にエラーが発生しました: $($_.Exception.Message)" "ERROR"
  }
}

# 1. ユーザーフォルダーの復元（OneDriveアンインストール前に実行）
Write-Status "ユーザーフォルダーをOneDriveから標準の場所に復元中..."

$userProfile = [Environment]::GetFolderPath('UserProfile')
$folders = @(
  @{
    Name          = "デスクトップ"
    RegistryValue = "Desktop"
    DefaultPath   = Join-Path $userProfile "Desktop"
  },
  @{
    Name          = "マイドキュメント"
    RegistryValue = "Personal"
    DefaultPath   = Join-Path $userProfile "Documents"
  },
  @{
    Name          = "ピクチャ"
    RegistryValue = "My Pictures"
    DefaultPath   = Join-Path $userProfile "Pictures"
  }
)

foreach ($folder in $folders) {
  Restore-UserFolder -FolderName $folder.Name -RegistryValue $folder.RegistryValue -DefaultPath $folder.DefaultPath
}

# 2. OneDriveプロセスの終了
Write-Status "OneDriveプロセスの終了中..."
try {
  Get-Process -Name "OneDrive" -ErrorAction SilentlyContinue | Stop-Process -Force
  Get-Process -Name "OneDriveSetup" -ErrorAction SilentlyContinue | Stop-Process -Force
  Write-Status "OneDriveプロセスを終了しました" "SUCCESS"
}
catch {
  Write-Status "OneDriveプロセスの終了中にエラーが発生しました: $($_.Exception.Message)" "WARNING"
}

# 3. 現在のユーザーのOneDriveをアンインストール
Write-Status "現在のユーザーのOneDriveをアンインストール中..."

# Wingetを使用してアンインストール（Microsoft.OneDrive）
try {
  Write-Status "Microsoft.OneDriveをアンインストール中..."
  winget uninstall Microsoft.OneDrive --silent
}
catch {
  Write-Status "Microsoft.OneDriveのアンインストール中にエラーが発生しました" "WARNING"
}

# Wingetを使用してアンインストール（onedrive）
try {
  Write-Status "onedriveをアンインストール中..."
  winget uninstall onedrive --silent
}
catch {
  Write-Status "onedriveのアンインストール中にエラーが発生しました" "WARNING"
}

# 4. 全ユーザー向けOneDriveのアンインストール
Write-Status "全ユーザー向けOneDriveのアンインストール中..."

$systemPaths = @(
  "C:\Program Files (x86)\Microsoft OneDrive\OneDriveSetup.exe",
  "C:\Windows\System32\OneDriveSetup.exe"
)

foreach ($path in $systemPaths) {
  if (Test-Path $path) {
    try {
      Start-Process -FilePath $path -ArgumentList "/uninstall", "/allusers" -Wait -NoNewWindow
      Write-Status "システムワイドOneDriveをアンインストールしました: $path" "SUCCESS"
    }
    catch {
      Write-Status "システムワイドOneDriveのアンインストールに失敗しました: $path" "ERROR"
    }
  }
}

# 5. 新規ユーザーへのOneDrive自動インストールを防止
Write-Status "新規ユーザーへのOneDrive自動インストールを防止中..."
try {
  reg load "hklm\Default_profile" "C:\Users\Default\NTUSER.DAT" 2>$null
  reg delete "hklm\Default_profile\SOFTWARE\Microsoft\Windows\CurrentVersion\Run" /v "OneDriveSetup" /f 2>$null
  reg unload "hklm\Default_profile" 2>$null
  Write-Status "デフォルトプロファイルからOneDriveSetupを削除しました" "SUCCESS"
}
catch {
  Write-Status "デフォルトプロファイルの編集に失敗しました" "WARNING"
}

# 6. OneDriveの統合機能を無効化
Write-Status "OneDriveの統合機能を無効化中..."
try {
  $policyPath = "HKLM:\SOFTWARE\Policies\Microsoft\Windows\OneDrive"
  if (-not (Test-Path $policyPath)) {
    New-Item -Path $policyPath -Force | Out-Null
  }
  Set-ItemProperty -Path $policyPath -Name "DisableFileSyncNGSC" -Value 1 -Type DWord
  Write-Status "OneDrive統合機能を無効化しました" "SUCCESS"
}
catch {
  Write-Status "OneDrive統合機能の無効化に失敗しました: $($_.Exception.Message)" "ERROR"
}

# 7. OneDriveのスタートアップエントリを削除
Write-Status "OneDriveのスタートアップエントリを削除中..."
try {
  $runPath = "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Run"

  $startupEntries = @("OneDriveSetup", "OneDrive")
  foreach ($entry in $startupEntries) {
    if (Get-ItemProperty -Path $runPath -Name $entry -ErrorAction SilentlyContinue) {
      Remove-ItemProperty -Path $runPath -Name $entry -ErrorAction SilentlyContinue
    }
  }
  Write-Status "スタートアップエントリを削除しました" "SUCCESS"
}
catch {
  Write-Status "スタートアップエントリの削除に失敗しました: $($_.Exception.Message)" "WARNING"
}

# 8. OneDriveフォルダーのクリーンアップ
Write-Status "OneDriveフォルダーのクリーンアップ中..."
try {
  $oneDrivePaths = @(
    (Join-Path $userProfile "OneDrive"),
    (Join-Path $userProfile "OneDrive - Personal")
  )

  foreach ($path in $oneDrivePaths) {
    if (Test-Path $path) {
      # フォルダーが空の場合のみ削除
      $items = Get-ChildItem -Path $path -Force -ErrorAction SilentlyContinue
      if ($items.Count -eq 0) {
        Remove-Item -Path $path -Recurse -Force -ErrorAction SilentlyContinue
        Write-Status "空のOneDriveフォルダーを削除しました: $path" "SUCCESS"
      }
      else {
        Write-Status "OneDriveフォルダーにファイルが残っています: $path" "WARNING"
      }
    }
  }
}
catch {
  Write-Status "OneDriveフォルダーのクリーンアップ中にエラーが発生しました: $($_.Exception.Message)" "WARNING"
}


Write-Host ""
Write-Host "=======================================================" -ForegroundColor Green
Write-Host "OneDriveの完全削除とフォルダー復元が完了しました！" -ForegroundColor Green
Write-Host ""
Write-Host "実行された処理:" -ForegroundColor Cyan
Write-Host "- ユーザーフォルダーの標準場所への復元" -ForegroundColor White
Write-Host "- OneDriveのアンインストール（現在のユーザー）" -ForegroundColor White
Write-Host "- OneDriveのアンインストール（全ユーザー）" -ForegroundColor White
Write-Host "- 新規ユーザーへの自動インストール防止" -ForegroundColor White
Write-Host "- OneDrive統合機能の無効化" -ForegroundColor White
Write-Host "- スタートアップエントリの削除" -ForegroundColor White
Write-Host "- OneDriveフォルダーのクリーンアップ" -ForegroundColor White
Write-Host ""
Write-Host "注意: 変更を完全に反映するには手動で再起動してください。" -ForegroundColor Yellow
Write-Host "=======================================================" -ForegroundColor Green
Write-Host ""

Write-Host "スクリプトの実行が完了しました。" -ForegroundColor Green
