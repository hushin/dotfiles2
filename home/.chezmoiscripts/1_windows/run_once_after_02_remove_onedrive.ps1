# OneDrive完全アンインストール・無効化スクリプト
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
Write-Host "OneDrive 完全アンインストール・無効化スクリプト" -ForegroundColor Green
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

# 1. OneDriveプロセスの終了
Write-Status "OneDriveプロセスの終了中..."
try {
  Get-Process -Name "OneDrive" -ErrorAction SilentlyContinue | Stop-Process -Force
  Get-Process -Name "OneDriveSetup" -ErrorAction SilentlyContinue | Stop-Process -Force
  Write-Status "OneDriveプロセスを終了しました" "SUCCESS"
}
catch {
  Write-Status "OneDriveプロセスの終了中にエラーが発生しました: $($_.Exception.Message)" "WARNING"
}

# 2. 現在のユーザーのOneDriveをアンインストール
Write-Status "現在のユーザーのOneDriveをアンインストール中..."

# Wingetを使用してアンインストール（Microsoft.OneDrive）
try {
  Write-Status "Microsoft.OneDriveをアンインストール中..."
  $wingetResult1 = winget uninstall Microsoft.OneDrive --silent 2>&1
  if ($LASTEXITCODE -eq 0) {
    Write-Status "Microsoft.OneDriveのアンインストールが完了しました" "SUCCESS"
  }
  else {
    Write-Status "Microsoft.OneDriveのアンインストールに失敗しました" "WARNING"
  }
}
catch {
  Write-Status "Microsoft.OneDriveのアンインストール中にエラーが発生しました" "WARNING"
}

# Wingetを使用してアンインストール（onedrive）
try {
  Write-Status "onedriveをアンインストール中..."
  $wingetResult2 = winget uninstall onedrive --silent 2>&1
  if ($LASTEXITCODE -eq 0) {
    Write-Status "onedriveのアンインストールが完了しました" "SUCCESS"
  }
  else {
    Write-Status "onedriveのアンインストールに失敗しました" "WARNING"
  }
}
catch {
  Write-Status "onedriveのアンインストール中にエラーが発生しました" "WARNING"
}

# 3. 全ユーザー向けOneDriveのアンインストール
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

# 4. 新規ユーザーへのOneDrive自動インストールを防止
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

# 5. OneDriveの統合機能を無効化
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

# 6. Navigation PaneからOneDriveアイコンを削除
Write-Status "Navigation PaneからOneDriveアイコンを削除中..."
try {
  $clsidPath = "HKCR:\CLSID\{018D5C66-4533-4307-9B53-224DE2ED1FE6}"
  $userClsidPath = "HKCU:\SOFTWARE\Classes\CLSID\{018D5C66-4533-4307-9B53-224DE2ED1FE6}"

  # システムワイド設定
  if (-not (Test-Path $clsidPath)) {
    New-Item -Path $clsidPath -Force | Out-Null
  }
  Set-ItemProperty -Path $clsidPath -Name "System.IsPinnedToNameSpaceTree" -Value 0 -Type DWord

  # ユーザー設定
  if (-not (Test-Path $userClsidPath)) {
    New-Item -Path $userClsidPath -Force | Out-Null
  }
  Set-ItemProperty -Path $userClsidPath -Name "System.IsPinnedToNameSpaceTree" -Value 0 -Type DWord

  Write-Status "Navigation PaneからOneDriveアイコンを削除しました" "SUCCESS"
}
catch {
  Write-Status "Navigation Paneアイコンの削除に失敗しました: $($_.Exception.Message)" "ERROR"
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

# 8. OneDriveフォルダとキャッシュのクリーンアップ
Write-Status "OneDriveフォルダとキャッシュのクリーンアップ中..."

$foldersToRemove = @(
  "$env:USERPROFILE\OneDrive",
  "$env:LOCALAPPDATA\Microsoft\OneDrive",
  "$env:APPDATA\Microsoft\OneDrive"
)

foreach ($folder in $foldersToRemove) {
  if (Test-Path $folder) {
    try {
      Remove-Item -Path $folder -Recurse -Force -ErrorAction SilentlyContinue
      Write-Status "削除しました: $folder" "SUCCESS"
    }
    catch {
      Write-Status "削除に失敗しました: $folder - $($_.Exception.Message)" "WARNING"
    }
  }
}

# 9. エクスプローラーの再起動
Write-Status "エクスプローラーを再起動中..."
try {
  Stop-Process -Name "explorer" -Force
  Start-Sleep -Seconds 2
  Start-Process "explorer.exe"
  Write-Status "エクスプローラーを再起動しました" "SUCCESS"
}
catch {
  Write-Status "エクスプローラーの再起動に失敗しました: $($_.Exception.Message)" "WARNING"
}

Write-Host ""
Write-Host "=======================================================" -ForegroundColor Green
Write-Host "OneDriveの完全削除が完了しました！" -ForegroundColor Green
Write-Host ""
Write-Host "実行された処理:" -ForegroundColor Cyan
Write-Host "- OneDriveのアンインストール（現在のユーザー）" -ForegroundColor White
Write-Host "- OneDriveのアンインストール（全ユーザー）" -ForegroundColor White
Write-Host "- 新規ユーザーへの自動インストール防止" -ForegroundColor White
Write-Host "- OneDrive統合機能の無効化" -ForegroundColor White
Write-Host "- Navigation Paneからアイコン削除" -ForegroundColor White
Write-Host "- スタートアップエントリの削除" -ForegroundColor White
Write-Host "- OneDriveフォルダとキャッシュの削除" -ForegroundColor White
Write-Host ""
Write-Host "注意: 変更を完全に反映するには手動で再起動してください。" -ForegroundColor Yellow
Write-Host "=======================================================" -ForegroundColor Green
Write-Host ""

Write-Host "スクリプトの実行が完了しました。" -ForegroundColor Green
