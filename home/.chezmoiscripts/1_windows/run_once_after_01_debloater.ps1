<#
.SYNOPSIS
    Windows 11 Simple Debloater - セキュリティを保ちつつシンプルな設定に
.DESCRIPTION
    必要最小限のブロートウェア除去と設定変更を行う軽量スクリプト
    セキュリティ機能は維持し、システム安定性を重視
.NOTES
    Author: Custom Script
    Version: 1.1
    Requires: Administrator権限
#>

# Self-elevate the script if required
if (-Not ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole] 'Administrator')) {
  if ([int](Get-CimInstance -Class Win32_OperatingSystem | Select-Object -ExpandProperty BuildNumber) -ge 6000) {
    $CommandLine = "-NoExit -File `"" + $MyInvocation.MyCommand.Path + "`" " + $MyInvocation.UnboundArguments
    Start-Process -Wait -FilePath PowerShell.exe -Verb Runas -ArgumentList $CommandLine
    Exit
  }
}

# カラー出力設定
$Host.UI.RawUI.BackgroundColor = "Black"
$Host.UI.RawUI.ForegroundColor = "Green"
Clear-Host

Write-Host "================================================" -ForegroundColor Cyan
Write-Host "  Windows 11 Simple Debloater" -ForegroundColor Yellow
Write-Host "  セキュリティ保持 & シンプル設定" -ForegroundColor Yellow
Write-Host "================================================" -ForegroundColor Cyan
Write-Host ""

# 復元ポイント作成
Write-Host "システム復元ポイントを作成中..." -ForegroundColor Green
try {
  Checkpoint-Computer -Description "Debloater_Before_$(Get-Date -Format 'yyyyMMdd_HHmm')" -RestorePointType "MODIFY_SETTINGS"
  Write-Host "   ✓ 復元ポイント作成完了" -ForegroundColor Green
}
catch {
  Write-Host "   ⚠ 復元ポイント作成に失敗しました" -ForegroundColor Yellow
}

# 不要なプリインストールアプリの除去（安全なもののみ）
Write-Host "`n不要なアプリを除去中..." -ForegroundColor Green

$AppsToRemove = @(
  "Microsoft.BingNews",
  "Microsoft.BingWeather",
  "Microsoft.GetHelp",
  "Microsoft.Getstarted",
  "Microsoft.Microsoft3DViewer",
  "Microsoft.MicrosoftOfficeHub",
  "Microsoft.MicrosoftSolitaireCollection",
  "Microsoft.MixedReality.Portal",
  "Microsoft.Office.OneNote",
  "Microsoft.People",
  "Microsoft.PowerAutomateDesktop",
  "Microsoft.Print3D",
  "Microsoft.Todos",
  # "Microsoft.WindowsAlarms",
  "Microsoft.WindowsFeedbackHub",
  "Microsoft.WindowsMaps",
  # "Microsoft.WindowsSoundRecorder",
  # "Microsoft.ZuneMusic",
  # "Microsoft.ZuneVideo",
  "MicrosoftTeams"
  # "Clipchamp.Clipchamp"
)

foreach ($App in $AppsToRemove) {
  try {
    $Package = Get-AppxPackage -Name $App -AllUsers -ErrorAction SilentlyContinue
    if ($Package) {
      Remove-AppxPackage -Package $Package.PackageFullName -ErrorAction SilentlyContinue
      Get-AppxProvisionedPackage -Online | Where-Object DisplayName -EQ $App | Remove-AppxProvisionedPackage -Online -ErrorAction SilentlyContinue
      Write-Host "   ✓ $App を除去しました" -ForegroundColor Green
    }
  }
  catch {
    Write-Host "   ✗ $App の除去に失敗" -ForegroundColor Red
  }
}

# プライバシー設定の最適化
Write-Host "`nプライバシー設定を最適化中..." -ForegroundColor Green

# テレメトリーの最小化（完全無効化はしない）
if (-not (Test-Path "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\DataCollection")) {
  New-Item -Path "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\DataCollection" -Force | Out-Null
}
Set-ItemProperty -Path "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\DataCollection" -Name "AllowTelemetry" -Value 1 -Type DWord

if (-not (Test-Path "HKLM:\SOFTWARE\Policies\Microsoft\Windows\DataCollection")) {
  New-Item -Path "HKLM:\SOFTWARE\Policies\Microsoft\Windows\DataCollection" -Force | Out-Null
}
Set-ItemProperty -Path "HKLM:\SOFTWARE\Policies\Microsoft\Windows\DataCollection" -Name "AllowTelemetry" -Value 1 -Type DWord

# 広告IDの無効化
if (-not (Test-Path "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\AdvertisingInfo")) {
  New-Item -Path "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\AdvertisingInfo" -Force | Out-Null
}
Set-ItemProperty -Path "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\AdvertisingInfo" -Name "Enabled" -Value 0 -Type DWord

# 位置情報の制限
if (-not (Test-Path "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\CapabilityAccessManager\ConsentStore\location")) {
  New-Item -Path "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\CapabilityAccessManager\ConsentStore\location" -Force | Out-Null
}
Set-ItemProperty -Path "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\CapabilityAccessManager\ConsentStore\location" -Name "Value" -Value "Deny" -Type String

# アクティビティ履歴の無効化
if (-not (Test-Path "HKLM:\SOFTWARE\Policies\Microsoft\Windows\System")) {
  New-Item -Path "HKLM:\SOFTWARE\Policies\Microsoft\Windows\System" -Force | Out-Null
}
Set-ItemProperty -Path "HKLM:\SOFTWARE\Policies\Microsoft\Windows\System" -Name "EnableActivityFeed" -Value 0 -Type DWord
Set-ItemProperty -Path "HKLM:\SOFTWARE\Policies\Microsoft\Windows\System" -Name "PublishUserActivities" -Value 0 -Type DWord

Write-Host "   ✓ プライバシー設定完了" -ForegroundColor Green

# UI最適化
Write-Host "`nUI設定を最適化中..." -ForegroundColor Green

# スタートメニューからおすすめを除去
if (-not (Test-Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced")) {
  New-Item -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced" -Force | Out-Null
}
Set-ItemProperty -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced" -Name "Start_IrisRecommendations" -Value 0 -Type DWord

# タスクバーからWidget除去
Set-ItemProperty -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced" -Name "TaskbarDa" -Value 0 -Type DWord

# タスクバーからチャット除去
Set-ItemProperty -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced" -Name "TaskbarMn" -Value 0 -Type DWord

# 検索ボックスを無効化
if (-not (Test-Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Search")) {
  New-Item -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Search" -Force | Out-Null
}
Set-ItemProperty -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Search" -Name "SearchboxTaskbarMode" -Value 0 -Type DWord

# ロングファイルパスを有効化
if (-not (Test-Path "HKLM:\SYSTEM\CurrentControlSet\Control\FileSystem")) {
  New-Item -Path "HKLM:\SYSTEM\CurrentControlSet\Control\FileSystem" -Force | Out-Null
}
Set-ItemProperty -Path "HKLM:\SYSTEM\CurrentControlSet\Control\FileSystem" -Name "LongPathsEnabled" -Value 1 -Type DWord

# ファイル拡張子の表示
Set-ItemProperty -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced" -Name "HideFileExt" -Value 0 -Type DWord

# 隠しファイルの表示（コメントアウト）
# Set-ItemProperty -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced" -Name "Hidden" -Value 1 -Type DWord

Write-Host "   ✓ UI最適化完了" -ForegroundColor Green

# パフォーマンス最適化
Write-Host "`nパフォーマンス設定を最適化中..." -ForegroundColor Green

# 視覚効果の最適化
if (-not (Test-Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\VisualEffects")) {
  New-Item -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\VisualEffects" -Force | Out-Null
}
Set-ItemProperty -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\VisualEffects" -Name "VisualFXSetting" -Value 2 -Type DWord

# スタートアップアプリの最適化
if (-not (Test-Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Serialize")) {
  New-Item -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Serialize" -Force | Out-Null
}
Set-ItemProperty -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Serialize" -Name "StartupDelayInMSec" -Value 0 -Type DWord

Write-Host "   ✓ パフォーマンス最適化完了" -ForegroundColor Green

# Bing検索の無効化
Write-Host "`nWindows検索からBingを除去中..." -ForegroundColor Green
Set-ItemProperty -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Search" -Name "BingSearchEnabled" -Value 0 -Type DWord
Set-ItemProperty -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Search" -Name "CortanaConsent" -Value 0 -Type DWord
Write-Host "   ✓ Bing検索除去完了" -ForegroundColor Green

# 不要なサービスの無効化（慎重な選択）
Write-Host "`n不要なサービスを無効化中..." -ForegroundColor Green

$ServicesToDisable = @(
  "DiagTrack",            # Connected User Experiences and Telemetry
  "dmwappushservice",     # WAP Push Message Routing Service
  "SysMain",              # Superfetch (必要に応じて)
  "TrkWks"                # Distributed Link Tracking Client
)

foreach ($Service in $ServicesToDisable) {
  try {
    $ServiceObj = Get-Service -Name $Service -ErrorAction SilentlyContinue
    if ($ServiceObj) {
      Set-Service -Name $Service -StartupType Disabled -ErrorAction SilentlyContinue
      Stop-Service -Name $Service -Force -ErrorAction SilentlyContinue
      Write-Host "   ✓ $Service サービスを無効化" -ForegroundColor Green
    }
  }
  catch {
    Write-Host "   ✗ $Service の無効化に失敗" -ForegroundColor Red
  }
}

# セキュリティ設定の確認・強化
Write-Host "`nセキュリティ設定を確認中..." -ForegroundColor Green

# Windows Defenderが有効か確認
$DefenderStatus = Get-MpPreference -ErrorAction SilentlyContinue
if ($DefenderStatus) {
  Write-Host "   ✓ Windows Defender有効" -ForegroundColor Green
}
else {
  Write-Host "   ⚠ Windows Defenderの状態を確認してください" -ForegroundColor Yellow
}

# UACが有効か確認
$UACStatus = Get-ItemProperty -Path "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System" -Name "EnableLUA" -ErrorAction SilentlyContinue
if ($UACStatus.EnableLUA -eq 1) {
  Write-Host "   ✓ UAC有効" -ForegroundColor Green
}
else {
  Write-Host "   ⚠ UACが無効になっています" -ForegroundColor Yellow
}

# Windows Updateの自動更新確認
$WUStatus = Get-ItemProperty -Path "HKLM:\SOFTWARE\Policies\Microsoft\Windows\WindowsUpdate\AU" -Name "NoAutoUpdate" -ErrorAction SilentlyContinue
if (-not $WUStatus -or $WUStatus.NoAutoUpdate -eq 0) {
  Write-Host "   ✓ Windows Update自動更新有効" -ForegroundColor Green
}
else {
  Write-Host "   ⚠ Windows Update自動更新が無効です" -ForegroundColor Yellow
}

# Microsoft Defenderの除外設定追加
Write-Host "`nMicrosoft Defender: 除外設定を追加中..." -ForegroundColor Green
try {
  Add-MpPreference -ExclusionPath "$env:USERPROFILE\.config" -ErrorAction SilentlyContinue
  Add-MpPreference -ExclusionPath "$env:USERPROFILE\bin" -ErrorAction SilentlyContinue
  Add-MpPreference -ExclusionPath "$env:USERPROFILE\ghq" -ErrorAction SilentlyContinue
  Write-Host "   ✓ 除外設定追加完了" -ForegroundColor Green
}
catch {
  Write-Host "   ⚠ 除外設定の追加に失敗しました" -ForegroundColor Yellow
}

Write-Host "`n================================================" -ForegroundColor Cyan
Write-Host "  Debloat完了しました" -ForegroundColor Green
Write-Host "================================================" -ForegroundColor Cyan
