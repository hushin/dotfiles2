# JIS キーボードのキーリマップ変更スクリプト

# Self-elevate the script if required
if (-Not ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole] 'Administrator')) {
  if ([int](Get-CimInstance -Class Win32_OperatingSystem | Select-Object -ExpandProperty BuildNumber) -ge 6000) {
    $CommandLine = "-NoExit -File `"" + $MyInvocation.MyCommand.Path + "`" " + $MyInvocation.UnboundArguments
    Start-Process -Wait -FilePath PowerShell.exe -Verb Runas -ArgumentList $CommandLine
    Exit
  }
}

Write-Host "完全キーリマップ設定を開始します..." -ForegroundColor Green
Write-Host ""
Write-Host "設定内容:" -ForegroundColor Cyan
Write-Host "  Caps Lock ⇄ Control (相互入れ替え)" -ForegroundColor White
Write-Host "  無変換キー → F13" -ForegroundColor White
Write-Host "  変換キー   → スペースキー" -ForegroundColor White
Write-Host "  左Alt     → F13" -ForegroundColor White
Write-Host "  左Win     → 左Alt" -ForegroundColor White
Write-Host "  カナ/かな → F14" -ForegroundColor White
Write-Host "  右Alt     → F14" -ForegroundColor White
Write-Host "  Application → 右Alt" -ForegroundColor White
Write-Host "  ￥\を見た目の位置で打てるようにする" -ForegroundColor White
Write-Host "  ]の位置で半角全角(\`) " -ForegroundColor White
Write-Host ""

# レジストリパス
$registryPath = "HKLM:\SYSTEM\CurrentControlSet\Control\Keyboard Layout"
$valueName = "Scancode Map"

try {
  # 既存のScancode Mapをバックアップ
  $existingValue = Get-ItemProperty -Path $registryPath -Name $valueName -ErrorAction SilentlyContinue
  if ($existingValue) {
    Write-Host "既存のScancode Mapが見つかりました。バックアップを作成します..." -ForegroundColor Yellow
    $backupPath = "HKLM:\SYSTEM\CurrentControlSet\Control\Keyboard Layout\Backup_$(Get-Date -Format 'yyyyMMdd_HHmmss')"
    New-Item -Path $backupPath -Force | Out-Null
    Set-ItemProperty -Path $backupPath -Name "Original_Scancode_Map" -Value $existingValue.$valueName
    Write-Host "バックアップが作成されました: $backupPath" -ForegroundColor Green
  }

  # Scancode Mapの値を設定
  # バイト配列の説明:
  # 00,00,00,00 - バージョン（常に0）
  # 00,00,00,00 - フラグ（常に0）
  # 0b,00,00,00 - エントリ数（10つのマッピング + 終了マーカー = 11）
  # 1d,00,3a,00 - Caps Lock (0x3a) を Left Ctrl (0x1d) にマップ
  # 3a,00,1d,00 - Left Ctrl (0x1d) を Caps Lock (0x3a) にマップ
  # 64,00,7b,00 - 無変換キー (0x7b) を F13 (0x64) にマップ
  # 39,00,79,00 - 変換キー (0x79) を スペースキー (0x39) にマップ
  # 64,00,38,00 - Left Alt (0x38) を F13 (0x64) にマップ
  # 38,00,5b,e0 - Left Win (0xe05b) を Left Alt (0x38) にマップ
  # 65,00,70,00 - カナ/かな (0x70) を F14 (0x65) にマップ
  # 65,00,38,e0 - 右Alt (0xe038) を F14 (0x65) にマップ
  # 38,e0,5d,e0 - Application (0xe05d) を 右Alt (0xe038) にマップ
  # 2b,00,7d,00 - 0x7d を 0x2b にマップ
  # 29,00,2b,00 - 0x2b を 半角全角 (0x29) にマップ
  # 00,00,00,00 - 終了マーカー
  $scancodeMap = [byte[]](0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0c, 0x00, 0x00, 0x00, 0x1d, 0x00, 0x3a, 0x00, 0x3a, 0x00, 0x1d, 0x00, 0x64, 0x00, 0x7b, 0x00, 0x39, 0x00, 0x79, 0x00, 0x64, 0x00, 0x38, 0x00, 0x38, 0x00, 0x5b, 0xe0, 0x65, 0x00, 0x70, 0x00, 0x65, 0x00, 0x38, 0xe0, 0x38, 0xe0, 0x5d, 0xe0, 0x2b, 0x00, 0x7d, 0x00, 0x29, 0x00, 0x2b, 0x00, 0x00, 0x00, 0x00, 0x00)

  # レジストリに値を設定
  Set-ItemProperty -Path $registryPath -Name $valueName -Value $scancodeMap -Type Binary

  Write-Host "レジストリの設定が完了しました。" -ForegroundColor Green
  Write-Host ""
  Write-Host "設定を有効にするには再起動が必要です。" -ForegroundColor Yellow
  Write-Host "再起動後、すべてのキー割り当てが変更されます。" -ForegroundColor Yellow

  Write-Host "手動で再起動してください。" -ForegroundColor Yellow
}
catch {
  Write-Error "エラーが発生しました: $($_.Exception.Message)"
  Read-Host "Enterキーを押して終了してください"
  exit 1
}

# 元に戻すためのスクリプト関数
function Show-RevertInstructions {
  Write-Host ""
  Write-Host "=== 設定を元に戻す方法 ===" -ForegroundColor Cyan
  Write-Host "以下のコマンドを管理者権限のPowerShellで実行してください："
  Write-Host "Remove-ItemProperty -Path 'HKLM:\SYSTEM\CurrentControlSet\Control\Keyboard Layout' -Name 'Scancode Map' -ErrorAction SilentlyContinue"
  Write-Host "その後、再起動してください。"
  Write-Host ""
}

Show-RevertInstructions
