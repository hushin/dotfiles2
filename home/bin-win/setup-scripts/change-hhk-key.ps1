# HHK キーボードのキーリマップ変更スクリプト（LWin→F13, RWin→F14）

# Self-elevate the script if required
if (-Not ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole] 'Administrator')) {
    if ([int](Get-CimInstance -Class Win32_OperatingSystem | Select-Object -ExpandProperty BuildNumber) -ge 6000) {
        $CommandLine = "-NoExit -File `"" + $MyInvocation.MyCommand.Path + "`" " + $MyInvocation.UnboundArguments
        Start-Process -Wait -FilePath PowerShell.exe -Verb Runas -ArgumentList $CommandLine
        Exit
    }
}

Write-Host "HHKキーリマップ設定を開始します..." -ForegroundColor Green
Write-Host ""
Write-Host "設定内容:" -ForegroundColor Cyan
Write-Host "  左Win → F13" -ForegroundColor White
Write-Host "  右Win → F14" -ForegroundColor White
Write-Host ""

# 実行確認
Write-Host "この設定を実行しますか？ (y/N): " -NoNewline -ForegroundColor Yellow
$confirmation = Read-Host
if ($confirmation -ne 'y' -and $confirmation -ne 'Y') {
    Write-Host "キャンセルしました。" -ForegroundColor Red
    exit 0
}

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
    # 03,00,00,00 - エントリ数（2つのマッピング + 終了マーカー = 3）
    # 64,00,5b,e0 - Left Win (0xe05b) を F13 (0x64) にマップ
    # 65,00,5c,e0 - Right Win (0xe05c) を F14 (0x65) にマップ
    # 00,00,00,00 - 終了マーカー
    $scancodeMap = [byte[]](0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x64, 0x00, 0x5b, 0xe0, 0x65, 0x00, 0x5c, 0xe0, 0x00, 0x00, 0x00, 0x00)

    # レジストリに値を設定
    Set-ItemProperty -Path $registryPath -Name $valueName -Value $scancodeMap -Type Binary

    Write-Host "レジストリの設定が完了しました。" -ForegroundColor Green
    Write-Host ""
    Write-Host "設定を有効にするには再起動が必要です。" -ForegroundColor Yellow
    Write-Host "再起動後、WinキーがF13/F14として動作します。" -ForegroundColor Yellow

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
