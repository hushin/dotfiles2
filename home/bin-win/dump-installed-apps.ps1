# インストール済みアプリ一覧を出力するスクリプト

$OutputFile = "$HOME\installed-apps.csv"

# CSVヘッダーを出力
"アプリ名,バージョン,発行者,種別" | Out-File -FilePath $OutputFile -Encoding UTF8


try {
    # Microsoft Store アプリの取得
    $StoreApps = Get-AppxPackage | Select-Object Name, Version, Publisher | Sort-Object Name
    foreach ($app in $StoreApps) {
        "`"$($app.Name)`",`"$($app.Version)`",`"$($app.Publisher)`",`"Store`"" | Out-File -FilePath $OutputFile -Append -Encoding UTF8
    }

    # デスクトップアプリケーションの取得

    # レジストリから取得（より高速で確実）
    $DesktopApps = @()

    # 64bit アプリ
    $Apps64 = Get-ItemProperty HKLM:\Software\Microsoft\Windows\CurrentVersion\Uninstall\* -ErrorAction SilentlyContinue |
    Where-Object { $_.DisplayName -ne $null -and $_.DisplayName -ne "" } |
    Select-Object DisplayName, DisplayVersion, Publisher

    # 32bit アプリ（64bit Windowsの場合）
    if ([Environment]::Is64BitOperatingSystem) {
        $Apps32 = Get-ItemProperty HKLM:\Software\WOW6432Node\Microsoft\Windows\CurrentVersion\Uninstall\* -ErrorAction SilentlyContinue |
        Where-Object { $_.DisplayName -ne $null -and $_.DisplayName -ne "" } |
        Select-Object DisplayName, DisplayVersion, Publisher
        $DesktopApps = ($Apps64 + $Apps32) | Sort-Object DisplayName -Unique
    }
    else {
        $DesktopApps = $Apps64 | Sort-Object DisplayName
    }

    # ユーザー固有のアプリも取得
    $UserApps = Get-ItemProperty HKCU:\Software\Microsoft\Windows\CurrentVersion\Uninstall\* -ErrorAction SilentlyContinue |
    Where-Object { $_.DisplayName -ne $null -and $_.DisplayName -ne "" } |
    Select-Object DisplayName, DisplayVersion, Publisher

    $DesktopApps = ($DesktopApps + $UserApps) | Sort-Object DisplayName -Unique

    foreach ($app in $DesktopApps) {
        $version = if ($app.DisplayVersion) { $app.DisplayVersion } else { "" }
        $publisher = if ($app.Publisher) { $app.Publisher } else { "" }
        "`"$($app.DisplayName)`",`"$version`",`"$publisher`",`"Desktop`"" | Out-File -FilePath $OutputFile -Append -Encoding UTF8
    }

    Write-Host "完了！ファイルが保存されました: $OutputFile" -ForegroundColor Green

    # ファイルを開くかの確認
    $choice = Read-Host "ファイルを開きますか？ (y/n)"
    if ($choice -eq 'y' -or $choice -eq 'Y') {
        Start-Process notepad.exe -ArgumentList $OutputFile
    }

}
catch {
    Write-Host "エラーが発生しました: $($_.Exception.Message)" -ForegroundColor Red
    "エラー: $($_.Exception.Message)" | Out-File -FilePath $OutputFile -Append -Encoding UTF8
}
