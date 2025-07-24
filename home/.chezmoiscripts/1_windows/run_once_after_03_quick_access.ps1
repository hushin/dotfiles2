# ディレクトリをクイックアクセスにピン止めするPowerShellスクリプト
function Add-ToQuickAccess {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path
    )

    # Shell.Applicationオブジェクトを作成
    $shell = New-Object -ComObject Shell.Application
    Write-Host "ディレクトリ: $Path" -ForegroundColor Green
    # フォルダオブジェクトを取得
    $folder = $shell.Namespace($Path)
    # フォルダをクイックアクセスにピン止め
    $folder.Self.InvokeVerb("pintohome")
    # COMオブジェクトをクリーンアップ
    [System.Runtime.Interopservices.Marshal]::ReleaseComObject($shell) | Out-Null
    Write-Host "✓ ディレクトリをクイックアクセスにピン止めしました: $Path" -ForegroundColor Green
}

# ホームディレクトリをピン止め
Add-ToQuickAccess -Path $env:USERPROFILE
