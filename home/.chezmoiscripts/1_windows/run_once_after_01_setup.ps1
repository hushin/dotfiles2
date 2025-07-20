# Self-elevate the script if required
if (-Not ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole] 'Administrator')) {
  if ([int](Get-CimInstance -Class Win32_OperatingSystem | Select-Object -ExpandProperty BuildNumber) -ge 6000) {
    $CommandLine = "-NoExit -File `"" + $MyInvocation.MyCommand.Path + "`" " + $MyInvocation.UnboundArguments
    Start-Process -Wait -FilePath PowerShell.exe -Verb Runas -ArgumentList $CommandLine
    Exit
  }
}

Write-Output "Microsoft Defender: 除外設定を追加"
Add-MpPreference -ExclusionPath $env:USERPROFILE\.config
Add-MpPreference -ExclusionPath $env:USERPROFILE\bin
Add-MpPreference -ExclusionPath $env:USERPROFILE\ghq

Write-Output "ロングファイルパスを有効化"
Set-ItemProperty "HKLM:\SYSTEM\CurrentControlSet\Control\FileSystem" -Name "LongPathsEnabled" -Value 1

Write-Output "エクスプローラー: ファイル名拡張子: 表示"
Set-ItemProperty HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced -name "HideFileExt" -Value 0

Write-Output "クイックアクセス: ホームディレクトリをピン留め"

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

# org-protocol
Write-Output "org-protocol: レジストリに登録"
New-PSDrive -Name HKCR -PSProvider Registry -Root HKEY_CLASSES_ROOT
New-Item 'HKCR:\org-protocol\shell\open\command' -Force
Set-ItemProperty -Path 'HKCR:\org-protocol' -name '(default)' -Value 'URL:Org Protocol'
Set-ItemProperty -Path 'HKCR:\org-protocol' -name 'URL Protocol' -Value ''
Set-ItemProperty -Path 'HKCR:\org-protocol\shell\open\command' -name '(default)' -Value "`"$env:USERPROFILE\scoop\apps\emacs\current\bin\emacsclientw.exe`" `"%1`""
