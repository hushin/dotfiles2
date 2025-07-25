# Self-elevate the script if required
if (-Not ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole] 'Administrator')) {
    if ([int](Get-CimInstance -Class Win32_OperatingSystem | Select-Object -ExpandProperty BuildNumber) -ge 6000) {
        $CommandLine = "-NoExit -File `"" + $MyInvocation.MyCommand.Path + "`" " + $MyInvocation.UnboundArguments
        Start-Process -Wait -FilePath PowerShell.exe -Verb Runas -ArgumentList $CommandLine
        Exit
    }
}

# org-protocol
Write-Output "org-protocol: レジストリに登録"
New-PSDrive -Name HKCR -PSProvider Registry -Root HKEY_CLASSES_ROOT
New-Item 'HKCR:\org-protocol\shell\open\command' -Force
Set-ItemProperty -Path 'HKCR:\org-protocol' -name '(default)' -Value 'URL:Org Protocol'
Set-ItemProperty -Path 'HKCR:\org-protocol' -name 'URL Protocol' -Value ''
Set-ItemProperty -Path 'HKCR:\org-protocol\shell\open\command' -name '(default)' -Value "`"$env:USERPROFILE\scoop\apps\emacs\current\bin\emacsclientw.exe`" `"%1`""
