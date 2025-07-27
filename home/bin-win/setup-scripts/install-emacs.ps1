# Self-elevate the script if required
if (-Not ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole] 'Administrator')) {
    if ([int](Get-CimInstance -Class Win32_OperatingSystem | Select-Object -ExpandProperty BuildNumber) -ge 6000) {
        $CommandLine = "-NoExit -File `"" + $MyInvocation.MyCommand.Path + "`" " + $MyInvocation.UnboundArguments
        Start-Process -Wait -FilePath PowerShell.exe -Verb Runas -ArgumentList $CommandLine
        Exit
    }
}

winget install -e --id GNU.Emacs --source winget

function Add-ToUserPath {
    param(
        [string]$Path
    )

    $currentPath = [Environment]::GetEnvironmentVariable("PATH", "User")
    if ($currentPath -notlike "*$Path*") {
        [Environment]::SetEnvironmentVariable("PATH", "$currentPath;$Path", "User")
        Write-Output "PATHに追加しました: $Path"
    }
    else {
        Write-Output "PATHに既に存在します: $Path"
    }
}

function Get-LatestEmacsPath {
    param(
        [string]$SubPath = ""
    )

    $emacsBasePath = "$env:ProgramFiles\Emacs"
    if (Test-Path $emacsBasePath) {
        $latestEmacsDir = Get-ChildItem -Path $emacsBasePath -Directory |
        Sort-Object Name -Descending |
        Select-Object -First 1

        if ($latestEmacsDir) {
            if ($SubPath) {
                return Join-Path $latestEmacsDir.FullName $SubPath
            }
            else {
                return $latestEmacsDir.FullName
            }
        }
    }
    return $null
}


# Add Emacs bin to PATH
Write-Output "Emacs binディレクトリをPATHに追加"
$emacsBinPath = Get-LatestEmacsPath -SubPath "bin"
if ($emacsBinPath -and (Test-Path $emacsBinPath)) {
    Add-ToUserPath -Path $emacsBinPath
}

# Add .config/emacs/bin to PATH
$emacsDotdBinPath = "$env:USERPROFILE\.config\emacs\bin"
if (Test-Path $emacsDotdBinPath) {
    Add-ToUserPath -Path $emacsDotdBinPath
}

function Update-PathVariable {
    $env:Path = [System.Environment]::GetEnvironmentVariable("Path", "Machine") + ";" +
    [System.Environment]::GetEnvironmentVariable("Path", "User")
}
Update-PathVariable

# Install Doom Emacs
Write-Output "Doom Emacsのインストール"
doom install
Write-Output "Doom Emacsのsync"
doom sync

# org-protocol
Write-Output "org-protocol: レジストリに登録"
$emacsClientPath = Get-LatestEmacsPath -SubPath "bin\emacsclientw.exe"
if ($emacsClientPath -and (Test-Path $emacsClientPath)) {
    New-PSDrive -Name HKCR -PSProvider Registry -Root HKEY_CLASSES_ROOT
    New-Item 'HKCR:\org-protocol\shell\open\command' -Force
    Set-ItemProperty -Path 'HKCR:\org-protocol' -name '(default)' -Value 'URL:Org Protocol'
    Set-ItemProperty -Path 'HKCR:\org-protocol' -name 'URL Protocol' -Value ''
    Set-ItemProperty -Path 'HKCR:\org-protocol\shell\open\command' -name '(default)' -Value "`"$emacsClientPath`" `"%1`""
    Write-Output "org-protocolを登録しました: $emacsClientPath"
}
else {
    Write-Warning "emacsclientw.exeが見つかりません"
}
