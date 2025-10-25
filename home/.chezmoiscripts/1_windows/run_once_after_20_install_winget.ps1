$packages = @(
    "AutoHotkey.AutoHotkey",
    "AgileBits.1Password",
    "AgileBits.1Password.CLI",
    "Google.Chrome",
    "Google.JapaneseIME",
    "Microsoft.VisualStudioCode",
    "Microsoft.PowerShell",
    "Microsoft.PowerToys",
    "Dropbox.Dropbox",
    "MartiCliment.UniGetUI",
    "jdx.mise",
    "Starship.Starship",
    "rsteube.Carapace",
    "junegunn.fzf",
    "BurntSushi.ripgrep.GNU",
    "eza-community.eza"
)

foreach ($package in $packages) {
    Write-Host "Installing $package..." -ForegroundColor Green
    winget install -e --id $package --source winget
}

winget install 9PM860492SZD # Microsoft PC Manager
