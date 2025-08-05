$packages = @(
    "AutoHotkey.AutoHotkey",
    "AgileBits.1Password",
    "AgileBits.1Password.CLI",
    "Google.Chrome",
    "Google.JapaneseIME",
    "Microsoft.VisualStudioCode",
    "Microsoft.PowerShell",
    "Microsoft.PowerToys",
    # "Microsoft.PCManager",
    "Dropbox.Dropbox",
    "MartiCliment.UniGetUI",
    "jdx.mise",
    "BurntSushi.ripgrep.GNU",
    "sharkdp.fd",
    "Starship.Starship",
    "rsteube.Carapace",
    "junegunn.fzf",
    "eza-community.eza"
)

foreach ($package in $packages) {
    Write-Host "Installing $package..." -ForegroundColor Green
    winget install -e --id $package --source winget
}
