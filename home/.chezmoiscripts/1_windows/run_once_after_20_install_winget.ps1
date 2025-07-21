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
  "Starship.Starship",
  "rsteube.Carapace"
)

foreach ($package in $packages) {
  winget install -e --id $package --source winget
}
