$packages = @(
    "Adobe.Acrobat.Reader.64-bit",
    "Mozilla.Firefox",
    "Zoom.Zoom",
    "TheDocumentFoundation.LibreOffice",
    "DevToys-app.DevToys",
    "JohnMacFarlane.Pandoc",
    "WinDirStat.WinDirStat"
)

foreach ($package in $packages) {
    winget install -e --id $package --source winget
}

