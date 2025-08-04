$packages = @(
    "Discord.Discord",
    "Amazon.Kindle",
    "Valve.Steam",
    "MPC-BE.MPC-BE",
    "VideoLAN.VLC",
    "neelabo.NeeView",
    "Philips.HueSync"
)

Write-Host "Installing LINE Desktop..." -ForegroundColor Green
winget install -e --id XPFCC4CD725961 # LINE Desktop

foreach ($package in $packages) {
    Write-Host "Installing $package..." -ForegroundColor Green
    winget install -e --id $package --source winget
}
