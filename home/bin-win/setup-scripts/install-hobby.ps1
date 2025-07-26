$packages = @(
    "Discord.Discord",
    "Amazon.Kindle",
    "Valve.Steam",
    "MPC-BE.MPC-BE",
    "VideoLAN.VLC",
    "neelabo.NeeView",
    "Philips.HueSync"
)

winget install -e --id XPFCC4CD725961 # LINE Desktop

foreach ($package in $packages) {
    winget install -e --id $package --source winget
}
