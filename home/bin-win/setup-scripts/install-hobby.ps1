$packages = @(
    "LINE.LINE",
    "Discord.Discord",
    "Valve.Steam",
    "MPC-BE.MPC-BE",
    "VideoLAN.VLC",
    "neelabo.NeeView",
    "Philips.HueSync"
)

foreach ($package in $packages) {
    winget install -e --id $package --source winget
}
