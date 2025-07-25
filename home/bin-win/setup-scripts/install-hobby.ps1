$packages = @(
    "LINE.LINE",
    "Discord.Discord",
    "Valve.Steam",
    "MPC-BE.MPC-BE",
    "neelabo.NeeView",
    "VideoLAN.VLC"
)

foreach ($package in $packages) {
    winget install -e --id $package --source winget
}
