$packages = @(
    "Nvidia.CUDA",
    "BlenderFoundation.Blender",
    "Gyan.FFmpeg",
    "OBSProject.OBSStudio",
    "Audacity.Audacity",
    "Logitech.LGS",
    "HiroshibaKazuyuki.VOICEVOX"
)

foreach ($package in $packages) {
    winget install -e --id $package --source winget
}


