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
    Write-Host "Installing $package..." -ForegroundColor Green
    winget install -e --id $package --source winget
}


