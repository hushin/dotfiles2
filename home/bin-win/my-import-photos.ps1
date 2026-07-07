# import-photos.ps1
# SDカード(F:\DCIM)のRAW/JPEGを撮影日別フォルダに整理しつつ、
# PCローカルと外付けSSD(E:)の2箇所へ取り込む

$SdCardPath = "F:\DCIM"
$PcDestRoot = "$env:USERPROFILE\Pictures\RAW"
$SsdDestRoot = "E:\Photos"
$ImagingEdgeExe = "C:\Program Files\Sony\Imaging Edge\Viewer.exe"
$LogPath = "$env:USERPROFILE\.logs\photo-import"
$LogFile = "$LogPath\import-$(Get-Date -Format 'yyyy-MM-dd').log"

function Write-Log {
    param([string]$Message)
    $Timestamp = Get-Date -Format 'yyyy-MM-dd HH:mm:ss'
    $LogEntry = "[$Timestamp] $Message"
    Write-Host $LogEntry
    $LogEntry | Out-File -FilePath $LogFile -Append -Encoding UTF8
}

if (-not (Test-Path $LogPath)) {
    New-Item -ItemType Directory -Path $LogPath -Force | Out-Null
}

Write-Log "=== 写真取り込み開始 ==="

if (-not (Test-Path $SdCardPath)) {
    Write-Log "ERROR: SDカードが見つかりません: $SdCardPath"
    exit 1
}

try {
    $null = & exiftool -ver 2>$null
} catch {
    Write-Log "ERROR: exiftoolが見つかりません。'winget install OliverBetz.ExifTool' でインストールしてください"
    exit 1
}

Write-Log "撮影日を取得中: $SdCardPath"
$exifJson = & exiftool -r -j -DateTimeOriginal -d "%Y-%m-%d" $SdCardPath 2>$null
$exifData = $exifJson | ConvertFrom-Json

if (-not $exifData -or $exifData.Count -eq 0) {
    Write-Log "WARNING: 取り込み対象のファイルが見つかりませんでした"
    exit 0
}

Write-Log "$($exifData.Count) 件のファイルを検出しました"

$copiedPc = 0
$skippedPc = 0
$copiedSsd = 0
$skippedSsd = 0
$copiedBytes = 0
$totalCount = $exifData.Count
$index = 0
$copyStartTime = Get-Date

foreach ($entry in $exifData) {
    $index++
    $sourceFile = $entry.SourceFile
    $shotDate = $entry.DateTimeOriginal
    if (-not $shotDate) {
        $shotDate = (Get-Item $sourceFile).LastWriteTime.ToString("yyyy-MM-dd")
    }
    $fileName = Split-Path $sourceFile -Leaf

    $pcDestDir = Join-Path $PcDestRoot $shotDate
    $ssdDestDir = Join-Path $SsdDestRoot $shotDate
    if (-not (Test-Path $pcDestDir)) { New-Item -ItemType Directory -Path $pcDestDir -Force | Out-Null }
    if (-not (Test-Path $ssdDestDir)) { New-Item -ItemType Directory -Path $ssdDestDir -Force | Out-Null }

    $pcDestFile = Join-Path $pcDestDir $fileName
    $ssdDestFile = Join-Path $ssdDestDir $fileName

    Write-Progress -Activity "写真取り込み中" -Status "$index / $totalCount : $fileName" -PercentComplete ([int](($index / $totalCount) * 100))

    if (Test-Path $pcDestFile) {
        $skippedPc++
    } else {
        Copy-Item -Path $sourceFile -Destination $pcDestFile -Force
        $copiedPc++
        $copiedBytes += (Get-Item $pcDestFile).Length
    }

    if (Test-Path $ssdDestFile) {
        $skippedSsd++
    } else {
        # SDカードへの読み直しを避けるため、既にコピー済みならPC側からコピーする
        $copySource = if (Test-Path $pcDestFile) { $pcDestFile } else { $sourceFile }
        Copy-Item -Path $copySource -Destination $ssdDestFile -Force
        $copiedSsd++
    }

    if ($index % 20 -eq 0 -or $index -eq $totalCount) {
        $elapsed = (Get-Date) - $copyStartTime
        $mbPerSec = if ($elapsed.TotalSeconds -gt 0) { [math]::Round(($copiedBytes / 1MB) / $elapsed.TotalSeconds, 1) } else { 0 }
        Write-Log ("進捗: {0}/{1} 件 (経過 {2:mm\:ss}, 平均 {3} MB/s)" -f $index, $totalCount, $elapsed, $mbPerSec)
    }
}

Write-Progress -Activity "写真取り込み中" -Completed
Write-Log "PC ($PcDestRoot): $copiedPc 件コピー, $skippedPc 件スキップ (既存)"
Write-Log "SSD ($SsdDestRoot): $copiedSsd 件コピー, $skippedSsd 件スキップ (既存)"
Write-Log "=== 写真取り込み完了 ==="

if (Test-Path $ImagingEdgeExe) {
    Write-Log "Imaging Edge Desktop (Viewer) を起動します"
    Start-Process -FilePath $ImagingEdgeExe
} else {
    Write-Log "WARNING: Imaging Edge Desktopが見つかりません: $ImagingEdgeExe"
}
