# my-export-photos.ps1
# RAWフォルダ内の撮影日フォルダを fzf で選択し、共有用にリサイズして exported フォルダへ書き出す
#
# - 長辺 1920px
# - JPEG 品質 90 (System.Drawing / GDI+ の Quality パラメータで正確に指定)
# - EXIF メタデータは残さない (共有用のため位置情報などの漏えいを防ぐ)
# - EXIF Orientation は手動で回転して反映 (GDI+ は自動回転しないため)
# - PNG の透過部分は白で塗りつぶす
# - RAW (ARW等) は同一名の JPEG がある場合はその JPEG を変換し、無い場合は
#   埋め込みプレビュー (exiftool) から変換する
# - 動画 (MP4等) は変換対象外
# - HEIC/HEIF は Windows の HEIF コーデック (Microsoft Store の HEIF Image Extensions)
#   がインストールされていれば変換可能

[CmdletBinding()]
param(
    [switch]$Force,   # 既に存在する出力ファイルを上書きする
    [switch]$NoOpen   # 変換後にエクスプローラーを開かない
)

$ErrorActionPreference = 'Stop'
Add-Type -AssemblyName System.Drawing

$HomePath    = $env:HOME                    # %HOME% (= C:\Users\n1)
$SourceRoot  = Join-Path $HomePath 'Pictures\RAW'
$DestRoot    = Join-Path $HomePath 'Pictures\exported'
$MaxEdge     = 1920                         # 長辺の上限 (px)
$JpegQuality = 90                           # JPEG 品質

$ImageExtensions = @('.jpg', '.jpeg', '.png', '.tif', '.tiff', '.heic', '.heif', '.arw', '.cr2', '.cr3', '.nef')
$RawExtensions   = @('.arw', '.cr2', '.cr3', '.nef')
$VideoExtensions = @('.mp4', '.mov', '.mts')

# --- 前提ツールの確認 ---
$missing = @('fzf', 'exiftool') | Where-Object { -not (Get-Command $_ -ErrorAction SilentlyContinue) }
if ($missing) {
    Write-Host "ERROR: 次のツールが見つかりません: $($missing -join ', ')" -ForegroundColor Red
    Write-Host '  fzf     : winget install junegunn.fzf' -ForegroundColor Yellow
    Write-Host '  exiftool: winget install OliverBetz.ExifTool' -ForegroundColor Yellow
    exit 1
}

if (-not (Test-Path -LiteralPath $SourceRoot)) {
    Write-Host "ERROR: フォルダが見つかりません: $SourceRoot" -ForegroundColor Red
    exit 1
}

# --- 撮影日フォルダの選択 (fzf) ---
# フォルダ名が日付 (YYYY-MM-DD) のため降順 = 新しい順
$folders = Get-ChildItem -LiteralPath $SourceRoot -Directory | Sort-Object Name -Descending | Select-Object -ExpandProperty Name
if (-not $folders) {
    Write-Host "ERROR: $SourceRoot にフォルダがありません" -ForegroundColor Red
    exit 1
}
$selected = $folders | fzf --prompt "写真フォルダを選択: " --height=40% --reverse
if (-not $selected) {
    Write-Host '選択がキャンセルされました' -ForegroundColor Yellow
    exit 0
}

$srcDir  = Join-Path $SourceRoot $selected
$destDir = Join-Path $DestRoot $selected
New-Item -ItemType Directory -Path $destDir -Force | Out-Null

# --- 対象ファイルの選定 ---
# JPEG はそのまま変換対象。RAW は同一名の JPEG があればスキップ (JPEG がフル解像度の現像結果のため)
$allFiles = Get-ChildItem -LiteralPath $srcDir -File | Where-Object { $_.Extension -in $ImageExtensions }
if (-not $allFiles) {
    Write-Host "選択したフォルダに変換対象の画像がありません: $srcDir" -ForegroundColor Yellow
    exit 0
}
$jpgStems = @($allFiles | Where-Object { $_.Extension.ToLowerInvariant() -notin $RawExtensions } |
    ForEach-Object { $_.BaseName.ToLowerInvariant() })
$targets = @($allFiles | Where-Object {
    if ($_.Extension.ToLowerInvariant() -in $RawExtensions) {
        $_.BaseName.ToLowerInvariant() -notin $jpgStems   # RAW はペア JPEG が無いものだけ
    } else {
        $true
    }
})

$videos = @(Get-ChildItem -LiteralPath $srcDir -File | Where-Object { $_.Extension -in $VideoExtensions })
if ($videos) {
    Write-Host "動画は変換対象外です (スキップ: $($videos.Count) 件)" -ForegroundColor DarkGray
}

Write-Host "変換元: $srcDir" -ForegroundColor Cyan
Write-Host "変換先: $destDir" -ForegroundColor Cyan
Write-Host "対象: $($targets.Count) 件 (長辺 $MaxEdge px, JPEG 品質 $JpegQuality)" -ForegroundColor Cyan

# --- 変換処理 ---
# 1枚の画像をリサイズして JPEG で書き出す (EXIF Orientation を反映、メタデータは残さない)
function Convert-Photo {
    param(
        [string]$Source,
        [string]$Output,
        [int]$MaxEdge,
        [int]$Quality
    )

    $img = $null
    try {
        $img = [System.Drawing.Image]::FromFile($Source)

        # EXIF Orientation を反映 (1: 通常 / 3: 180° / 6: 90°CW / 8: 270°CW)
        if ($img.PropertyIdList -contains 0x0112) {
            switch ($img.GetPropertyItem(0x0112).Value[0]) {
                3 { $img.RotateFlip([System.Drawing.RotateFlipType]::Rotate180FlipNone) }
                6 { $img.RotateFlip([System.Drawing.RotateFlipType]::Rotate90FlipNone) }
                8 { $img.RotateFlip([System.Drawing.RotateFlipType]::Rotate270FlipNone) }
            }
        }

        # 長辺が MaxEdge を超える場合のみ縮小 (小さい画像は拡大しない)
        $newW = $img.Width
        $newH = $img.Height
        $longEdge = [Math]::Max($newW, $newH)
        if ($longEdge -gt $MaxEdge) {
            $scale = $MaxEdge / $longEdge
            $newW = [Math]::Max(1, [int][Math]::Round($newW * $scale))
            $newH = [Math]::Max(1, [int][Math]::Round($newH * $scale))
        }

        $bmp = New-Object System.Drawing.Bitmap($newW, $newH)
        try {
            $g = [System.Drawing.Graphics]::FromImage($bmp)
            try {
                $g.Clear([System.Drawing.Color]::White)   # PNG 透過部分は白で塗りつぶす
                $g.InterpolationMode = [System.Drawing.Drawing2D.InterpolationMode]::HighQualityBicubic
                $g.SmoothingMode     = [System.Drawing.Drawing2D.SmoothingMode]::HighQuality
                $g.PixelOffsetMode   = [System.Drawing.Drawing2D.PixelOffsetMode]::HighQuality
                $g.DrawImage($img, 0, 0, $newW, $newH)
            } finally {
                $g.Dispose()
            }

            $encoder = [System.Drawing.Imaging.ImageCodecInfo]::GetImageEncoders() |
                Where-Object { $_.MimeType -eq 'image/jpeg' }
            $ep = New-Object System.Drawing.Imaging.EncoderParameters(1)
            $ep.Param[0] = New-Object System.Drawing.Imaging.EncoderParameter(
                [System.Drawing.Imaging.Encoder]::Quality, [long]$Quality)
            $bmp.Save($Output, $encoder, $ep)
        } finally {
            $bmp.Dispose()
        }
    } finally {
        if ($img) { $img.Dispose() }
    }
}

$converted = 0
$skipped   = 0
$failed    = 0
$index     = 0

foreach ($file in $targets) {
    $index++
    $outFile = Join-Path $destDir ([IO.Path]::ChangeExtension($file.Name, '.jpg'))

    if ((Test-Path -LiteralPath $outFile) -and -not $Force) {
        $skipped++
        Write-Progress -Activity '写真をエクスポート中' -Status "$index / $($targets.Count): $($file.Name) (既存のためスキップ)" -PercentComplete (($index / $targets.Count) * 100)
        continue
    }

    Write-Progress -Activity '写真をエクスポート中' -Status "$index / $($targets.Count): $($file.Name)" -PercentComplete (($index / $targets.Count) * 100)

    $workFile   = $file.FullName
    $tmpPreview = $null
    try {
        if ($file.Extension.ToLowerInvariant() -in $RawExtensions) {
            # RAW から埋め込みプレビューを抽出
            # (PowerShell のリダイレクトはバイナリを破壊するため cmd 経由で書き出す)
            $tmpPreview = Join-Path ([IO.Path]::GetTempPath()) ("preview-" + [guid]::NewGuid().ToString('N') + '.jpg')
            cmd.exe /d /c "exiftool -b -PreviewImage `"$($file.FullName)`" > `"$tmpPreview`""
            if ($LASTEXITCODE -ne 0 -or -not (Test-Path -LiteralPath $tmpPreview)) {
                throw "RAW プレビュー抽出に失敗: $($file.Name)"
            }
            $workFile = $tmpPreview
        }

        Convert-Photo -Source $workFile -Output $outFile -MaxEdge $MaxEdge -Quality $JpegQuality
        $converted++
    } catch {
        $failed++
        Write-Host "WARNING: 変換失敗: $($file.Name) - $($_.Exception.Message)" -ForegroundColor Yellow
    } finally {
        if ($tmpPreview) {
            Remove-Item -LiteralPath $tmpPreview -Force -ErrorAction SilentlyContinue
        }
    }
}
Write-Progress -Activity '写真をエクスポート中' -Completed

Write-Host ''
Write-Host "=== 完了: $converted 件変換 / $skipped 件スキップ / $failed 件失敗 ===" -ForegroundColor $(if ($failed) { 'Yellow' } else { 'Green' })

if (-not $NoOpen -and (Test-Path -LiteralPath $destDir)) {
    Start-Process explorer.exe $destDir
}
