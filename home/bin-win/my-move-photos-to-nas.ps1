# my-move-photos-to-nas.ps1
# PCローカルの Pictures\RAW にある撮影日別フォルダ (YYYY-MM-DD) を
# Synology NAS (\\DS218Plus\home\Photos\<year>\) へ移動する
#
# - 移動先フォルダが既に存在する場合はスキップする (上書きしない)
# - robocopy /MOVE により「コピー成功後にのみ元フォルダを削除」するため、
#   コピーに失敗したフォルダはローカルに残る (データ消失を防ぐ)
# - 途中で失敗して移動先に中途半端なフォルダが残った場合、次回実行時は
#   移動先が存在するためスキップされる。ログを確認して手動で解決すること
# - NAS認証はWindows資格情報マネージャーに事前登録済みであること
#   (未登録の場合は `cmdkey /add:DS218Plus /user:ユーザー名 /pass:パスワード`)
#
# 使い方:
#   .\my-move-photos-to-nas.ps1 -DryRun   # 移動せずに対象一覧のみ表示
#   .\my-move-photos-to-nas.ps1           # 確認後に移動

param(
    [switch]$DryRun
)

$SourcePath = "$env:USERPROFILE\Pictures\RAW"
$NasDestRoot = "\\DS218Plus\home\Photos"
$LogPath = "$env:USERPROFILE\.logs\photo-move"
$LogFile = "$LogPath\move-$(Get-Date -Format 'yyyy-MM-dd').log"

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

if ($DryRun) {
    Write-Log "=== 写真NAS移動 (ドライラン: 実際には移動しません) ==="
} else {
    Write-Log "=== 写真NAS移動開始 ==="
}

if (-not (Test-Path $SourcePath)) {
    Write-Log "ERROR: コピー元が見つかりません: $SourcePath"
    exit 1
}

if (-not (Test-Path $NasDestRoot)) {
    Write-Log "ERROR: NASに接続できません: $NasDestRoot (NASの電源/ネットワーク/資格情報を確認してください)"
    exit 1
}

# 撮影日別フォルダ (YYYY-MM-DD) のみを対象とする
$targetFolders = Get-ChildItem -Path $SourcePath -Directory |
    Where-Object { $_.Name -match '^\d{4}-\d{2}-\d{2}$' } |
    Sort-Object Name

# 対象外 (ファイルや命名規則外のフォルダ) があれば警告する
$others = Get-ChildItem -Path $SourcePath |
    Where-Object { -not ($_.PSIsContainer -and $_.Name -match '^\d{4}-\d{2}-\d{2}$') }
foreach ($item in $others) {
    Write-Log "WARNING: 命名規則外のためスキップ: $($item.Name)"
}

if ($targetFolders.Count -eq 0) {
    Write-Log "移動対象のフォルダが見つかりませんでした"
    exit 0
}

# 移動先が既に存在するフォルダはスキップ対象に分ける
$skipList = @()
$moveList = @()
foreach ($folder in $targetFolders) {
    $year = $folder.Name.Substring(0, 4)
    $destDir = Join-Path (Join-Path $NasDestRoot $year) $folder.Name
    if (Test-Path $destDir) {
        $skipList += [pscustomobject]@{ Name = $folder.Name; Dest = $destDir }
    } else {
        $sizeMB = [math]::Round(((Get-ChildItem -Path $folder.FullName -Recurse -File -ErrorAction SilentlyContinue | Measure-Object Length -Sum).Sum / 1MB), 1)
        $moveList += [pscustomobject]@{ Name = $folder.Name; Dest = $destDir; FullPath = $folder.FullName; SizeMB = $sizeMB }
    }
}

Write-Host ""
Write-Host "=== 移動対象 ($($moveList.Count) 件) ==="
foreach ($item in $moveList) {
    Write-Host ("  {0}  ({1:N1} GB)  ->  {2}" -f $item.Name, ($item.SizeMB / 1024), $item.Dest)
}
if ($skipList.Count -gt 0) {
    Write-Host ""
    Write-Host "=== スキップ (移動先が既に存在) ($($skipList.Count) 件) ==="
    foreach ($item in $skipList) {
        Write-Host "  $($item.Name)  ->  $($item.Dest)"
    }
}

if ($DryRun) {
    Write-Log "ドライラン終了: 移動 $($moveList.Count) 件 / スキップ(既存) $($skipList.Count) 件"
    exit 0
}

if ($moveList.Count -eq 0) {
    Write-Log "移動対象がありませんでした"
    exit 0
}

$answer = Read-Host "上記のフォルダをNASへ移動しますか? (y/N)"
if ($answer -ne 'y' -and $answer -ne 'Y') {
    Write-Log "移動はキャンセルされました"
    exit 0
}

$moved = 0
$failed = 0
$totalCount = $moveList.Count
$index = 0

foreach ($item in $moveList) {
    $index++
    Write-Progress -Activity "写真をNASへ移動中" -Status "$index / $totalCount : $($item.Name)" -PercentComplete ([int](($index / $totalCount) * 100))

    Write-Log "移動中: $($item.Name) -> $($item.Dest)"
    robocopy $item.FullPath $item.Dest /MOVE /E /Z /R:2 /W:5 /MT:8 /NP /LOG+:$LogFile
    if ($LASTEXITCODE -lt 8) {
        $moved++
        Write-Log "OK: $($item.Name) を移動しました"
    } else {
        $failed++
        Write-Log "ERROR: $($item.Name) の移動に失敗しました (robocopy ExitCode: $LASTEXITCODE)"
    }
}

Write-Progress -Activity "写真をNASへ移動中" -Completed
Write-Log "=== 完了: 移動 $moved 件 / 失敗 $failed 件 / スキップ(既存) $($skipList.Count) 件 ==="

if ($failed -gt 0) {
    exit 1
}