# backup-photos-to-nas.ps1
# PCローカルのRAWフォルダをSynology NASへrobocopy /MIRで差分バックアップする
# NASは常時起動していない前提のため、週末など手動実行を想定
#
# NAS認証はWindows資格情報マネージャーに事前登録済みであること
# (未登録の場合は `cmdkey /add:NAS名 /user:ユーザー名 /pass:パスワード` 等で登録)

$SourcePath = "$env:USERPROFILE\Pictures\RAW"
$NasPath = "\\NAS\photos" # TODO: 実際のNAS共有パスに書き換える
$LogPath = "$env:USERPROFILE\.logs\photo-backup"
$LogFile = "$LogPath\backup-$(Get-Date -Format 'yyyy-MM-dd').log"

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

Write-Log "=== NASバックアップ開始 ==="

if (-not (Test-Path $SourcePath)) {
    Write-Log "ERROR: コピー元が見つかりません: $SourcePath"
    exit 1
}

# コピー元が空/取得失敗のまま /MIR を実行するとNAS側のファイルが削除されるため、事前に安全確認する
$sourceFileCount = (Get-ChildItem -Path $SourcePath -Recurse -File -ErrorAction SilentlyContinue | Measure-Object).Count
if ($sourceFileCount -eq 0) {
    Write-Log "ERROR: コピー元にファイルが1件もありません。/MIR によるNAS側データ消失を防ぐため中断します: $SourcePath"
    exit 1
}

if (-not (Test-Path $NasPath)) {
    Write-Log "ERROR: NASに接続できません: $NasPath (NASの電源/ネットワーク/資格情報を確認してください)"
    exit 1
}

Write-Log "コピー元: $SourcePath ($sourceFileCount 件)"
Write-Log "コピー先: $NasPath"

robocopy $SourcePath $NasPath /MIR /Z /R:2 /W:5 /MT:8 /NP /LOG+:$LogFile

$ExitCode = $LASTEXITCODE
if ($ExitCode -ge 8) {
    Write-Log "ERROR: robocopyが失敗しました (ExitCode: $ExitCode)"
    exit 1
}

Write-Log "=== NASバックアップ完了 (ExitCode: $ExitCode) ==="
