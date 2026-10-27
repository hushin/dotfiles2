# sync-restic-password.ps1
# 1Password の restic パスワードを DPAPI キャッシュへ反映する。
#
# 1Password 側でパスワードを変更したときだけ実行すればよい。
# 通常のバックアップはキャッシュだけを使うため、認証ダイアログは表示されない。

param(
    [string]$Reference,
    [switch]$Clear
)

# Windows PowerShell 5.1 でネイティブコマンドの stderr を扱うため Continue にする。
$ErrorActionPreference = "Continue"

$ConfigPath = "$env:USERPROFILE\.config\restic"
$HelperPath = "$ConfigPath\restic-1password.ps1"

if (-not (Test-Path -LiteralPath $HelperPath)) {
    Write-Error "1Password helper not found: $HelperPath (run 'chezmoi apply' first)"
    exit 1
}
. $HelperPath

if ($Clear) {
    $cachePath = Clear-ResticPasswordCache
    Write-Host "キャッシュを削除しました: $cachePath"
    Write-Host "次回のバックアップ時に 1Password から再取得します。"
    exit 0
}

if ([string]::IsNullOrWhiteSpace($Reference)) {
    $Reference = Get-ResticOnePasswordRef
}
if ([string]::IsNullOrWhiteSpace($Reference)) {
    Write-Error "1Password の参照が設定されていません。setup-project-backup.ps1 を先に実行してください。"
    exit 1
}

Write-Host "1Password からパスワードを取得します: $Reference"
Write-Host "（1Password の認証ダイアログが表示された場合は許可してください）"

try {
    $password = Get-ResticPasswordFromOnePassword -Reference $Reference
    $cacheFile = Set-ResticPasswordCache -Password $password
}
catch {
    Write-Error $_.Exception.Message
    exit 1
}

Write-Host "パスワードキャッシュを更新しました: $cacheFile"
Write-Host "次回のバックアップから 1Password の認証ダイアログは表示されません。"
