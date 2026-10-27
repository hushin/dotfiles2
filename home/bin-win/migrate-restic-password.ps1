# migrate-restic-password.ps1
# 既存の restic リポジトリのパスワードを 1Password 管理のパスワードへ移行する
#
# 旧パスワードは ~/.config/restic/password.txt があればそれを使用し、
# なければ SecureString で対話入力する。
# 各リポジトリで `restic key passwd` を実行し、新パスワードで開けることを確認する。

param(
    [string]$BackupDrive = "E:",
    [string]$ConfigPath = "$env:USERPROFILE\.config\restic",
    [switch]$DryRun
)

# Windows PowerShell 5.1 では、$ErrorActionPreference = 'Stop' の状態で
# ネイティブコマンドの stderr をリダイレクトすると例外になる。
# 失敗判定は各コマンドの $LASTEXITCODE で行うため、ここでは Continue にする。
$ErrorActionPreference = "Continue"

$BackupPath = "$BackupDrive\restic-backup\projects"
$LegacyPasswordFile = "$ConfigPath\password.txt"
$HelperPath = "$ConfigPath\restic-1password.ps1"

if (-not (Test-Path -LiteralPath $HelperPath)) {
    Write-Error "1Password helper not found: $HelperPath (run 'chezmoi apply' first)"
    exit 1
}
. $HelperPath

$repos = @(Get-ChildItem -LiteralPath $BackupPath -Directory -ErrorAction SilentlyContinue |
    Where-Object { Test-Path -LiteralPath (Join-Path $_.FullName "config") })

if ($repos.Count -eq 0) {
    Write-Host "移行対象のリポジトリが見つかりません: $BackupPath"
    exit 0
}

Write-Host "移行対象: $($repos.Count) 件"
foreach ($repo in $repos) {
    Write-Host "  $($repo.FullName)"
}

if ($DryRun) {
    Write-Host "DRY RUN: 変更しません"
    exit 0
}

try {
    Assert-OnePasswordSignedIn
}
catch {
    Write-Error $_.Exception.Message
    exit 1
}

# 新パスワードを先に取得しておき、旧パスワードの入力後に失敗しないようにする
try {
    $NewPassword = Get-ResticPasswordFromOnePassword
}
catch {
    Write-Error $_.Exception.Message
    exit 1
}

# 旧パスワードの取得
$OldPassword = $null
if (Test-Path -LiteralPath $LegacyPasswordFile) {
    $OldPassword = (Get-Content -LiteralPath $LegacyPasswordFile -Raw).TrimEnd("`r", "`n")
    Write-Host "旧パスワードファイルを使用します: $LegacyPasswordFile"
}
else {
    $secure = Read-Host "旧 restic パスワードを入力してください" -AsSecureString
    $bstr = [Runtime.InteropServices.Marshal]::SecureStringToBSTR($secure)
    try {
        $OldPassword = [Runtime.InteropServices.Marshal]::PtrToStringBSTR($bstr)
    }
    finally {
        [Runtime.InteropServices.Marshal]::ZeroFreeBSTR($bstr)
    }
}

if ([string]::IsNullOrEmpty($OldPassword)) {
    Write-Error "旧パスワードが空です"
    exit 1
}

$answer = Read-Host "上記リポジトリのパスワードを 1Password のパスワードへ移行しますか? (y/N)"
if ($answer -notmatch '^[Yy]$') {
    Write-Host "キャンセルしました"
    exit 0
}

# 新パスワードは一時ファイル経由で restic に渡す。使用後は必ず削除する。
$tempPasswordFile = [IO.Path]::GetTempFileName()
try {
    [System.IO.File]::WriteAllText($tempPasswordFile, $NewPassword, [System.Text.UTF8Encoding]::new($false))
    if ($env:OS -eq "Windows_NT") {
        $identity = [System.Security.Principal.WindowsIdentity]::GetCurrent().Name
        & icacls $tempPasswordFile /inheritance:r /grant:r "${identity}:F" | Out-Null
    }

    $failed = 0
    foreach ($repo in $repos) {
        Write-Host ""
        Write-Host "=== $($repo.Name) ==="

        $env:RESTIC_REPOSITORY = $repo.FullName
        $env:RESTIC_PASSWORD = $NewPassword
        Remove-Item Env:\RESTIC_PASSWORD_FILE -ErrorAction SilentlyContinue
        Remove-Item Env:\RESTIC_PASSWORD_COMMAND -ErrorAction SilentlyContinue

        # すでに 1Password のパスワードへ移行済みならスキップ
        $null = & restic cat config 2>$null
        if ($LASTEXITCODE -eq 0) {
            Write-Host "  移行済み (1Password のパスワードで開けます): $($repo.FullName)" -ForegroundColor DarkGray
            continue
        }

        # 旧パスワードで開けることを確認
        $env:RESTIC_PASSWORD = $OldPassword
        $null = & restic cat config 2>$null
        if ($LASTEXITCODE -ne 0) {
            Write-Host "  旧パスワードでも 1Password のパスワードでも開けませんでした。スキップ: $($repo.FullName)" -ForegroundColor Red
            $failed++
            continue
        }

        # restic 側で新キーの検証後に旧キーが削除される
        & restic key passwd --new-password-file $tempPasswordFile
        if ($LASTEXITCODE -ne 0) {
            Write-Host "  パスワード変更に失敗: $($repo.FullName)" -ForegroundColor Red
            $failed++
            continue
        }

        # 新パスワードで開けることを確認
        $env:RESTIC_PASSWORD = $NewPassword
        $null = & restic cat config 2>$null
        if ($LASTEXITCODE -ne 0) {
            Write-Host "  新パスワードでの確認に失敗: $($repo.FullName)" -ForegroundColor Red
            $failed++
            continue
        }

        Write-Host "  移行完了: $($repo.FullName)" -ForegroundColor Green
    }
}
finally {
    Remove-Item -LiteralPath $tempPasswordFile -Force -ErrorAction SilentlyContinue
    Remove-Item Env:\RESTIC_PASSWORD -ErrorAction SilentlyContinue
    Remove-Item Env:\RESTIC_REPOSITORY -ErrorAction SilentlyContinue
}

if ($failed -gt 0) {
    Write-Host ""
    Write-Host "$failed 件のリポジトリで移行に失敗しました。旧 password.txt は残しています。" -ForegroundColor Yellow
    exit 1
}

Write-Host ""
Write-Host "すべてのリポジトリを移行しました。" -ForegroundColor Green
if (Test-Path -LiteralPath $LegacyPasswordFile) {
    $delete = Read-Host "旧 password.txt を削除しますか? (y/N)"
    if ($delete -match '^[Yy]$') {
        Remove-Item -LiteralPath $LegacyPasswordFile -Force
        Write-Host "削除しました: $LegacyPasswordFile"
    }
    else {
        Write-Host "旧 password.txt は残しています: $LegacyPasswordFile"
    }
}
