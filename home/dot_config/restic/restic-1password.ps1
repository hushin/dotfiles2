# 1Password CLI 経由で restic のパスワードを扱う共通処理。
# パスワードはディスクに保存せず、restic を実行するプロセスの環境変数にのみ展開する。

function Get-ResticOnePasswordRefFile {
    Join-Path $env:USERPROFILE ".config\restic\1password-ref.txt"
}

# 1Password のアプリ連携は約 10 分で再認証を要求する。30 分ごとに走る
# スケジュールタスクから op を呼ぶと毎回 Windows Hello のダイアログが出るため、
# パスワードは DPAPI (CurrentUser) で暗号化してローカルにキャッシュし、
# バックアップ時は op を呼ばない。
function Get-ResticPasswordCacheFile {
    Join-Path $env:USERPROFILE ".config\restic\password.dpapi"
}

function Get-ResticCachedPassword {
    $cacheFile = Get-ResticPasswordCacheFile
    if (-not (Test-Path -LiteralPath $cacheFile)) {
        return $null
    }

    try {
        Add-Type -AssemblyName System.Security -ErrorAction SilentlyContinue
        $encrypted = [System.IO.File]::ReadAllText($cacheFile, [System.Text.UTF8Encoding]::new($false)).Trim()
        if (-not $encrypted) {
            return $null
        }

        $bytes = [System.Security.Cryptography.ProtectedData]::Unprotect(
            [Convert]::FromBase64String($encrypted),
            $null,
            [System.Security.Cryptography.DataProtectionScope]::CurrentUser)
        return [System.Text.Encoding]::UTF8.GetString($bytes)
    }
    catch {
        Write-Warning "パスワードキャッシュを復号できませんでした (別の Windows ユーザーで作成された可能性): $cacheFile"
        return $null
    }
}

function Set-ResticPasswordCache {
    param([Parameter(Mandatory = $true)][string]$Password)

    Add-Type -AssemblyName System.Security -ErrorAction SilentlyContinue

    $cacheFile = Get-ResticPasswordCacheFile
    $cacheDir = Split-Path -Parent $cacheFile
    if (-not (Test-Path -LiteralPath $cacheDir)) {
        New-Item -ItemType Directory -Path $cacheDir -Force | Out-Null
    }

    $bytes = [System.Text.Encoding]::UTF8.GetBytes($Password)
    $protected = [System.Security.Cryptography.ProtectedData]::Protect(
        $bytes,
        $null,
        [System.Security.Cryptography.DataProtectionScope]::CurrentUser)

    [System.IO.File]::WriteAllText($cacheFile, [Convert]::ToBase64String($protected), [System.Text.UTF8Encoding]::new($false))

    # DPAPI で暗号化済みだが、同じユーザー以外が読めないよう ACL も絞る
    if ($env:OS -eq "Windows_NT") {
        $identity = [System.Security.Principal.WindowsIdentity]::GetCurrent().Name
        & icacls $cacheFile /inheritance:r /grant:r "${identity}:F" | Out-Null
    }

    return $cacheFile
}

function Clear-ResticPasswordCache {
    $cacheFile = Get-ResticPasswordCacheFile
    if (Test-Path -LiteralPath $cacheFile) {
        Remove-Item -LiteralPath $cacheFile -Force
    }
    return $cacheFile
}


function Get-ResticOnePasswordRef {
    if ($env:RESTIC_OP_REF) {
        $envRef = ($env:RESTIC_OP_REF).Trim()
        if ($envRef) {
            return $envRef
        }
    }

    $refFile = Get-ResticOnePasswordRefFile
    if (Test-Path -LiteralPath $refFile) {
        $ref = [System.IO.File]::ReadAllText($refFile, [System.Text.UTF8Encoding]::new($false)).Trim()
        if ($ref) {
            return $ref
        }
    }

    # 参照が未設定の場合は $null を返す。呼び出し側で
    # 「setup-project-backup.ps1 を実行してください」と案内する。
    return $null
}

function Assert-OnePasswordCli {
    if (-not (Get-Command op -ErrorAction SilentlyContinue)) {
        throw "1Password CLI (op) が見つかりません。'winget install AgileBits.1Password.CLI' でインストールし、1Password の設定で CLI 連携を有効にしてください。"
    }
}

# Windows PowerShell 5.1 では、$ErrorActionPreference = 'Stop' の状態で
# ネイティブコマンドの stderr をリダイレクトすると例外になる。
# stderr を捨てる呼び出しだけ、この関数で一時的に Continue に切り替える。
function Invoke-NativeWithContinuePreference {
    param([Parameter(Mandatory = $true)][scriptblock]$ScriptBlock)

    $previous = $ErrorActionPreference
    $ErrorActionPreference = "Continue"
    try {
        & $ScriptBlock
    }
    finally {
        $ErrorActionPreference = $previous
    }
}

# 1Password アプリがロック中 / CLI 連携が一時的に応答しない場合に出るエラー。
# これらは少し待つと成功することがあるため、リトライ対象にする。
function Test-OnePasswordTransientError {
    param([string]$Text)

    if ([string]::IsNullOrWhiteSpace($Text)) {
        return $false
    }

    return $Text -match 'not signed in|1Password is locked|app.*locked|could not connect to 1Password|connection (refused|reset)|timed out|ECONNREFUSED|ETIMEDOUT'
}

function Invoke-OnePasswordCli {
    param(
        [Parameter(Mandatory = $true)][string[]]$Arguments,
        [int]$Attempts = 1,
        [int]$DelaySeconds = 1,
        # 標準入力を NUL にする。op item create はパイプされた標準入力を
        # アイテムの JSON テンプレートとして読むため、自動実行時に必要。
        [switch]$NullStdin
    )

    for ($attempt = 1; $attempt -le $Attempts; $attempt++) {
        # op は UTF-8 で出力する。Windows PowerShell の既定のデコードでは
        # 日本語の vault / 項目名を含む JSON が壊れるため、呼び出しごとに切り替える。
        $previousEncoding = [Console]::OutputEncoding
        try {
            [Console]::OutputEncoding = [System.Text.UTF8Encoding]::new($false)

            if ($NullStdin) {
                $quotedArguments = $Arguments | ForEach-Object { '"' + ($_ -replace '"', '\"') + '"' }
                $commandLine = "op " + ($quotedArguments -join " ") + " < NUL"
                $output = @(Invoke-NativeWithContinuePreference { & cmd.exe /c $commandLine 2>&1 })
            }
            else {
                $output = @(Invoke-NativeWithContinuePreference { & op @Arguments 2>&1 })
            }
        }
        finally {
            [Console]::OutputEncoding = $previousEncoding
        }

        $exitCode = $LASTEXITCODE
        $text = ($output | Out-String).Trim()

        $retryable = ($exitCode -ne 0) -and (Test-OnePasswordTransientError $text)
        if (-not $retryable -or $attempt -ge $Attempts) {
            return [pscustomobject]@{
                ExitCode = $exitCode
                Output   = $output
                Text     = $text
            }
        }

        Start-Sleep -Seconds $DelaySeconds
    }
}

function Assert-OnePasswordSignedIn {
    Assert-OnePasswordCli

    # 1Password アプリがスリープしていた直後の初回呼び出しは
    # "account is not signed in" で失敗することがあるため数回リトライする。
    $whoami = Invoke-OnePasswordCli -Arguments @("whoami") -Attempts 2 -DelaySeconds 2
    if ($whoami.ExitCode -eq 0) {
        return
    }

    # op whoami が失敗してもデータ系コマンドが通る環境があるため、
    # vault 一覧の取得でもサインイン状態を確認する。
    $vaults = Invoke-OnePasswordCli -Arguments @("vault", "list", "--format=json") -Attempts 1
    if ($vaults.ExitCode -eq 0) {
        return
    }

    $detail = if ($whoami.Text) { " [$($whoami.Text -replace '\s+', ' ')]" } else { "" }
    throw "1Password CLI がサインインしていません$detail。1Password アプリを起動してロックを解除し、設定 > 開発者 で「1Password CLI と統合」を有効にしてください。"
}

function Get-OnePasswordVaultNames {
    Assert-OnePasswordCli

    $result = Invoke-OnePasswordCli -Arguments @("vault", "list", "--format=json") -Attempts 2 -DelaySeconds 2
    if ($result.ExitCode -ne 0) {
        throw "1Password の vault 一覧を取得できませんでした。1Password アプリがロック解除されているか、CLI 連携が有効か確認してください。詳細: $($result.Text)"
    }

    return @($result.Text | ConvertFrom-Json | ForEach-Object { $_.name })
}

function Read-OnePasswordSecret {
    param([Parameter(Mandatory = $true)][string]$Reference)

    # op は UTF-8 で出力する。Windows PowerShell の既定のデコードで文字化けしないようにする。
    $previousEncoding = [Console]::OutputEncoding
    try {
        [Console]::OutputEncoding = [System.Text.UTF8Encoding]::new($false)
        $secret = Invoke-NativeWithContinuePreference { & op read --no-newline $Reference 2>$null }
    }
    finally {
        [Console]::OutputEncoding = $previousEncoding
    }

    if ($LASTEXITCODE -ne 0 -or [string]::IsNullOrEmpty($secret)) {
        return $null
    }

    return $secret
}

function Get-ResticPasswordFromOnePassword {
    param([string]$Reference)

    Assert-OnePasswordCli
    if ([string]::IsNullOrWhiteSpace($Reference)) {
        $Reference = Get-ResticOnePasswordRef
    }
    if ([string]::IsNullOrWhiteSpace($Reference)) {
        throw "1Password の参照が設定されていません。setup-project-backup.ps1 を実行するか、環境変数 RESTIC_OP_REF に参照 (例: op://Personal/restic/password) を設定してください。"
    }

    $password = Read-OnePasswordSecret -Reference $Reference

    # 初回の op 呼び出しが一時的に失敗することがあるため、
    # サインイン状態が復旧していれば 1 回だけ取得をやり直す。
    if ([string]::IsNullOrEmpty($password)) {
        $whoami = Invoke-OnePasswordCli -Arguments @("whoami") -Attempts 2 -DelaySeconds 2
        if ($whoami.ExitCode -eq 0) {
            $password = Read-OnePasswordSecret -Reference $Reference
        }
    }

    if ([string]::IsNullOrEmpty($password)) {
        throw "1Password から restic のパスワードを取得できませんでした ($Reference)。1Password がロックされていないか、項目とパスワードフィールドが存在するか確認してください。"
    }

    return $password
}

function Set-ResticPasswordEnvironment {
    param([string]$Reference)

    $resolvedRef = if ([string]::IsNullOrWhiteSpace($Reference)) { Get-ResticOnePasswordRef } else { $Reference }

    # 通常のバックアップは DPAPI キャッシュだけを使い、1Password には問い合わせない。
    $password = Get-ResticCachedPassword
    $source = "cache"

    if ([string]::IsNullOrEmpty($password)) {
        # キャッシュがないときだけ 1Password から取得する（このとき認証ダイアログが出る）
        $password = Get-ResticPasswordFromOnePassword -Reference $resolvedRef
        $null = Set-ResticPasswordCache -Password $password
        $source = "1password"
    }

    $env:RESTIC_PASSWORD = $password
    Remove-Item Env:\RESTIC_PASSWORD_FILE -ErrorAction SilentlyContinue
    Remove-Item Env:\RESTIC_PASSWORD_COMMAND -ErrorAction SilentlyContinue
    return $source
}

function Test-ResticOnePasswordItem {
    param(
        [Parameter(Mandatory = $true)][string]$Vault,
        [Parameter(Mandatory = $true)][string]$Item
    )

    Assert-OnePasswordCli

    $result = Invoke-OnePasswordCli -Arguments @("item", "get", $Item, "--vault", $Vault, "--format", "json") -Attempts 2 -DelaySeconds 2
    if ($result.ExitCode -eq 0) {
        return $true
    }

    if ($result.Text -match "isn't a vault|not a vault") {
        throw "1Password の vault '$Vault' が見つかりません。存在する vault 名を指定してください。"
    }
    if ($result.Text -match "isn't an item|not found") {
        return $false
    }

    throw "1Password の項目 '$Item' を確認できませんでした (vault: $Vault): $($result.Text)"
}

function New-ResticOnePasswordItem {
    param(
        [Parameter(Mandatory = $true)][string]$Vault,
        [Parameter(Mandatory = $true)][string]$Item
    )

    Assert-OnePasswordCli

    $result = Invoke-OnePasswordCli -Arguments @("item", "create", "--category=password", "--title=$Item", "--vault=$Vault", "--generate-password") -Attempts 1 -NullStdin
    if ($result.ExitCode -ne 0) {
        throw "1Password に項目 '$Item' を作成できませんでした (vault: $Vault): $($result.Text)"
    }
}
