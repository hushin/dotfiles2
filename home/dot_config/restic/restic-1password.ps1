# 1Password CLI 経由で restic のパスワードを扱う共通処理。
# パスワードはディスクに保存せず、restic を実行するプロセスの環境変数にのみ展開する。

function Get-ResticOnePasswordRefFile {
    Join-Path $env:USERPROFILE ".config\restic\1password-ref.txt"
}

function Get-ResticLegacyPasswordFile {
    Join-Path $env:USERPROFILE ".config\restic\password.txt"
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

    return "op://Private/restic/password"
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

function Assert-OnePasswordSignedIn {
    Assert-OnePasswordCli

    $null = Invoke-NativeWithContinuePreference { & op whoami 2>$null }
    if ($LASTEXITCODE -ne 0) {
        throw "1Password CLI がサインインしていません。1Password アプリを起動してロックを解除し、CLI 連携を有効にしてください。"
    }
}

function Get-ResticPasswordFromOnePassword {
    param([string]$Reference)

    Assert-OnePasswordCli
    if ([string]::IsNullOrWhiteSpace($Reference)) {
        $Reference = Get-ResticOnePasswordRef
    }

    # op は UTF-8 で出力する。Windows PowerShell の既定のデコードで文字化けしないようにする。
    $previousEncoding = [Console]::OutputEncoding
    try {
        [Console]::OutputEncoding = [System.Text.UTF8Encoding]::new($false)
        $password = Invoke-NativeWithContinuePreference { & op read --no-newline $Reference }
    }
    finally {
        [Console]::OutputEncoding = $previousEncoding
    }

    if ($LASTEXITCODE -ne 0) {
        throw "1Password から restic のパスワードを取得できませんでした ($Reference)。1Password がロックされていないか、CLI 連携が有効か確認してください。"
    }
    if ([string]::IsNullOrEmpty($password)) {
        throw "1Password の項目が空です: $Reference"
    }

    return $password
}

function Set-ResticPasswordEnvironment {
    param([string]$Reference)

    # 1Password の設定がまだで旧パスワードファイルが残っている場合は、
    # chezmoi apply 直後にスケジュールタスクが失敗しないよう従来どおり動作させる。
    if ([string]::IsNullOrWhiteSpace($Reference) -and
        [string]::IsNullOrWhiteSpace($env:RESTIC_OP_REF) -and
        -not (Test-Path -LiteralPath (Get-ResticOnePasswordRefFile)) -and
        (Test-Path -LiteralPath (Get-ResticLegacyPasswordFile))) {
        Remove-Item Env:\RESTIC_PASSWORD -ErrorAction SilentlyContinue
        Remove-Item Env:\RESTIC_PASSWORD_COMMAND -ErrorAction SilentlyContinue
        $env:RESTIC_PASSWORD_FILE = Get-ResticLegacyPasswordFile
        return "legacy"
    }

    $env:RESTIC_PASSWORD = Get-ResticPasswordFromOnePassword -Reference $Reference
    Remove-Item Env:\RESTIC_PASSWORD_FILE -ErrorAction SilentlyContinue
    Remove-Item Env:\RESTIC_PASSWORD_COMMAND -ErrorAction SilentlyContinue
    return "1password"
}

function Test-ResticOnePasswordItem {
    param(
        [Parameter(Mandatory = $true)][string]$Vault,
        [Parameter(Mandatory = $true)][string]$Item
    )

    Assert-OnePasswordCli

    # 存在確認だけなので、認証エラーなどは呼び出し側で op whoami により先に検出する。
    $null = Invoke-NativeWithContinuePreference { & op item get $Item --vault $Vault --format json 2>$null }
    return ($LASTEXITCODE -eq 0)
}

function New-ResticOnePasswordItem {
    param(
        [Parameter(Mandatory = $true)][string]$Vault,
        [Parameter(Mandatory = $true)][string]$Item
    )

    Assert-OnePasswordCli

    $output = Invoke-NativeWithContinuePreference { & op item create --category=password "--title=$Item" "--vault=$Vault" --generate-password 2>&1 }
    if ($LASTEXITCODE -ne 0) {
        throw "1Password に項目 '$Item' を作成できませんでした (vault: $Vault): $output"
    }
}
