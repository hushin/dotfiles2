# $PROFILE.CurrentUserCurrentHost

$env:FZF_DEFAULT_COMMAND = 'rg -g "" --hidden --ignore ".git"'
$env:FZF_DEFAULT_OPTS = "--height 70% --layout=reverse --border --ansi --inline-info"

# ref. https://secon.dev/entry/2020/08/17/070735/
Set-PSReadLineOption -EditMode Emacs
Set-PSReadLineOption -BellStyle None
# 標準だと Ctrl+d は DeleteCharOrExit のため、うっかり端末が終了することを防ぐ
Set-PSReadLineKeyHandler -Chord 'Ctrl+d' -Function DeleteChar
# EditMode Emacs 標準のタブ補完
Set-PSReadLineKeyHandler -Key Tab -Function Complete
# メニュー補完に変更
Set-PSReadLineKeyHandler -Key Tab -Function MenuComplete

# uutils-coreutils プロファイルに追加
@"
    arch, base32, base64, basename, cat, cksum, comm, cp, cut, date, df, dircolors, dirname,
    echo, env, expand, expr, factor, false, fmt, fold, hashsum, head, hostname, join, link, ln,
    ls, md5sum, mkdir, mktemp, more, mv, nl, nproc, od, paste, printenv, printf, ptx, pwd,
    readlink, realpath, relpath, rm, rmdir, seq, sha1sum, sha224sum, sha256sum, sha3-224sum,
    sha3-256sum, sha3-384sum, sha3-512sum, sha384sum, sha3sum, sha512sum, shake128sum,
    shake256sum, shred, shuf, sleep, sort, split, sum, sync, tac, tail, tee, test, touch, tr,
    true, truncate, tsort, unexpand, uniq, wc, whoami, yes
"@ -split ',' |
ForEach-Object { $_.trim() } |
Where-Object { ! @('tee', 'sort', 'sleep').Contains($_) } |
ForEach-Object {
    $cmd = $_
    if (Test-Path Alias:$cmd) { Remove-Item -Path Alias:$cmd }
    $fn = '$input | coreutils ' + $cmd + ' $args'
    Invoke-Expression "function global:$cmd { $fn }"
}

Set-Alias open Invoke-Item

. "$PSScriptRoot\functions.ps1"
. "$PSScriptRoot\abbreviations.ps1"

# key binding
# 実行後入力待ちになるため、AcceptLine を実行する
Set-PSReadLineKeyHandler -Chord 'Ctrl+]' -ScriptBlock { gf; [Microsoft.PowerShell.PSConsoleReadLine]::AcceptLine() }
# Ctrl+j: zoxideの履歴からディレクトリを選択して移動
Set-PSReadLineKeyHandler -Chord 'Ctrl+j' -ScriptBlock {
    $selection = zoxide query --list | fzf
    if ($selection) {
        Set-Location $selection
        [Microsoft.PowerShell.PSConsoleReadLine]::AcceptLine()
    }
}
Set-PsFzfOption -PSReadlineChordProvider 'Ctrl+t' -PSReadlineChordReverseHistory 'Ctrl+r'

$localrc = "$env:HOMEPATH/.profile.local.ps1"

if (Test-Path $localrc) {
    . $localrc
}

# carapace (補完ツール) の設定
$env:CARAPACE_BRIDGES = "zsh,fish,bash,inshellisense" # optional
Set-PSReadLineOption -Colors @{ "Selection" = "`e[7m" }
Set-PSReadlineKeyHandler -Key Tab -Function MenuComplete
carapace _carapace | Out-String | Invoke-Expression

# Starship の初期化
Invoke-Expression (&starship init powershell)
# zoxide (cdの改良) の初期化
Invoke-Expression (& { (zoxide init powershell | Out-String) })
