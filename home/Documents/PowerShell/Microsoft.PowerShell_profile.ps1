# $PROFILE.CurrentUserCurrentHost

# 現在のセッションの PATH 情報を更新するための関数
function Update-PathVariable {
    $env:Path = [System.Environment]::GetEnvironmentVariable("Path", "Machine") +
    ";" +
    [System.Environment]::GetEnvironmentVariable("Path", "User")
}

# miseのshimパスを追加
$shimPath = "$env:USERPROFILE\AppData\Local\mise\shims"
$currentPath = [Environment]::GetEnvironmentVariable('Path', 'User')
$newPath = $currentPath + ";" + $shimPath
[Environment]::SetEnvironmentVariable('Path', $newPath, 'User')

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

Set-Alias grep rg
Set-Alias open Invoke-Item

function gf {
    $path = ghq list | fzf
    if ($LastExitCode -eq 0) {
        Set-Location "$(ghq root)\$path"
    }
}
function ghg {
    ghq get --shallow $args
}

function mkdev {
    if ($args.Length -ne 1) {
        Write-Output "Usage: mkdev dir-name"
        return
    }
    $dirName = $args[0]
    $devPath = "$(ghq root)\github.com\$(git config user.name)\$dirName"
    mkdir -p $devPath
    Set-Location $devPath
    git init
}

function mksandbox {
    if ($args.Length -ne 1) {
        Write-Output "Usage: mksandbox dir-name"
        return
    }
    $dirName = $args[0]
    $devPath = "$(ghq root)\github.com\$(git config user.name)-sandbox\$dirName"
    mkdir -p $devPath
    Set-Location $devPath
    git init
}

function crrepo {
    # Gitリポジトリのルートに移動
    $gitRoot = git rev-parse --show-toplevel 2>$null
    if ($gitRoot) {
        Set-Location $gitRoot
    }

    # リポジトリ名を取得（owner/repo形式）
    $currentPath = (Get-Location).Path -replace '\\', '/'
    if ($currentPath -match '[^/]*/[^/]*$') {
        $repoName = $Matches[0]
        gh repo create $repoName --source=. $args
    }
    else {
        Write-Error "Could not determine repository name from current directory"
    }
}

function which($cmdname) {
    Get-Command $cmdname | Select-Object -ExpandProperty Definition
}

# Abbreviation system (fish-like abbr)
$global:Abbreviations = @{}

function abbr {
    param(
        [string]$Key,
        [string]$Value
    )

    if (-not $Key) {
        # List all abbreviations
        foreach ($abbr in $global:Abbreviations.GetEnumerator()) {
            Write-Output "$($abbr.Key) -> $($abbr.Value)"
        }
        return
    }

    if (-not $Value) {
        # Show specific abbreviation
        if ($global:Abbreviations.ContainsKey($Key)) {
            Write-Output "$Key -> $($global:Abbreviations[$Key])"
        }
        else {
            Write-Output "Abbreviation '$Key' not found"
        }
        return
    }

    # Add abbreviation
    $global:Abbreviations[$Key] = $Value
    # Write-Output "Added abbreviation: $Key -> $Value"
}

# Function to handle abbreviation expansion
function Expand-Abbreviation {
    $line = $null
    $cursor = $null
    [Microsoft.PowerShell.PSConsoleReadLine]::GetBufferState([ref]$line, [ref]$cursor)

    # Get the word before cursor
    $words = $line.Substring(0, $cursor) -split '\s+'
    $lastWord = $words[-1]

    if ($global:Abbreviations.ContainsKey($lastWord)) {
        # Replace the abbreviation
        $replacement = $global:Abbreviations[$lastWord]
        $startPos = $cursor - $lastWord.Length
        [Microsoft.PowerShell.PSConsoleReadLine]::Replace($startPos, $lastWord.Length, $replacement)
    }
}

# PSReadLine handler for abbreviation expansion on Space
Set-PSReadLineKeyHandler -Key ' ' -ScriptBlock {
    Expand-Abbreviation
    # Insert the space
    [Microsoft.PowerShell.PSConsoleReadLine]::Insert(' ')
}

# PSReadLine handler for abbreviation expansion on Enter
Set-PSReadLineKeyHandler -Key 'Enter' -ScriptBlock {
    Expand-Abbreviation
    # Execute the command
    [Microsoft.PowerShell.PSConsoleReadLine]::AcceptLine()
}

# Git abbreviations
abbr 'ta' 'tig --all'
abbr 'ts' 'tig status'
abbr 'g' 'git'
abbr 'gsh' 'git show'
abbr 'gdc' 'git diff --cached'
abbr 'gs' 'git switch'
abbr 'gsc' 'git switch -c'

# Search abbreviations
abbr 'rgh' 'rg --hidden'

# Chezmoi abbreviations
abbr 'ch' 'chezmoi'
abbr 'cha' 'chezmoi add'
abbr 'che' 'chezmoi edit'
abbr 'chm' 'chezmoi merge'
abbr 'chd' 'chezmoi diff'
abbr 'chp' 'chezmoi apply'
abbr 'chu' 'chezmoi update'

# Utility abbreviations
abbr 'reload' '. $PROFILE'
abbr 'diff' 'delta'
abbr 'find' 'fd'
abbr 'cdu' 'cd (git rev-parse --show-toplevel)'
abbr 'tree' 'tree -N'
abbr 'notes' 'rg "TODO|HACK|FIXME|OPTIMIZE"'

# Editor abbreviations (Windows adapted)
abbr 'e' 'code'
abbr 'o' 'Invoke-Item'

# eza abbreviations (if available)
if (Get-Command eza -ErrorAction SilentlyContinue) {
    abbr 'ls' 'eza --icons'
    abbr 'll' 'eza --icons -lhag --time-style long-iso'
    abbr 'lt' 'eza --icons --tree'
}
else {
    abbr 'll' 'ls -la'
    abbr 'la' 'ls -a'
}

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
