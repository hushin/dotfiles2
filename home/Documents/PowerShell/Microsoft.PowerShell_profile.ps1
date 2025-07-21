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
  $fn = '$input | uutils ' + $cmd + ' $args'
  Invoke-Expression "function global:$cmd { $fn }"
}

Set-Alias grep rg

function cd {
  if ($args.Length -gt 0) {
    Set-Location $args[0]
  }
  else {
    Set-Location $env:HOMEPATH
  }
}

function gf {
  $path = ghq list | fzf
  if ($LastExitCode -eq 0) {
    cd "$(ghq root)\$path"
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
  cd $devPath
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
  cd $devPath
  git init
}

function crrepo {
  # Gitリポジトリのルートに移動
  $gitRoot = git rev-parse --show-toplevel 2>$null
  if ($gitRoot) {
    Set-Location $gitRoot
  }

  # リポジトリ名を取得（owner/repo形式）
  $pwd = (Get-Location).Path -replace '\\', '/'
  if ($pwd -match '[^/]*/[^/]*$') {
    $repoName = $Matches[0]
    gh repo create $repoName --source=. $args
  }
  else {
    Write-Error "Could not determine repository name from current directory"
  }
}

function reload {
  Update-PathVariable
  # プロファイルを再読み込み
  . $PROFILE.CurrentUserCurrentHost
}

# key binding
# 実行後入力待ちになるため、AcceptLine を実行する
Set-PSReadLineKeyHandler -Chord 'Ctrl+]' -ScriptBlock { gf; [Microsoft.PowerShell.PSConsoleReadLine]::AcceptLine() }
Set-PSReadLineKeyHandler -Chord 'Ctrl+j' -ScriptBlock { Invoke-FuzzyZLocation; [Microsoft.PowerShell.PSConsoleReadLine]::AcceptLine() }
# Set-PsFzfOption -PSReadlineChordProvider 'Ctrl+t' -PSReadlineChordReverseHistory 'Ctrl+r'

$localrc = "$env:HOMEPATH/.profile.local.ps1"

if (Test-Path $localrc) {
  . $localrc
}

# carapace (補完ツール) の設定
$env:CARAPACE_BRIDGES = "zsh,fish,bash,inshellisense" # optional
Set-PSReadLineOption -Colors @{ "Selection" = "`e[7m" }
Set-PSReadlineKeyHandler -Key Tab -Function MenuComplete
carapace _carapace | Out-String | Invoke-Expression

# zoxide (cdの改良) の初期化
Invoke-Expression (& { (zoxide init powershell | Out-String) })
# Starship の初期化 (最後に追加)
Invoke-Expression (&starship init powershell)
