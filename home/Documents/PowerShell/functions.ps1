# 現在のセッションの PATH 情報を更新するための関数
function Update-PathVariable {
    $env:Path = [System.Environment]::GetEnvironmentVariable("Path", "Machine") + ";" +
    [System.Environment]::GetEnvironmentVariable("Path", "User")
}

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

function open {
    param([string]$path)

    if ($path -match '^https?://|^ftp://|^mailto:') {
        Start-Process $path
    }
    elseif (Test-Path $path) {
        if ((Get-Item $path).PSIsContainer) {
            explorer.exe $path
        }
        else {
            Invoke-Item $path
        }
    }
    else {
        Write-Error "パスが見つかりません: $path"
    }
}

function gwt {
    param([string]$branch)

    $gitCdupDir = git rev-parse --show-cdup
    git worktree add "$($gitCdupDir)git-worktrees/$branch" -b $branch
}

function swt {
    param([string]$query)

    $fzf_opts = "--border"

    if ($query) {
        $fzf_opts += " --query ""$query"" --select-1 --exit-0"
    }

    $selected = git worktree list | fzf $fzf_opts

    if ($selected) {
        # git worktree list の出力からパス部分を抽出
        # 例: "C:/Users/n1/worktrees/my-feature HEAD      (detached)" -> "C:/Users/n1/worktrees/my-feature"
        # 例: "C:/Users/n1/worktrees/another-branch another-branch  sha1" -> "C:/Users/n1/worktrees/another-branch"
        $target_dir = $selected.Split(' ')[0]
        Set-Location $target_dir
    }
}
