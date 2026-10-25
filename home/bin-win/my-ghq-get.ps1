param(
    [string]$Repository
)

function Invoke-GhJson {
    param([string[]]$GhArgs)

    # gh emits UTF-8. Capturing native output decodes it with the console code
    # page, which corrupts non-ASCII descriptions and breaks ConvertFrom-Json.
    $previousEncoding = [Console]::OutputEncoding
    try {
        [Console]::OutputEncoding = [System.Text.UTF8Encoding]::new($false)
        $output = & gh @GhArgs 2>$null
        if ($LASTEXITCODE -ne 0 -or -not $output) {
            return $null
        }
        return $output
    }
    finally {
        [Console]::OutputEncoding = $previousEncoding
    }
}

function Get-GitHubOwner {
    # Prefer the authenticated GitHub account: git config user.name is a display
    # name, may be set in several files (returning an array), and may not match
    # the GitHub login at all.
    $login = (& gh api user --jq .login 2>$null | Select-Object -First 1)
    if ($LASTEXITCODE -eq 0 -and $login) {
        return $login.Trim()
    }

    $name = (git config --get user.name 2>$null | Where-Object { $_ } | Select-Object -First 1)
    if ($name) {
        return $name.Trim()
    }

    return $null
}

function Get-OwnerRepos {
    param([string]$Owner)

    # Keep the --json list free of spaces: PowerShell would split it into extra
    # positional args and gh fails with "accepts at most 1 arg(s), received 3".
    $json = Invoke-GhJson @('repo', 'list', $Owner, '--limit', '1000', '--json', 'nameWithOwner,description,updatedAt')
    if (-not $json) {
        return @()
    }

    try {
        return @($json | ConvertFrom-Json)
    }
    catch {
        Write-Warning "Failed to parse repository list for ${Owner}: $($_.Exception.Message)"
        return @()
    }
}

function Main {
    $owner = Get-GitHubOwner
    if (-not $owner) {
        Write-Error "Error: could not determine GitHub owner (is gh authenticated?)"
        exit 1
    }

    $sandboxSuffix = "-sandbox"

    Write-Host "Fetching repositories..."

    # Keyed by nameWithOwner so duplicates across accounts are collapsed.
    $byName = [ordered]@{}

    foreach ($account in @($owner, "$owner$sandboxSuffix")) {
        $accountRepos = @(Get-OwnerRepos -Owner $account)
        if ($accountRepos.Count -eq 0) {
            Write-Warning "No repositories fetched for $account (missing account or no access)"
            continue
        }
        foreach ($repo in $accountRepos) {
            if (-not $byName.Contains($repo.nameWithOwner)) {
                $byName[$repo.nameWithOwner] = $repo
            }
        }
    }

    if ($byName.Count -eq 0) {
        Write-Error "Error: No repositories found or failed to fetch from GitHub"
        exit 1
    }

    $sortedRepos = $byName.Values | Sort-Object { [datetime]$_.updatedAt } -Descending

    $formattedRepos = $sortedRepos | ForEach-Object {
        $date = ([datetime]$_.updatedAt).ToString("yyyy-MM-dd")
        $desc = if ($_.description) { $_.description } else { "No description" }
        "{0}`t{1,-35}`t{2}" -f $date, $_.nameWithOwner, $desc
    }

    if ($Repository) {
        $selectedRepo = $Repository
    }
    else {
        $selected = $formattedRepos | fzf --prompt="Select repository (date repo desc): " --height=40% --reverse --delimiter="`t" --with-nth=1,2,3

        if (-not $selected) {
            Write-Host "No repository selected."
            exit 0
        }

        # Field 2 is the repo, padded by the format string; drop the padding.
        $selectedRepo = (($selected -split "`t")[1]).Trim()
    }

    # Accept "owner/repo", or a bare repo name for the current owner.
    if ($selectedRepo -notmatch '/') {
        $selectedRepo = "$owner/$selectedRepo"
    }

    Write-Host "Getting repository: $selectedRepo"

    ghq get -p $selectedRepo
    if ($LASTEXITCODE -eq 0) {
        Write-Host "Successfully cloned: $selectedRepo" -ForegroundColor Green
    }
    else {
        Write-Error "Error: Failed to clone repository: $selectedRepo"
        exit 1
    }
}

Main
