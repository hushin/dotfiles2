param(
    [string]$Repository
)

function Main {
    $gitUser = git config user.name
    if (-not $gitUser) {
        Write-Error "Error: git user.name not configured"
        exit 1
    }

    $sandboxSuffix = "-sandbox"

    Write-Host "Fetching repositories..."

    $repos = @()

    try {
        $mainRepos = gh repo list $gitUser --json nameWithOwner, description, updatedAt | ConvertFrom-Json
        $repos += $mainRepos
    }
    catch {
        Write-Warning "Failed to fetch repositories for $gitUser"
    }

    try {
        $sandboxRepos = gh repo list "$gitUser$sandboxSuffix" --json nameWithOwner, description, updatedAt | ConvertFrom-Json
        $repos += $sandboxRepos
    }
    catch {
        Write-Warning "Failed to fetch repositories for $gitUser$sandboxSuffix"
    }

    if ($repos.Count -eq 0) {
        Write-Error "Error: No repositories found or failed to fetch from GitHub"
        exit 1
    }

    $sortedRepos = $repos | Sort-Object updatedAt -Descending | Get-Unique -AsString

    $formattedRepos = $sortedRepos | ForEach-Object {
        $date = ([datetime]$_.updatedAt).ToString("yyyy-MM-dd")
        $desc = if ($_.description) { $_.description } else { "No description" }
        "{0,-12} {1,-35} {2}" -f $date, $_.nameWithOwner, $desc
    }

    if ($Repository) {
        $selectedRepo = $Repository
    }
    else {
        $selected = $formattedRepos | fzf --prompt="Select repository (date repo desc): " --height=40% --reverse

        if (-not $selected) {
            Write-Host "No repository selected."
            exit 0
        }

        $selectedRepo = ($selected -split '\s+')[1]
    }

    Write-Host "Getting repository: $selectedRepo"

    $result = ghq get -p $selectedRepo
    if ($LASTEXITCODE -eq 0) {
        Write-Host "Successfully cloned: $selectedRepo" -ForegroundColor Green
    }
    else {
        Write-Error "Error: Failed to clone repository: $selectedRepo"
        exit 1
    }
}

Main
