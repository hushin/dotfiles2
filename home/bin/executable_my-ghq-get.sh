#!/bin/bash

set -euo pipefail

my_ghq_get() {
    local git_user
    git_user=$(git config user.name)
    
    if [[ -z "$git_user" ]]; then
        echo "Error: git user.name not configured" >&2
        return 1
    fi

    local sandbox_suffix="-sandbox"
    local repos_info
    
    # Get repositories from both main user and sandbox user
    repos_info=$(
        {
            gh repo list "$git_user" --json nameWithOwner,description,updatedAt --jq '.[] | "\(.updatedAt)\t\(.nameWithOwner)\t\(.description // "No description")"' 2>/dev/null || true
            gh repo list "${git_user}${sandbox_suffix}" --json nameWithOwner,description,updatedAt --jq '.[] | "\(.updatedAt)\t\(.nameWithOwner)\t\(.description // "No description")"' 2>/dev/null || true
        } | sort -r | uniq -f1
    )

    if [[ -z "$repos_info" ]]; then
        echo "Error: No repositories found or failed to fetch from GitHub" >&2
        return 1
    fi

    # Format repositories for display
    local formatted_repos
    formatted_repos=$(printf '%s\n' "$repos_info" | awk -F'\t' '{
        split($1, date_parts, "T")
        split(date_parts[1], ymd, "-")
        short_date = ymd[1] "-" ymd[2] "-" ymd[3]
        printf "%-12s %-35s %s\n", short_date, $2, $3
    }')

    # Let user select repository with fzf
    local selected_line
    selected_line=$(printf '%s\n' "$formatted_repos" | fzf --prompt="Select repository (date repo desc): " --height=40% --reverse)

    if [[ -n "$selected_line" ]]; then
        local selected_repo
        selected_repo=$(echo "$selected_line" | awk '{print $2}')
        echo "Getting repository: $selected_repo"
        
        if ghq get -p "$selected_repo"; then
            echo "Successfully cloned: $selected_repo"
        else
            echo "Error: Failed to clone repository: $selected_repo" >&2
            return 1
        fi
    else
        echo "No repository selected."
        return 0
    fi
}

# Execute the function if script is run directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    my_ghq_get "$@"
fi