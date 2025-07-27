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

    $lastWord = $line

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
abbr 'grep' 'rg'
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
abbr 'cd' 'z'

# Editor abbreviations (Windows adapted)
abbr 'e' 'code'
abbr 'o' 'Invoke-Item'

# eza abbreviations (if available)
if (Get-Command eza -ErrorAction SilentlyContinue) {
    abbr 'ls' 'eza --icons'
    abbr 'll' 'eza --icons -lhg --time-style long-iso'
    abbr 'la' 'eza --icons -lhag --time-style long-iso'
    abbr 'lt' 'eza --icons --tree'
}
else {
    abbr 'll' 'ls -la'
    abbr 'la' 'ls -a'
}
