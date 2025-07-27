#!/bin/bash
# czs - chezmoi script selector

set -euo pipefail

CHEZMOI_SOURCE_DIR=$(chezmoi source-path)
SCRIPTS_DIR="$CHEZMOI_SOURCE_DIR/.chezmoiscripts"

if [[ ! -d "$SCRIPTS_DIR" ]]; then
    echo "Scripts directory not found: $SCRIPTS_DIR" >&2
    exit 1
fi

# Find all script files
scripts=$(find "$SCRIPTS_DIR" -type f \( -name "*.sh" -o -name "*.sh.tmpl" -o -name "*.fish" -o -name "*.ps1" \) | sort)

if [[ -z "$scripts" ]]; then
    echo "No scripts found in $SCRIPTS_DIR" >&2
    exit 1
fi

# Format script list for fzf display
formatted_scripts=""
while IFS= read -r script; do
    relative_path=${script#"$SCRIPTS_DIR/"}
    basename=$(basename "$script")
    dirname=$(dirname "$relative_path")
    formatted_scripts+="$(printf '%-10s' "$dirname") $basename|$script"$'\n'
done <<< "$scripts"

# Use fzf to select script
selected=$(echo "$formatted_scripts" | fzf \
    --delimiter='|' \
    --with-nth=1 \
    --preview='cat {2}' \
    --preview-window='right:60%' \
    --prompt='Select script: ' \
    --height=60% \
    --border)

if [[ -z "$selected" ]]; then
    echo "No script selected" >&2
    exit 0
fi

# Extract the actual file path
script_path=$(echo "$selected" | cut -d'|' -f2)
script_name=$(basename "$script_path")

echo "Selected: $script_name"
echo "Path: $script_path"

# Execute script
execute_script() {
    local script_file="$1"
    local shebang=$(head -n1 "$script_file" 2>/dev/null)

    # Default to bash if no shebang
    local interpreter="bash"

    if [[ "$shebang" =~ ^#!/.*fish ]]; then
        interpreter="fish"
    elif [[ "$shebang" =~ ^#!/.*bash ]]; then
        interpreter="bash"
    elif [[ "$shebang" =~ ^#!/bin/sh ]]; then
        interpreter="sh"
    fi

    echo "Executing with $interpreter..."
    echo ""
    "$interpreter" "$script_file"
}

# Check if it's a template file
if [[ "$script_path" == *.tmpl ]]; then
    echo ""
    echo "This is a template file. Executing with chezmoi execute-template..."
    echo ""
    cd "$CHEZMOI_SOURCE_DIR"

    # Create temporary file for template output
    temp_script=$(mktemp)
    trap "rm -f $temp_script" EXIT

    chezmoi execute-template < "$script_path" > "$temp_script"
    execute_script "$temp_script"
else
    echo ""
    execute_script "$script_path"
fi
