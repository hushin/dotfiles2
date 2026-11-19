#!/bin/bash
set -euo pipefail

export PATH="$HOME/.local/bin:$HOME/.bun/bin:$HOME/.local/share/mise/shims:$PATH"

has() {
    type "$1" > /dev/null 2>&1
}

if ! has "claude"; then
    echo "install claude code"
    curl -fsSL https://claude.ai/install.sh | bash
fi

if ! has "pi"; then
    echo "install pi"
    curl -fsSL https://pi.dev/install.sh | sh
fi

if ! has "gws" && has "npm"; then
    echo "install gws"
    npm install -g --allow-scripts=@googleworkspace/cli @googleworkspace/cli
fi

if ! has "fetchmd"; then
    echo "install fetchmd"
    mkdir -p "$HOME/.local/bin"
    curl -fsSL https://raw.githubusercontent.com/hushin/fetchmd/main/scripts/install.sh | bash
fi

if ! git help -a | grep -qw "wt"; then
    echo "install git-wt"
    go install github.com/k1LoW/git-wt@latest
fi
