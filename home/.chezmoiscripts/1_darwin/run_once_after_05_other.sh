#!/bin/bash
set -euo pipefail

has() {
    type "$1" > /dev/null 2>&1
}

if ! has "claude"; then
    echo "install claude code"
    curl -fsSL https://claude.ai/install.sh | bash
fi

if ! has "fetchmd"; then
    echo "install fetchmd"
    mkdir -p "$HOME/.local/bin"
    curl -fsSL https://raw.githubusercontent.com/hushin/fetchmd/main/scripts/install.sh | bash
fi
