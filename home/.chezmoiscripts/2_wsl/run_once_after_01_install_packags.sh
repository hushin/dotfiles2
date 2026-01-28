#!/bin/bash
set -euo pipefail

has() {
    type "$1" > /dev/null 2>&1
}

# xsel
if ! has "xsel"; then
    echo "install xsel"
    sudo apt install -y xsel
fi
