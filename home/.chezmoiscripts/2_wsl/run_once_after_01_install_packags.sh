#!/bin/bash
set -euo pipefail

has() {
    type "$1" > /dev/null 2>&1
}
# wslu
if ! has "wslview"; then
    echo "install wslu"
    sudo apt install -y wslu
fi

# xsel
if ! has "xsel"; then
    echo "install xsel"
    sudo apt install -y xsel
fi
