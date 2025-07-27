#!/bin/bash
set -euo pipefail

has() {
    type "$1" > /dev/null 2>&1
}


if ! has "nkf"; then
    echo "install nkf"
    sudo apt install -y nkf
fi

if ! has "tldr"; then
    echo "install tldr"
    sudo apt install -y tldr
fi
