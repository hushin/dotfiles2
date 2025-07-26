#!/bin/bash
set -euo pipefail

has() {
    type "$1" > /dev/null 2>&1
}

if has "emacs"; then
    echo "install doom emacs"
    ~/.config/emacs/bin/doom install
fi
