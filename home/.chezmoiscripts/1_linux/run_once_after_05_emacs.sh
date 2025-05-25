#!/bin/bash
set -euo pipefail

has() {
  type "$1" > /dev/null 2>&1
}

~/.config/emacs/bin/doom install
