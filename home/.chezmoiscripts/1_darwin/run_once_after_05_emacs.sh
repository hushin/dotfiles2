#!/bin/bash
set -euo pipefail

source ~/.profile

echo "install doom emacs"
~/.config/emacs/bin/doom install
~/.config/emacs/bin/doom sync

# osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@30/Emacs.app" at posix file "/Applications" with properties {name:"Emacs.app"}'
