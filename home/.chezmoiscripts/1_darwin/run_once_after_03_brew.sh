#!/bin/bash
set -euo pipefail

eval $(/opt/homebrew/bin/brew shellenv)

echo "brew update"
brew update
echo "brew doctor"
brew doctor

echo "Install homebrew apps"
brew bundle --global

if [ "$SHELL" != "$(which fish)" ]; then
    echo "set fish as default shell"
    sudo chsh -s $(which fish)
fi
