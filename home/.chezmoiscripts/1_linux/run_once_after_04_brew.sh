#!/bin/bash
set -euo pipefail

eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
brew update

echo "Install homebrew apps"
brew bundle --global
