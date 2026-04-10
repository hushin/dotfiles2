#!/bin/bash

set -eufo pipefail

has() {
    type "$1" > /dev/null 2>&1
}

# google-japanese-ime に必要 だったが、2026/2 で不要になったはず
# softwareupdate --install-rosetta --agree-to-license

if ! has "brew"; then
    # Install homebrew: https://brew.sh/
    echo "Install homebrew"
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

eval $(/opt/homebrew/bin/brew shellenv)

echo "Install homebrew apps"
brew bundle --global
