#!/bin/bash

set -eufo pipefail

has() {
    type "$1" > /dev/null 2>&1
}

# Ask for the administrator password upfront
sudo -v

# Keep-alive: update existing `sudo` time stamp until `brew.sh` has finished
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

# softwareupdate --install-rosetta --agree-to-license

if ! has "brew"; then
    # Install homebrew: https://brew.sh/
    echo "Install homebrew"
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi
