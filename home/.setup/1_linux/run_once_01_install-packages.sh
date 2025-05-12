#!/bin/bash
set -euo pipefail

has() {
  type "$1" > /dev/null 2>&1
}

sudo apt update

# cmake
if ! has "cmake"; then
  echo "install cmake"
  sudo apt install -y cmake
fi

# mise
if ! has "mise"; then
  echo "innstall mise"
  curl https://mise.run | sh
  # sudo apt install -y libyaml-dev libevent-dev ncurses-dev build-essential bison pkg-config
fi

# fish
if ! has "fish"; then
  echo "install fish"
  sudo apt install -y software-properties-common
  sudo add-apt-repository -y ppa:fish-shell/release-4
  sudo apt update
  sudo apt install -y fish
  echo "set fish as default shell"
  sudo chsh -s $(which fish)
fi
