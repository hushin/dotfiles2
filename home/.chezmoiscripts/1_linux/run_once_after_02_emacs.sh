#!/bin/bash
set -euo pipefail

has() {
  type "$1" > /dev/null 2>&1
}

if ! has "emacs"; then
  # https://launchpad.net/~ubuntuhandbook1/+archive/ubuntu/emacs
  echo "install emacs"
  sudo add-apt-repository -y ppa:ubuntuhandbook1/emacs
  sudo apt update
  sudo apt install -y emacs-nox
fi

~/.config/emacs/bin/doom install
