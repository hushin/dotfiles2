#!/bin/bash
set -euo pipefail

has() {
  type "$1" > /dev/null 2>&1
}

if ! has "ag"; then
  echo "install ag"
  sudo apt install -y silversearcher-ag
fi

if ! has "nkf"; then
  echo "install nkf"
  sudo apt install -y nkf
fi

if ! has "source-highlight"; then
  echo "install source-highlight"
  sudo apt install -y source-highlight
fi

if ! has "tldr"; then
  echo "install tldr"
  sudo apt install -y tldr
fi
