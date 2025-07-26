#!/bin/sh

# devcontainer 用
sh -c "$(curl -fsLS get.chezmoi.io)" -- -b $HOME/.local/bin
chezmoi init --apply hushin/dotfiles2
