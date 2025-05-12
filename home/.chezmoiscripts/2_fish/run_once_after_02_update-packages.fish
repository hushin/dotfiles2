#!/usr/bin/fish

echo "install fish plugins"
fisher update

echo "update fish completions"
mise completion fish > ~/.config/fish/completions/mise.fish
fish_update_completions
