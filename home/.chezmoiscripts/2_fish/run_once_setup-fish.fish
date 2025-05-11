#!/usr/bin/fish

function has
  type "$argv[1]" > /dev/null 2>&1
end

# fisher
if not has "fisher"
  echo "install fisher"
  curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher
end

echo "install fish plugins"
fisher update

mise completion fish > ~/.config/fish/completions/mise.fish
fish_update_completions
