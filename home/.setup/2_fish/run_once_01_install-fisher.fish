#!/usr/bin/fish

function has
  type "$argv[1]" > /dev/null 2>&1
end

# fisher
if not has "fisher"
  echo "install fisher"
  curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher
end
mise completion fish > ~/.config/fish/completions/mise.fish
