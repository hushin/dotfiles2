if status is-interactive
    # Commands to run in interactive sessions can go here
end

if type -q fenv
    fenv source ~/.profile
end

if command -v mise >/dev/null 2>&1
    mise activate fish | source
end
if command -v starship >/dev/null 2>&1
    starship init fish | source
end

. ~/.config/fish/env.fish
. ~/.config/fish/aliases.fish
. ~/.config/fish/funcs.fish
. ~/.config/fish/keybinds.fish

if command -v zoxide >/dev/null 2>&1
    zoxide init fish | source
end
