if status is-interactive
    # Commands to run in interactive sessions can go here
end

if type -q fenv
    fenv source ~/.profile
end
~/.local/bin/mise activate fish | source
starship init fish | source

. ~/.config/fish/env.fish
. ~/.config/fish/aliases.fish
. ~/.config/fish/funcs.fish
. ~/.config/fish/keybinds.fish
