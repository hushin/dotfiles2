# fzf
set -x FZF_DEFAULT_COMMAND 'rg -g "" --hidden --ignore ".git"'
set -x FZF_DEFAULT_OPTS "--height 70% --layout=reverse --border --ansi --inline-info"

# tmux のために設定
set -g SHELL "$(which fish)"
