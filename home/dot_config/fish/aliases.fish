abbr -a ta 'tig --all'
abbr -a ts 'tig status'
abbr -a g 'git'
abbr -a gsh 'git show'
abbr -a gdc 'git diff --cached'
abbr -a gs 'git switch'
abbr -a gsc 'git switch -c'
abbr -a rgh 'rg --hidden'

abbr -a ch 'chezmoi'
abbr -a cha 'chezmoi add'
abbr -a che 'chezmoi edit'
abbr -a chm 'chezmoi merge'
abbr -a chd 'chezmoi diff'
abbr -a chp 'chezmoi apply'
abbr -a chu 'chezmoi update'

type -qa tac || abbr -a tac 'tail -r'

abbr -a reload 'source ~/.config/fish/config.fish'
abbr -a diff 'delta'
abbr -a find 'fd'
abbr -a cdu 'cd-gitroot'
abbr -a tree "tree -NC" # N: 文字化け対策, C:色をつける
abbr -a notes 'rg "TODO|HACK|FIXME|OPTIMIZE"'

abbr -a e 'emacsclient -t -a ""'
abbr -a ekill 'emacsclient -e "(kill-emacs)"'

type -qa open && abbr -a o 'open'

if command -q eza
    abbr -a ls eza --icons
    abbr -a ll eza --icons -lhag --time-style long-iso
    abbr -a lt eza --icons --tree
end

abbr -a --position anywhere --set-cursor F '| fzf'
abbr -a --position anywhere --set-cursor G '| rg'
abbr -a --position anywhere --set-cursor H '| head'
abbr -a --position anywhere --set-cursor L '| less'
abbr -a --position anywhere --set-cursor N '>/dev/null ^/dev/null'
abbr -a --position anywhere --set-cursor N1 '>/dev/null'
abbr -a --position anywhere --set-cursor N2 '^/dev/null'
abbr -a --position anywhere --set-cursor T '| tail'
abbr -a --position anywhere --set-cursor C '| pbcopy'
