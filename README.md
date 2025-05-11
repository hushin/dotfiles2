```
sh -c "$(curl -fsLS get.chezmoi.io)" -- -b $HOME/.local/bin
chezmoi init --apply git@github.com:hushin/dotfiles2.git
```


## memo

### tmux

`Ctrl-t I` で プラグインインストール

```
cd ~/.config/tmux/plugins/tmux-mem-cpu-load
cmake .
make
```
