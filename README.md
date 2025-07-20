# dotfiles

```
sh -c "$(curl -fsLS get.chezmoi.io)" -- -b $HOME/.local/bin
chezmoi init --apply git@github.com:hushin/dotfiles2.git
```

## Setup

### Windows

`Win-X` ターミナル（管理者）を立ち上げて実行

```
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
winget install -e --id Git.Git --source winget
winget install -e --id twpayne.chezmoi --source winget
```

```
chezmoi init --apply hushin/dotfiles2
```

JIS キーボードの時

```
cd win-setup
.\change-jis-key.ps1
```

## memo

### tmux

`Ctrl-t I` で プラグインインストール

```
cd ~/.config/tmux/plugins/tmux-mem-cpu-load
cmake .
make
```
