# dotfiles

## Setup

### Windows

`Win-X Alt-A` ターミナル（管理者）を立ち上げて実行

```
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
winget install -e --id Git.Git --source winget
winget install -e --id twpayne.chezmoi --source winget
```

chezmoi のパス解決のため新しいタブを開いて実行

```
chezmoi init --apply hushin/dotfiles2
```

[他の設定](./docs/windows.md)

### Ubuntu(Devcontainer)

```
sh -c "$(curl -fsLS get.chezmoi.io)" -- -b $HOME/.local/bin
chezmoi init --apply git@github.com:hushin/dotfiles2.git
```

## 手動設定

### Chrome

- Surfingkeys
  - chrome://extensions/
    - ユーザースクリプトを許可する
  - Advanced mode:
    - https://raw.githubusercontent.com/hushin/dotfiles2/refs/heads/main/etc/SurfingkeysSetting.js
    - [src](./etc/SurfingkeysSetting.js)
- Tampermonkey
  - ダッシュボード → ユーティリティ → クラウド → Google Drive からインポート
- Stylebot
  - Google ドライブを介した手動同期
- [Mouse Dictionary](https://qiita.com/wtetsu/items/c43232c6c44918e977c9)
  - 英辞郎 の 辞書データをインポート

### tmux

`Ctrl-t I` で プラグインインストール

```
cd ~/.config/tmux/plugins/tmux-mem-cpu-load
cmake .
make
```
