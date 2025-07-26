# dotfiles

## 日常

[chezmoi コマンド](./docs/chezmoi-cheat-sheet.md)

## Setup

### Windows

`Win-X Alt-A` ターミナル（管理者）を立ち上げて実行

```powershell
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
winget install -e --id Git.Git --source winget
winget install -e --id twpayne.chezmoi --source winget
```

chezmoi のパス解決のため新しいタブを開いて実行

```powershell
chezmoi init --apply hushin/dotfiles2
```

[以降の Windows 設定](./docs/windows.md)

### Ubuntu

```sh
sh -c "$(curl -fsLS get.chezmoi.io)" -- -b $HOME/.local/bin
# chezmoi パス解決に source ~/.profile 必要かも。もしくはshellを新しく開く
chezmoi init --apply hushin/dotfiles2
```

## 手動設定

### 1Password

- セキュリティ
  - システムがアイドル状態になるとロックの時間をお好みで変更
- 開発者
  - SSH エージェントの設定
  - CLI と連携

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

### VSCode

- Setting Sync

### gh CLI

```sh
gh auth login
? Where do you use GitHub? GitHub.com
? What is your preferred protocol for Git operations on this host? SSH
? Generate a new SSH key to add to your GitHub account? No
```

### Emacs

Emacs 起動して実行

```
M-x nerd-icons-install-fonts
```

[Emacs トラブルシューティング](docs/emacs.md)

### Dropbox memo

Dropbox の設定が終わったら設定

Windows

```powershell
New-Item -ItemType SymbolicLink -Path "$env:USERPROFILE\Documents\memo" -Value "$env:USERPROFILE\Dropbox\memo"
```

### tmux(Ubuntu)

ビルド

```sh
cd ~/.config/tmux/plugins/tmux-mem-cpu-load && cmake . && make
```

tmux を起動し、`Ctrl-t I` で プラグインインストール

### このリポジトリ

chezmoi デフォルトだと https なので init 時に `--ssh` をつけたいが、1Password など初期設定が面倒なのでやっていない。
push する前に remote を ssh のものに変更する。

```sh
chezmoi cd
git remote set-url origin git@github.com:hushin/dotfiles2.git
```

### SSH

push 時こういったエラーが出たら `~/.ssh/known_hosts` にホストキーを追加する

> No ED25519 host key is known for github.com and you have requested strict checking.
> Host key verification failed.

```sh
ssh-keyscan -t ed25519 github.com >> ~/.ssh/known_hosts
```
