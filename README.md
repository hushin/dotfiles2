# dotfiles

## 日常

[chezmoi コマンド](./docs/chezmoi-cheat-sheet.md)

## Setup

### macOS

```sh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
eval "$(/opt/homebrew/bin/brew shellenv)"
brew install chezmoi
chezmoi init --apply hushin/dotfiles2
```

- [macOS 設定](./docs/macos.md)

### Windows

`Win-X Alt-A` ターミナル（管理者）を立ち上げて実行

```powershell
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
winget install -e --id Git.Git --source winget
winget install -e --id twpayne.chezmoi --source winget
```

chezmoi のパス解決のため新しいタブを開いて実行

```powershell
git config --global core.autocrlf false
chezmoi init --apply hushin/dotfiles2
```

```powershell
# PSFzf のエラーが出ているときは手動で実行する
Install-Module PSFzf -Scope CurrentUser
```

[以降の Windows 設定](./docs/windows.md)

### Ubuntu

```sh
sh -c "$(curl -fsLS get.chezmoi.io)" -- -b $HOME/.local/bin
# WSL は chezmoi パス解決のためshellを新しく開く
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
  - chrome://extensions/
    - ユーザースクリプトを許可する
  - ダッシュボード → ユーティリティ → クラウド → Google Drive からインポート
- Stylebot
  - Google ドライブを介した手動同期
- [Mouse Dictionary](https://qiita.com/wtetsu/items/c43232c6c44918e977c9)
  - 英辞郎 の 辞書データをインポート
- uBlock Origin Lite
  - 開発者モード をオン
  - 開発で以下を設定（デフォルトはフィルタリングなし）

```
フィルタリングなし:
  - all-urls
基本:
最適:
完全:
```

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

```sh
M-x nerd-icons-install-fonts
```

Windows は適当なフォルダを指定、 NFM.ttf を開いて手動でインストールする。

[Emacs トラブルシューティング](docs/emacs.md)

### Dropbox memo

Dropbox の設定が終わったら設定

macOS

```sh
ln -s "/Users/$(whoami)/Library/CloudStorage/Dropbox" ~/Dropbox
ln -s ~/Dropbox/memo ~/Documents/memo
```

Windows

```powershell
New-Item -ItemType SymbolicLink -Path "$env:USERPROFILE\Documents\memo" -Value "$env:USERPROFILE\Dropbox\memo"
```

WSL

```sh
mkdir -p ~/Documents && ln -s /mnt/c/Users/$(whoami)/Documents/memo ~/Documents/memo
```

### tmux(Ubuntu)

tmux を起動し、`Ctrl-t I` で プラグインインストール

その後 tmux-mem-cpu-load をビルド

```sh
cd ~/.config/tmux/plugins/tmux-mem-cpu-load && cmake . && make
```

### このリポジトリ

chezmoi デフォルトだと https なので init 時に `--ssh` をつけたいが、1Password など初期設定が面倒なのでやっていない。
push する前に remote を ssh のものに変更する。

```sh
# chezmoi cd してから
git remote set-url origin git@github.com:hushin/dotfiles2.git
```

### SSH

push 時こういったエラーが出たら `~/.ssh/known_hosts` にホストキーを追加する

> No ED25519 host key is known for github.com and you have requested strict checking.
> Host key verification failed.

```sh
ssh-keyscan -t ed25519 github.com >> ~/.ssh/known_hosts
```
