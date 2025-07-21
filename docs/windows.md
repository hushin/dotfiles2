# Windows セットアップ手順

`Win-X　Alt-A` ターミナル（管理者）を立ち上げて実行

```
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
winget install -e --id Git.Git --source winget
winget install -e --id twpayne.chezmoi --source winget
```

chezmoi のパス解決のため新しいタブを開いて実行

```
chezmoi init --apply hushin/dotfiles2
```

## キーボード レジストリのキーリマップ

実行後再起動して反映

### JIS キーボード

```
cd $HOME
.\win-setup\change-jis-key.ps1
```
