# Doom Emacs トラブルシューティング

## Couldn’t find ripgrep in your PATH

```sh
doom env
```

で環境変数を作り直す

## Windows で Doom Emacs が 壊れたとき

`doom sync --rebuild` する

それでもだめなら

```powershell
cd $HOME\.config\emacs
git pull
del -Force .local
doom install
doom sync
```

Emacs 起動して

- M-x doom/upgrade
- M-x package-upgrade-all
