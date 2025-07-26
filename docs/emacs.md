# Emacs トラブルシューティング

Windows で Doom Emacs が 壊れたとき

```powershell
cd $HOME\.config\emacs
git pull
del -Force .local
doom install
```

Emacs 起動して

- M-x doom/upgrade
- M-x package-upgrade-all
