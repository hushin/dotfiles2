# バックアップ

初期化前にデバイスからファイル・コンテンツをバックアップしておく

## チェックリスト

- [ ] git リポジトリ
  - [ ] ghq 管理
    - 詳しくは後述
    - Windows の場合、Windows 側と WSL 側両方見る
  - [ ] dotfiles
    - `chezmoi diff` があって取り込みたかったら `chezmoi re-add`
    - git push
- [ ] ファイル(ホームディレクトリ以下)
  - [ ] デスクトップ
  - [ ] ドキュメント
  - [ ] 画像
  - [ ] 動画
  - [ ] ダウンロード
  - [ ] その他作業フォルダ
  - [ ] home 以下 その他
- [ ] Chrome
  - [ ] ブラウザタブ
  - [ ] OneTab URL
  - [ ] Tampermonkey
  - [ ] Stylebot
- [ ] インストール済みのアプリをメモ
  - [ ] 必要に応じて dotfiles 更新
- [ ] アプリの設定
  - [ ] Blender
  - Steam … 基本的に別ディスクに退避させているので不要

## ghq フォルダ

ghq 以下のリポジトリを確認

### そもそも git 管理してないディレクトリを調べる

(ghq root)/github.com/hoge/fuga のような階層になっていること前提

```sh
# fish
cd (ghq root)
find . -maxdepth 3 -mindepth 3 -type d | xargs -I@ bash -c '[ -e @/.git ] || echo @'
```

### uncommitted, untracked, unpushed がないか調べる

```sh
# macOS, Linux
check-ghq.sh
# Windows
check-ghq.ps1
```

## インストール済みアプリの確認

### Windows

```powershell
dump-installed-apps.ps1
```

## Blender

```powershell
cd "$env:APPDATA\Blender Foundation\Blender\"
start .
# ディレクトリをコピー
```

使っているバージョンに cd して chezmoi add

```powershell
chezmoi add .\scripts\addons\pie_menu_editor_data\backups
chezmoi add .\scripts\presets\keyconfig\hushin_keymap.py
```
