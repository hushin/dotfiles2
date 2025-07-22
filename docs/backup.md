# バックアップ

初期化前にデバイスからファイル・コンテンツをバックアップしておく

## チェックリスト

- [ ] git リポジトリ
  - [ ] ghq 管理（後述）
  - [ ] dotfiles
- [ ] ファイル(ホームディレクトリ以下)
  - [ ] デスクトップ
  - [ ] ドキュメント
  - [ ] 画像
  - [ ] 動画
  - [ ] ダウンロード
  - [ ] その他作業フォルダ
- [ ] Chrome
  - [ ] ブラウザタブ
  - [ ] OneTab URL
  - [ ] Tampermonkey
  - [ ] Stylebot
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

TODO
