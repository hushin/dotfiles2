# macOS セットアップ

## ソフトウェアアップデート

```sh
# 確認
softwareupdate -l
# アップデート
softwareupdate -ia
```

## システム設定

- Touch ID
- キーボード > キーボードショートカット
  - 入力ソース: ^スペース 外す
  - Spotlight 検索: オフ
  - キーボード: 次のウインドウを操作対象にする Cmd-Shift-2
- ディスプレイ
  - Night Shift を日の入りから日の出まで

## アプリ起動

- Karabiner-elements
- 1Password
- Rectangle
- RayCast
- Dropbox

## 各アプリ

- Google 日本語入力
  - ¥ キーで バックスラッシュを入力
  - キー設定: ATOK から Ctrl-k で全角カタカナに変換
- Raycast
  - すべて復元: Dropbox に保存している設定を Import する
  - ゼロから手動設定メモ
    - `Alt + Space`
    - Extensions
      - Define Word: `d`

## アプリ更新

```
update-all-macos.sh
```
