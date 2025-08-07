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
- キーボード
  - キーボードショートカット
    - 入力ソース: ^スペース 外す
    - Spotlight 検索: オフ
    - キーボード: 次のウインドウを操作対象にする Cmd-Shift-2
  - テキスト入力 > 入力ソース > 編集
    - すべての入力ソース
      - 文頭を自動的に大文字にする: オフ
      - スペースバーを 2 回押してピリオドを入力: オフ
      - スマート引用符とスマートダッシュを使用: オフ
    - 日本語を消してひらがな（Google）を追加
- ディスプレイ
  - Night Shift を日の入りから日の出まで

## アプリ起動

- Karabiner-elements
- 1Password
- Dropbox
- RayCast
- Rectangle

## 各アプリ設定

### Google 日本語入力

- ¥ キーで バックスラッシュを入力
- キー設定: ATOK
- 入力補助
  - 変換前のアルファベット、数字を半角に設定

### Raycast

- すべて復元: Dropbox に保存している設定を Import する
- ゼロから手動設定メモ
  - `Alt + Space`
  - Extensions
    - Define Word: alias `d`
    - Clipboard History: `Cmd+shift+v`
    - Script Commands
      - https://github.com/raycast/script-commands
      - `~/raycast-commands`
    - Pomodoro
    - Visual Studio Code

### Rectangle

[RectangleConfig.json](../etc/config/RectangleConfig.json) をインポート

## アプリ更新

```
update-all-macos.sh
```
