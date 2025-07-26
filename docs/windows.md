# Windows セットアップ

## セットアップ script

個人用のセットアップ script を選択して実行

```powershell
my-setup.ps1
```

### キーボード レジストリのキーリマップ

- `change-hhk-key`: HHK US キーボードを変更
- `change-jis-key`: JIS キーボードを変更

実行後再起動して反映

### 各種アプリインストール

`install-0-all`

## WSL

ref. [WSL のインストール | Microsoft Learn](https://learn.microsoft.com/ja-jp/windows/wsl/install)

`Win-X Alt-A` ターミナル（管理者）を立ち上げて実行

```powershell
wsl --install
```

以下のエラーが出たときは再起動してもう一度実行する

> 必要な機能がインストールされていないため、操作を開始できませんでした。
> エラー コード: Wsl\InstallDistro\Service\RegisterDistro\CreateVm\HCS\HCS_E_SERVICE_NOT_AVAILABLE

## 手動インストール

- [PC TV Plus](https://www.sony.jp/software/store/products/pctv-plus/)
- [EasyCanvas (EL Display Hub)](https://www.easynlight.com/en/easycanvas)

## 手動設定・Windows

### システム → ディスプレイ（デスクトップ右クリックから開ける）

- 夜間モード
  - スケジュール設定

### 個人用設定

- ロック画面
  - ロック画面を個人用に設定: 画像
    - トリビアやヒント のチェックボックスを外す
  - ロック画面の状態: なし

### 時刻と言語

- 入力
  - キーボードの詳細設定
    - 規定の入力方式: Google 日本語入力

### アプリ

- 規定のアプリ
  - PDF を Acrobat に変更

### タスクバー

- 設定
  - ウィジェットのチェックを外す
- 不要なピン留めを削除

## 手動設定・アプリ

### PowerToys

- PowerToys Run
  - Windows System コマンド
    - 「システム コマンドは英語のものではなくローカライズされたものを使用する」チェックを外す
  - ClipboardManager
    - 直接アクティブ化コマンド: `cc`
  - GEmojiSharp
    - 直接アクティブ化コマンド: `:`

### NVIDIA

グラフィックスドライバ更新

### Steam

- 設定 > ストレージ > ドライブを追加 で 外付け SSD 指定、デフォルトに設定

## クリーンインストール手順

- [事前にバックアップする](backup.md)
- 初期化時、念のため外付け SDD 等は外しておく。

[Windows 11 Clean Installation](https://www.microsoft.com/ja-jp/windowsinsider/cleaninstall) にあるように、USB フラッシュ ドライブに Windows インストール メディアを入れて実行する。
