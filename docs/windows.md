# Windows セットアップ

## キーボード レジストリのキーリマップ

実行後再起動して反映

### JIS キーボード

```powershell
cd $HOME
.\win-setup\change-jis-key.ps1
```

## WSL

ref. [WSL のインストール | Microsoft Learn](https://learn.microsoft.com/ja-jp/windows/wsl/install)

`Win-X Alt-A` ターミナル（管理者）を立ち上げて実行

```powershell
wsl --install
```

以下のエラーが出たときは再起動してもう一度実行する

> 必要な機能がインストールされていないため、操作を開始できませんでした。
> エラー コード: Wsl\InstallDistro\Service\RegisterDistro\CreateVm\HCS\HCS_E_SERVICE_NOT_AVAILABLE

## 手動設定

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

## タスクバー

- 設定
  - ウィジェットのチェックを外す
- 不要なピン留めを削除
