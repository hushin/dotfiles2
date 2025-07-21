# Windows セットアップ

## キーボード レジストリのキーリマップ

実行後再起動して反映

### JIS キーボード

```
cd $HOME
.\win-setup\change-jis-key.ps1
```

## WSL

ref. [WSL のインストール | Microsoft Learn](https://learn.microsoft.com/ja-jp/windows/wsl/install)

`Win-X Alt-A` ターミナル（管理者）を立ち上げて実行

```
wsl --install
```

以下のエラーが出たときは再起動してもう一度実行する

> 必要な機能がインストールされていないため、操作を開始できませんでした。
> エラー コード: Wsl\InstallDistro\Service\RegisterDistro\CreateVm\HCS\HCS_E_SERVICE_NOT_AVAILABLE
