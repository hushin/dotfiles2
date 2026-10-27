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

### プロジェクトバックアップ (restic)

`setup-project-backup` は restic のパスワードを 1Password から取得します。通常のバックアップではパスワードをディスクに保存しません。

```powershell
# 1Password に Password アイテムがなければ、確認のうえ自動生成して作成する
my-setup.ps1 setup-project-backup

# 別の vault / item を使う場合は setup script を直接実行する
~\bin-win\setup-scripts\setup-project-backup.ps1 -OpVault "Personal" -OpItem "restic-backup"
```

- 既定の secret reference: `op://Private/restic/password`
- 使用する reference は `~/.config/restic/1password-ref.txt` に保存されます（パスワード自体は保存しません）。
- 1Password CLI (`op`) がインストールされ、1Password アプリとの CLI 連携が有効で、ロック解除されている必要があります。
- `1password-ref.txt` がまだない場合、旧 `password.txt` があれば従来どおりそちらを使用します（移行期間用）。
- スケジュールタスクは 30 分ごとに実行されます。1Password がロックされているとバックアップは失敗し、ログにエラーが残ります。
- `RESTIC_OP_REF` 環境変数で secret reference を一時的に上書きできます。

既存の `~/.config/restic/password.txt` がある場合は、先に 1Password のパスワードへ移行します。

```powershell
# 移行対象の確認
migrate-restic-password.ps1 -DryRun

# 移行 (旧パスワードは password.txt または対話入力から取得)
migrate-restic-password.ps1
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

Ubuntu のインストールが完了したら、WindowsTerminal の設定が変わっているので取り込む

```powershell
chezmoi re-add C:\Users\n1\AppData\Local\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\
```

## 手動インストール

- [PC TV Plus](https://www.sony.jp/software/store/products/pctv-plus/)
- [EasyCanvas (EL Display Hub)](https://www.easynlight.com/en/easycanvas)

## 手動設定・Windows

`Win + I` で設定を開く

### システム

#### ディスプレイ（デスクトップ右クリックからも開ける）

- 夜間モード
  - スケジュール設定

#### サウンド（右下のサウンド右クリックからも開ける）

- スピーカー、マイクを設定

#### 通知

- 追加の設定
  - 外す: Windows のウェルカム エクスペリエンス～
  - 外す: Windows を使用する際のヒントや提案～

#### マルチタスク

- ウィンドウのスナップ
  - 外す: ウィンドウをスナップしたときに、その次にスナップする対象を提案

#### 詳細設定

- ON: タスクの終了

### 個人用設定（デスクトップ右クリックからも開ける）

- ロック画面
  - ロック画面を個人用に設定: 画像
    - トリビアやヒント のチェックボックスを外す
  - ロック画面の状態: なし

### 時刻と言語

- 入力
  - キーボードの詳細設定
    - 規定の入力方式: Google 日本語入力
  - (US キーボード配列になっていないとき) 言語と地域 → 日本語 → 言語のオプション → キーボードレイアウト: 101/102 キー

### タスクバー

- 設定
  - ウィジェットのチェックを外す
- 不要なピン留めを削除

### アプリ

- 規定のアプリ
  - PDF を Acrobat に変更

### アカウント

- パスキー → 詳細オプション
  - パスキー マネージャー の 1Password を有効化

## 手動設定・アプリ

### Google 日本語入力

- キー設定: ATOK
- 入力補助
  - 変換前のアルファベット、数字を半角に設定
- その他
  - ホットキー(Ctrl+Shift)による IME の切り替えを無効化する

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

## 初期化手順

### 事前準備

- [事前にバックアップする](backup.md)
- 初期化時、念のため外付け SDD 等は外しておく。

Windows 設定から初期化か、クリーンインストール

### A: Windows 設定から初期化

`Win-I` → システム → 回復 → この PC をリセット → すべて削除する → クラウドからダウンロード

### B: クリーンインストール

[Windows 11 Clean Installation](https://www.microsoft.com/ja-jp/windowsinsider/cleaninstall) にあるように、USB フラッシュ ドライブに Windows インストール メディアを入れて実行する。

> [!WARNING]
> PC にプリインストールされているソフトはインストールされない

> [!NOTE]
> Windows セットアップ画面でシステムストレージのドライブが認識されないことがある
> インテル VMD ドライバー のインストールが必要そう（未確認）
