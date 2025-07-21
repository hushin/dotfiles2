# chezmoi よく使うコマンド

## 日常的な操作

### 設定変更の適用

変更された設定をホームディレクトリに適用する

```bash
chezmoi apply
```

script は実行せずファイルの反映だけ

```bash
chezmoi apply --exclude=scripts
```

### 差分確認

現在の状態とターゲット状態の差分を表示

```bash
chezmoi diff
```

### 状態確認

管理されているファイルの状態を表示

```bash
chezmoi status
```

### ファイルの編集

ソース状態でファイルを編集（例: `chezmoi edit ~/.vimrc`）

```bash
chezmoi edit <file>
```

### ファイルの再追加

変更されたファイルをソースに再追加

```bash
chezmoi re-add <file>
```

### ソースディレクトリに移動

chezmoi のソースディレクトリに移動

```bash
chezmoi cd
```

## 検証・診断

### 状態の検証

ターゲット状態が正しく適用されているか検証

```bash
chezmoi verify
```

### システム診断

システムの潜在的な問題をチェック

```bash
chezmoi doctor
```

## Git 操作（ソースディレクトリ内で）

### Git 状態確認

ソースディレクトリ内での git status

```bash
chezmoi git status
```

### 変更を Git に追加

ソースディレクトリ内での git add

```bash
chezmoi git add .
```

### コミット

ソースディレクトリ内での git commit

```bash
chezmoi git commit -m "message"
```

## その他

### 管理対象のファイル一覧

chezmoi が管理しているファイルの一覧を表示

```bash
chezmoi managed
```

### テンプレート実行

テンプレートをその場で実行して結果を確認

```bash
chezmoi execute-template
```

### 更新

リモートリポジトリから最新の変更を取得して適用

```bash
chezmoi update
```
