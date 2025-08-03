# chezmoi チートシート

https://www.chezmoi.io/user-guide/daily-operations/

## 日常的な更新作業

```bash
# 1. ファイルを追加・編集
chezmoi add ~/.new_config
chezmoi edit ~/.bashrc

# 2. 変更を確認
chezmoi diff

# 3. 適用
chezmoi apply

# 4. Git操作
chezmoi cd
git add .
git commit -m "Update configurations"
git push

# リモートから更新
chezmoi update
```

## 基本コマンド

### ファイル管理

```bash
# ファイルをchezmoiに追加
chezmoi add ~/.bashrc

# ディレクトリ全体を追加
chezmoi add ~/.config/git

# 変更されたファイルをchezmoiに再追加
chezmoi re-add ~/.bashrc

# 変更を確認
chezmoi diff

# ★ 変更を適用
chezmoi apply

# script は実行せずファイルの反映だけ
chezmoi apply --exclude=scripts

# ドライラン（実際には適用しない）
chezmoi apply --dry-run
```

### 編集

```bash
# chezmoi のソースディレクトリに移動
chezmoi cd

# chezmoiでファイルを編集
chezmoi edit ~/.bashrc

# ファイルを編集して適用
chezmoi edit -a ~/.bashrc
```

### ファイル属性の変更

```bash
# すでに chezmoi 管理のファイルにテンプレート機能を追加
chezmoi chattr +t ~/.bashrc
```

### Git 操作

```bash
# chezmoiディレクトリ内での git status
chezmoi git status

# 変更をGitにコミット
chezmoi git add .
chezmoi git commit -m "Update dotfiles"
chezmoi git push

# リモートから更新
chezmoi git pull
chezmoi apply

# ワンライナーで更新・適用
chezmoi update
```

### 状態確認

```bash
# 管理されているファイル一覧
chezmoi managed

# 変更されたファイル一覧
chezmoi status

# 未追跡ファイルの確認
chezmoi unmanaged
```

### その他

```bash
# 設定を確認
chezmoi doctor

# データ（使える変数）の確認
chezmoi data

# バックアップから復元
chezmoi diff --reverse
```

## ファイル命名規則

### 基本的な命名

- `dot_bashrc` → `~/.bashrc`
- `dot_config/git/config` → `~/.config/git/config`
- `private_dot_ssh/config` → `~/.ssh/config` (private)

### 属性付きファイル

```bash
# 実行可能ファイル
executable_dot_local/bin/script

# プライベートファイル（パーミッション600）
private_dot_ssh/id_rsa

# 読み取り専用ファイル
readonly_dot_file

# 空のファイル
empty_dot_keep
```

### テンプレート

```bash
# テンプレートファイル
dot_gitconfig.tmpl

# 実行可能テンプレート
executable_dot_script.tmpl
```

## テンプレート機能

### 基本的なテンプレート構文

```go
# ~/.local/share/chezmoi/dot_gitconfig.tmpl
[user]
    name = {{ .name }}
    email = {{ .email }}

{{- if eq .chezmoi.os "darwin" }}
[credential]
    helper = osxkeychain
{{- end }}
```

### データファイル（.chezmoi.toml.tmpl）

```toml
[data]
  command = "code"
  args = ["--wait"]
  gituser = {{ $gituser | quote }}
  email = {{ $email | quote }}
```

### 条件分岐の例

```go
{{- if eq .chezmoi.os "linux" }}
# Linux固有の設定
{{- else if eq .chezmoi.os "darwin" }}
# macOS固有の設定
{{- else if eq .chezmoi.os "windows" }}
# Windows固有の設定
{{- end }}

{{- if .personal }}
# 個人用設定
{{- else }}
# 仕事用設定
{{- end }}

{{ if eq .chezmoi.os "linux" }}
{{   if (.chezmoi.kernel.osrelease | lower | contains "microsoft") }}
# WSL-specific code
{{   end }}
{{ end }}

{{- if eq (env "REMOTE_CONTAINERS") "true" }}
# devcontainer 用設定
{{- end }}
```

## スクリプト

`.chezmoiscripts` 内 `run_once_after_` ファイルををいています。適用後一度だけ実行されるスクリプトになります。

https://www.chezmoi.io/user-guide/use-scripts-to-perform-actions/

## その他機能

### 暗号化

https://www.chezmoi.io/user-guide/encryption/

```bash
# 暗号化ファイルを追加
chezmoi add --encrypt ~/.ssh/id_rsa

# 暗号化されたファイルを編集
chezmoi edit ~/.ssh/id_rsa

# 暗号化設定（.chezmoi.yaml）
encryption: "gpg"
gpg:
  recipient: "your-key-id"
```

### 外部ファイルの取得

```bash
# .chezmoiexternal.yaml
".oh-my-zsh":
  type: "git-repo"
  url: "https://github.com/ohmyzsh/ohmyzsh.git"
  refreshPeriod: "168h"
```
