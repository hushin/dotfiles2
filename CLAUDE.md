## Repository Overview

Chezmoi で管理する dotfiles リポジトリ（Linux/macOS/Windows 対応）

## File Naming Conventions

- `dot_file` → `~/.file`
- `executable_` - 実行権限
- `private_` - 600 権限
- `.tmpl` - テンプレート処理

## Key Commands

- `chezmoi edit <file>` - ソース編集
- `chezmoi apply` - 適用
- `chezmoi diff` - 変更確認

## Template Conditionals

```go
{{- if eq .chezmoi.os "windows" }}
# Windows設定
{{- else if eq .chezmoi.os "linux" }}
# Linux設定
{{- else if eq .chezmoi.os "darwin" }}
# macOS設定
{{- end }}
```

## PowerShell File Encoding

PowerShell ファイル（.ps1）を保存する際は UTF-8 with BOM エンコーディングを使用する
