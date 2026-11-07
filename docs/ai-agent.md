# AI エージェント セットアップ

雑多なAI agent セットアップ用メモ

## pi

```sh
curl -fsSL https://pi.dev/install.sh | sh
```

## OpenCode

```sh
curl -fsSL https://opencode.ai/v2/install | bash

opencode auth login opencode
```

設定ファイルは chezmoi 管理（`~/.config/opencode/`）。

- `~/.config/opencode/opencode.jsonc` は `home/dot_config/opencode/opencode.jsonc.tmpl` から生成される
- モデルや MCP サーバーを変えるときは chezmoi ソース側（`.tmpl`）を編集して `chezmoi apply` する
- 直接 `~/.config/opencode/opencode.jsonc` を編集した場合は `chezmoi diff` で確認して取り込む

## apm

```sh
brew install apm
apm install -g
```

## browse

`browse` は拡張機能経由でユーザーの Chrome を操作する MCP サーバー。ログイン済みのセッション・拡張機能をそのまま使えるので、認証情報をエージェントに渡さずにブラウザ操作を任せられる。

### インストール

Windows (PowerShell):

```powershell
irm https://opencode.ai/browse/install.ps1 | iex
```

macOS / Linux:

```sh
curl -fsSL https://opencode.ai/browse/install | bash
```

インストーラがやること:

- バイナリを `~/.local/bin/browse`（Windows は `browse.exe`）に配置
- 実行中のエージェント（OpenCode など）に MCP サーバーとして登録
- Chrome を開いて拡張機能とペアリング（接続待ちは最大 90 秒程度）

Chrome に拡張機能が入っていない場合は、開いたページ（Chrome Web Store）から追加する。追加すると自動で接続される。

> [!NOTE]
> `~/.config/opencode/opencode.jsonc` は chezmoi 管理。インストーラは Windows 固定パス（`C:\Users\...`）を書き込むが、テンプレート側に browse のエントリを含めてあるので、インストール後に `chezmoi apply --force ~/.config/opencode/opencode.jsonc` でテンプレートの内容（`.chezmoi.homeDir` 由来のパス）に戻す。
> 再度 `browse install` を実行しても、既存の `browse` エントリは「different server」として変更されない。

### 動作確認

```sh
browse status
```

以下が出ていれば OK。

- `Extension: connected`
- `Relay: http://127.0.0.1:19988 ...`
- `Sessions` / `Targets` が想定どおり

接続できないとき:

```sh
browse doctor    # インストール・ランタイムの診断
browse install   # 拡張機能の再ペアリング
```

### 他のエージェントにも追加

```sh
browse install --agent claude
browse install --agent codex
browse install --agent vscode
browse install --agent claude-desktop
browse install --agent gemini
browse install --agent kiro
browse install --agent copilot
browse install --agent lm-studio
browse install --agent antigravity
```

### 設定

```sh
browse settings
```

- エージェント用タブを開くウィンドウ（現在のウィンドウ / 専用ウィンドウ）
- タブのグループ化（共有 / セッションごと / なし）
- 未使用タブの自動クローズ（10 / 30 / 60 分 / なし）
- エージェントカーソルの表示、ヒューマンライク入力、ハンドオフ通知

### アンインストール

```sh
browse uninstall
```
