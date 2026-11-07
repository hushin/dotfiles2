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

## Google Workspace CLI (gws)

Gmail などを CLI から操作するための `gws`。MCP サーバーではない（`gws mcp` は v0.8.0 で削除）。Google 公式サポート製品ではない点に注意。

### インストール

```powershell
npm install -g --allow-scripts=@googleworkspace/cli @googleworkspace/cli
gws --version
```

- npm 11 以降は postinstall script が既定でブロックされるため、バイナリを取得する `--allow-scripts` が必要
- npm の global prefix は `%APPDATA%\npm` に固定済み（[Windows セットアップ](windows.md) の Node.js / npm を参照）
- 更新も同じコマンドを再実行

### OAuth クライアント

GCP プロジェクトで Gmail API を有効化し、OAuth クライアント（デスクトップ アプリ）を作成しておく。

- GCP プロジェクト: `hobby-487823`
- OAuth クライアント: `gws CLI`（デスクトップ アプリ）
- 1Password: `Personal/google-workspace-mcp`（username = client ID、password = client secret）
- 同意画面: External / Testing、テストユーザーに使用する Gmail アカウントを追加

初回ログイン時だけ 1Password から環境変数として注入する。

```powershell
op run --env-file="$HOME\.config\opencode\secrets.env" -- gws auth login --readonly --services gmail
```

- `--scopes gmail` は v0.22.5 ではサービス名を解決せず、そのまま OAuth スコープとして送るため使わない。`--services gmail` と `--readonly` を使う
- ログイン後、gws が client ID/secret を `~/.config/gws/client_secret.json` に保存する（リポジトリには置かない）
- トークンは `~/.config/gws/credentials.enc` に OS キーリングの鍵で AES-256-GCM 暗号化して保存される

### 認証状態と動作確認

```powershell
gws auth status
gws gmail users messages list --params '{\"userId\":\"me\",\"maxResults\":5}'
gws gmail +triage --max 5
```

> [!NOTE]
> PowerShell から `--params` に JSON を渡すときは、内側の `"` を `\"` でエスケープする（そのまま `'{"..."}'` だと quote が剥がれて `Invalid --params JSON` になる）。

### 再認証と 7 日失効

OAuth 同意画面が「テスト」のままだと、リフレッシュトークンが 7 日で失効する。`invalid_grant` などの認証エラーが出たら再ログインする。

```powershell
op run --env-file="$HOME\.config\opencode\secrets.env" -- gws auth login --readonly --services gmail
```

恒久的に避けるには OAuth 同意画面を「本番環境」に公開する（個人利用なら未確認のままでも使えるが、表示と制限は Google の画面に従う）。

### Agent Skills

chezmoi 管理: `home/dot_agents/skills/gws-*` → `~/.agents/skills/`。

- `gws-shared`: 認証・グローバルフラグ・セキュリティ規則
- `gws-gmail`: Gmail API 全体
- `gws-gmail-triage`: 未読サマリー（`+triage`）
- `gws-gmail-read`: 本文・ヘッダー抽出（`+read`）

追加で取得する場合:

```powershell
npx skills add https://github.com/googleworkspace/cli/tree/main/skills/gws-gmail -g --copy -y
chezmoi add --recursive ~/.agents/skills/gws-gmail
```

送信・削除などの書き込み系（`gws-gmail-send` など）は、読み取り運用が安定してから追加する。

