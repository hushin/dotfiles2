# AI エージェント セットアップ

chezmoi で自動化できない手動作業（認証・Chrome 拡張ペアリング・外部サービス設定）のみ記載。

ツール本体のインストールと設定ファイル（`~/.claude/`, `~/.pi/`, `~/.config/opencode/`, `~/.gemini/`, `~/.apm/`, `~/.agents/skills/`）は chezmoi で管理している。

## OpenCode

```sh
opencode auth login opencode
```

## pi

`~/.pi/agent/models.json` が `ksec` 経由で Deploy の API キーを読むため、初回のみ 1Password から登録する（[ksec](./ksec.md)）。

```sh
ksec from-op DEPLOY_API_KEY "op://Personal/deploy com api key/credential"
```

## browse

Chrome 拡張のインストールとペアリングのみ手動（MCP 設定・Windows の relay 自動起動・WSL のトークン同期は chezmoi 管理）。

### 初回セットアップ

- **Windows**
  ```powershell
  irm https://opencode.ai/browse/install.ps1 | iex
  chezmoi apply --force ~/.config/opencode/opencode.jsonc
  ```
- **macOS**
  ```sh
  curl -fsSL https://opencode.ai/browse/install | bash
  chezmoi apply --force ~/.config/opencode/opencode.jsonc
  ```
- **WSL**（Windows 側でペアリング済みの前提）

  ```sh
  curl -fsSL https://opencode.ai/browse/install | bash -s -- --no-setup
  browse install --no-open
  chezmoi apply
  browse status
  ```

- インストーラが開くページから Chrome 拡張を追加すると自動でペアリングされる
- インストーラが `opencode.jsonc` に固定パスを書くため、`chezmoi apply --force` でテンプレートに戻す

### 運用メモ

- **WSL で `browse restart` を実行しない**: Windows の relay を止めて WSL 側に別 relay を立ててしまう。再起動は Windows 側で行う（古い relay が残る場合は `Stop-Process -Name browse` 後に `browse restart`）
- 接続できないとき: `browse status` / `browse pair`

## Google Workspace CLI (gws)

OAuth 認証と秘密情報のみ手動（ツール本体とスキル `gws-*` は chezmoi 管理）。

### OAuth クライアント

- GCP プロジェクト: `hobby-487823`（Gmail API 有効化済み）
- OAuth クライアント: `gws CLI`（デスクトップ アプリ）
- 1Password: `Personal/google-workspace-mcp`（username = client ID / password = client secret）
- 同意画面: External / 本番環境（未確認アプリのため、認可画面では「詳細」→「gws CLI（安全ではないページ）に移動」で続行）

### ログイン / 再認証

トークン（`~/.config/gws/credentials.enc`）は OS ごとの鍵で暗号化されるため、Windows と WSL でそれぞれログインする。

- **Windows**
  ```powershell
  op run --env-file="$HOME\.config\opencode\secrets.env" -- gws auth login --readonly --services gmail
  ```
- **WSL / Linux**（初回は `GWS_CLIENT_ID` / `GWS_CLIENT_SECRET` を [ksec](./ksec.md) へ登録）
  ```sh
  export GOOGLE_WORKSPACE_CLI_CLIENT_ID="$(ksec get GWS_CLIENT_ID)"
  export GOOGLE_WORKSPACE_CLI_CLIENT_SECRET="$(ksec get GWS_CLIENT_SECRET)"
  gws auth login
  ```

### 注意点

- PowerShell から `--params` に JSON を渡すときは内側の `"` を `\"` でエスケープする
- `gws auth status` で `The OAuth client was deleted.` が出たら `client_secret.json` を差し替えて再ログインする
