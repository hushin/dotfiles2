# ksec — PIN なしで読めるローカル秘密情報ストア

## 何か

1Password CLI (`op.exe`) は 1Password アプリのオートロック（既定 10 分）に追随するため、
間隔をあけて呼ぶたびに Windows Hello の PIN を要求される。ksec は一度取り出した値を
マシンローカルの仕組みで暗号化して保存し、以後は `ksec get` だけで読めるようにする。

```
ksec set NAME   →  ~/.local/share/ksec/NAME.<backend>   (暗号化 blob)
ksec get NAME   →  標準出力 (プロンプトなし)
```

コマンドと保存の意味は全プラットフォームで同じ。暗号化だけが環境ごとに変わる。

| backend | 使う環境 | 暗号化 | 保存先 |
|---|---|---|---|
| `dpapi` | WSL2 / Git Bash (Windows) | Windows DPAPI (ユーザーログオンに紐づく鍵) | `~/.local/share/ksec/NAME.dpapi` |
| `keychain` | macOS | ログイン Keychain のマスターキー + openssl AES-256 | `~/.local/share/ksec/NAME.keychain` |

`listing` / `has` / `rm` はファイル操作なので、Keychain を触るのは実際に読み書きするときだけ。

## 使い分け

1Password を解錠したままにしたくない場面で、スクリプトが高頻度に読むトークンだけを
ksec に置く。

| | 置くもの |
|---|---|
| 1Password | 長期間使う秘密、ローテーションする秘密、人間が読む秘密、他端末と共有する秘密 |
| ksec | スクリプト・エージェントが繰り返し読むトークン（GWS の OAuth クライアント等） |

ksec の実体はマシン固有で chezmoi では同期しないため、**二重管理**になる。
1Password 側を変更したら `ksec from-op` で入れ直す。

## セットアップ

chezmoi が配置するもの:

- `home/bin/executable_ksec` → `~/bin/ksec`（全プラットフォーム共通の実装）
- `home/.chezmoiscripts/2_wsl/run_once_after_10_ksec.sh` → WSL で初回の `chezmoi apply` 時に
  interop と保存領域を確認し、未登録なら移行コマンドを案内する

確認:

```sh
ksec doctor
# os         : Darwin
# shell      : posix
# backend    : keychain (auto)
# store      : ~/.local/share/ksec
# security   : /usr/bin/security
# openssl    : /usr/bin/openssl
# round-trip : ok (暗号化/復号に成功)
```

`doctor` は実際に往復（暗号化→復号）して照合するので、バックエンドが壊れていれば
終了コード 1 で落ちる。

## 使い方

```sh
echo "$TOKEN" | ksec set GITHUB_TOKEN   # パイプ入力（末尾改行も保持）
ksec set GITHUB_TOKEN                   # TTY ならエコーなしプロンプト
ksec get GITHUB_TOKEN                   # 末尾に改行を 1 つ付けて出力
ksec get -n GITHUB_TOKEN                # 改行なし
ksec list                               # 登録名の一覧
ksec has GITHUB_TOKEN                   # 存在チェック（0=あり / 1=なし / 2=不正名）
ksec rm GITHUB_TOKEN
```

`get` は「保存値 + 改行 1 つ」を出す。保存値が改行で終わっていれば 2 つになる
（正規化はしない）。`get -n` は保存されたバイト列をそのまま出す。

環境変数に渡す場合:

```sh
export GOOGLE_WORKSPACE_CLI_CLIENT_ID="$(ksec get GWS_CLIENT_ID)"
export GOOGLE_WORKSPACE_CLI_CLIENT_SECRET="$(ksec get GWS_CLIENT_SECRET)"
```

## このリポジトリでの登録

| name | 取得元 | 使う側 |
|---|---|---|
| `GWS_CLIENT_ID` / `GWS_CLIENT_SECRET` | `op://Personal/google-workspace-mcp/{username,password}` | gws の再ログイン（[ai-agent.md](./ai-agent.md)） |
| `JINA_API_KEY` | `op://Personal/JINA_API_KEY/password` | `~/bin/jsearch`（`$JINA_API_KEY` → ksec → `op` の順に解決） |
| `DEPLOY_API_KEY` | `op://Personal/deploy/password` | `~/.pi/agent/models.json` の `apiKey` |

## 1Password からの移行

```sh
ksec from-op GWS_CLIENT_ID     'op://Personal/google-workspace-mcp/username'
ksec from-op GWS_CLIENT_SECRET 'op://Personal/google-workspace-mcp/password'
```

`ksec from-op` は `op` / `op.exe` の `read` を 1 回だけ呼ぶので、そのときだけ
1Password のロック解除を求められる。以後は `ksec get` だけで読める。

## マシンを分ける場合

DPAPI の blob も Keychain の項目もマシン・ユーザーに紐づくため、**別マシンへはコピーできない**。
新しいマシンでは `ksec from-op` で取り直す（これが唯一の正しい移行）。

同じ Windows ユーザーであれば WSL と Windows ネイティブで blob を共有できる。
DPAPI はパスではなくユーザーに紐づくため、ファイルをコピーすれば読める:

```sh
# WSL → Windows ネイティブ（同じ Windows ユーザー）
dest=/mnt/c/Users/$USER/.local/share/ksec
install -d -m 700 "$dest"; cp -p ~/.local/share/ksec/*.dpapi "$dest"/
```

## セキュリティモデル

- **dpapi**: 暗号鍵は Windows のログオン資格情報から LSA が管理する。同一 Windows
  ユーザーとして動くプロセスは誰でも復号できる（解錠済みの Keychain と同じ強さ）。
  blob を別ユーザー・別マシンへコピーしても復号できない。
- **keychain**: マスターキーはログイン Keychain に 1 つだけ置き、秘密の本体はそのキーで
  AES-256 暗号化したファイル。マスターキーを消せば blob は復号できない。
- 平文はディスクに残らない。`dpapi` は base64 にして stdin で PowerShell に渡す（argv に
  載らない）。`keychain` は `-pass env:` で openssl に渡し、Keychain への初回登録も
  `security -i`（stdin）を使うので argv に載らない。
- 保存ファイルは 600、ディレクトリは 700（Windows 側は ACL 継承）。
- 同一ユーザーの他プロセスは読める。ディスク暗号化の代わりにはならない。

## 制約

- 対応環境は WSL2、Git Bash / MSYS、macOS。素の Linux は未対応
  （`KSEC_BACKEND` を明示すれば動くが、鍵管理は自分で用意することになる）
- マシン固有。新しいマシンでは再登録が必要
- 1Password の自動入力・共有・監査ログの対象外

## 実装メモ

過去に WSL を壊した 3 つの失敗から、次の制約を守っている。

- **PowerShell 用の `.ps1` は UTF-8 BOM 付き・ASCII のみ。** Windows PowerShell 5.1 は
  BOM の無いファイルを OEM コードページで読むため、非 ASCII があると
  `式またはステートメントのトークン '}' を使用できません` になる
- **`ConvertTo-SecureString` を使わない。** pwsh 7 から起動すると PSModulePath が混ざり
  5.1 側でモジュール解決に失敗する。DPAPI は `ProtectedData` を直接呼ぶ
- **一時ファイルを関数内の EXIT トラップで消さない。** `set -u` 下で `local` を参照して
  `ksec set` が常に終了コード 1 になっていた（しかも平文相当の base64 が `/tmp` に残った）
- **`~/bin/ksec` は `bin-win` に依存しない。** `bin-win` は Windows 以外では配置されないため、
  そこへ転送すると WSL で必ず失敗する

各ウィンドウズ経路（cmd / PowerShell / Git Bash）は同じ `~/bin/ksec` に到達する。
