# ksec — WSL から PIN なしで読める秘密情報ストア

## 何か

1Password CLI (`op.exe`) は 1Password アプリのオートロック（既定 10 分）に追随するため、
間隔をあけて呼ぶたびに Windows Hello の PIN を要求される。ksec は Windows の
DPAPI（ユーザーログオンに紐づく鍵）で暗号化して `~/.local/share/ksec/` に置き、
読み出し時にプロンプトを出さない。macOS のログイン Keychain と同じモデル。

```
ksec set NAME   →  ~/.local/share/ksec/NAME.dpapi  (DPAPI blob)
ksec get NAME   →  標準出力 (PIN なし)
```

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

- `home/bin/executable_ksec` → `~/bin/ksec`
- `home/.chezmoiscripts/2_wsl/run_once_after_10_ksec.sh` → 初回の `chezmoi apply` で
  interop と保存領域を確認し、未登録なら移行コマンドを案内する（認証ダイアログは出さない）

前提は WSL2 と Windows interop（`powershell.exe`, `wslpath`）。確認:

```sh
ksec doctor
# wsl        : yes
# powershell : /mnt/c/WINDOWS/System32/WindowsPowerShell/v1.0/powershell.exe
# wslpath    : /usr/bin/wslpath
# store      : ~/.local/share/ksec
# round-trip : ok (暗号化/復号に成功)
```

## 使い方

```sh
echo "$TOKEN" | ksec set GITHUB_TOKEN   # パイプ入力（末尾改行も保持）
ksec set GITHUB_TOKEN                   # 引数なしならエコーなしプロンプト
ksec get GITHUB_TOKEN                   # 末尾に改行を 1 つ付けて出力
ksec get -n GITHUB_TOKEN                # 改行なし
ksec list                               # 登録名の一覧
ksec has GITHUB_TOKEN                   # 終了コードのみ
ksec rm GITHUB_TOKEN
```

環境変数に渡す場合:

```sh
export GOOGLE_WORKSPACE_CLI_CLIENT_ID="$(ksec get GWS_CLIENT_ID)"
export GOOGLE_WORKSPACE_CLI_CLIENT_SECRET="$(ksec get GWS_CLIENT_SECRET)"
```

クリップボードへ（WSL から Windows 側へ）:

```sh
ksec get GITHUB_TOKEN | clip.exe
```

## 1Password からの移行

```sh
ksec from-op GWS_CLIENT_ID     'op://Personal/google-workspace-mcp/username'
ksec from-op GWS_CLIENT_SECRET 'op://Personal/google-workspace-mcp/password'
```

`ksec from-op` は `op` / `op.exe` の `read` を 1 回だけ呼ぶので、そのときだけ
1Password のロック解除を求められる。以後は `ksec get` だけで読める。

## セキュリティモデル

- 暗号鍵は Windows のログオン資格情報から LSA が管理する。**同一 Windows ユーザーとして
  動くプロセスは誰でも復号できる**（＝解錠済みの Keychain と同じ強さ）。ディスク暗号化の
  代わりにはならない。
- blob を別ユーザー・別マシンへコピーしても復号できない。
- 保存ファイルは `600`、保存ディレクトリは `700`。WSL の ext4 側に置き、Windows の
  `%USERPROFILE%` には平文も暗号文も出さない。
- 平文はディスクに残らない。パイプで PowerShell に渡す間だけ base64 で受け渡す
  （argv には載らない）。

## 制約

- WSL2 + interop 限定（`~/bin/ksec` は macOS にも配置されるが、実行すると明確に失敗する）
- マシン固有。新しいマシンでは再登録が必要
- 1Password の自動入力・共有・監査ログの対象外
