#!/bin/sh
# ksec: WSL から Windows DPAPI で秘密情報を読むための前提チェック。
#
# ksec 自体は ~/bin/ksec (chezmoi: home/bin/executable_ksec) に配置される。
# ここでは interop と保存領域が使えることを確認し、まだ何も入っていない場合に
# 1Password からの移行コマンドを案内するだけ。認証ダイアログは出さない。
set -eu

# WSL 以外では何もしない (macOS / 素の Linux には Windows DPAPI が無い)
grep -qi microsoft /proc/version 2>/dev/null || exit 0

ksec="$HOME/bin/ksec"
if [ ! -x "$ksec" ]; then
    echo "ksec: $ksec が無いためセットアップをスキップします" >&2
    exit 0
fi

if ! "$ksec" doctor; then
    echo "ksec: interop が使えないため利用できません (WSL の interop 設定を確認)" >&2
    exit 0
fi

if [ -z "$("$ksec" list 2>/dev/null || true)" ]; then
    cat >&2 <<'EOS'

ksec: まだ何も登録されていません。1Password から 1 回だけ取り出して保存します:

  ksec from-op GWS_CLIENT_ID     'op://Personal/google-workspace-mcp/username'
  ksec from-op GWS_CLIENT_SECRET 'op://Personal/google-workspace-mcp/password'

以後は `ksec get GWS_CLIENT_SECRET` で読めます (PIN 入力なし)。
EOS
fi

exit 0
