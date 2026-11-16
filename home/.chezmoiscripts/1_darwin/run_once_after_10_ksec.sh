#!/bin/sh
# ksec: macOS ではじめて chezmoi apply したときの前提チェック。
#
# ksec 自体は ~/bin/ksec (chezmoi: home/bin/executable_ksec) に配置される。
# ここではバックエンドが使えるか確認し、まだ何も入っていない場合に
# 1Password からの移行コマンドを案内するだけ。認証ダイアログは出さない。
set -eu

[ "$(uname -s)" = Darwin ] || exit 0

ksec="$HOME/bin/ksec"
if [ ! -x "$ksec" ]; then
    echo "ksec: $ksec が無いためセットアップをスキップします" >&2
    exit 0
fi

if ! "$ksec" doctor; then
    echo "ksec: バックエンドが使えません (login Keychain が解錠されているか確認)" >&2
    exit 0
fi

if [ -z "$("$ksec" list 2>/dev/null || true)" ]; then
    cat >&2 <<'EOS'

ksec: まだ何も登録されていません。1Password から 1 回だけ取り出して保存します:

  ksec from-op GWS_CLIENT_ID     'op://Personal/google-workspace-mcp/username'
  ksec from-op GWS_CLIENT_SECRET 'op://Personal/google-workspace-mcp/password'
  ksec from-op JINA_API_KEY      'op://Personal/JINA_API_KEY/password'

以後は `ksec get GWS_CLIENT_SECRET` で読めます (プロンプトなし)。
EOS
fi

exit 0
