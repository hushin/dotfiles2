#!/bin/sh
# browse: WSL から Windows の relay を使うためのトークン同期。
#
# Windows 側の browse が relay と Chrome 拡張のペアリングを所有している。
# WSL の CLI / MCP は同じトークンで Windows の relay に認証する必要があるため、
# Windows 側の relay-token を ~/.local/share/browse/relay-token に写す。
# WSL の mirrored ネットワークでは 127.0.0.1:19988 が Windows と共有される。
set -eu

# Windows のユーザーディレクトリ (WSL のユーザー名と異なる場合がある)
winhome=$(cmd.exe /c 'echo %USERPROFILE%' 2>/dev/null | tr -d '\r' || true)
if [ -z "$winhome" ] || ! command -v wslpath >/dev/null 2>&1; then
    exit 0
fi

winroot=$(wslpath -u "$winhome" 2>/dev/null || true)
[ -n "$winroot" ] || exit 0

src="$winroot/.local/share/browse/relay-token"
dst="$HOME/.local/share/browse/relay-token"

# Windows 側に browse がまだ無いときは何もしない
[ -r "$src" ] || exit 0

tmp=$(mktemp)
trap 'rm -f "$tmp"' EXIT
tr -d '\r\n' < "$src" > "$tmp"
[ -s "$tmp" ] || exit 0

# 中身が同じなら触らない
if [ -f "$dst" ] && cmp -s "$tmp" "$dst"; then
    exit 0
fi

mkdir -p "$(dirname "$dst")"
install -m 600 "$tmp" "$dst"
echo "browse: relay トークンを Windows から同期しました" >&2
