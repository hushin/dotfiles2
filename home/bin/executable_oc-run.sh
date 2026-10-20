#!/usr/bin/env bash
set -euo pipefail

# oc-run.sh — opencode run をヘッドレスで実行し、最終回答とエラーをまとめて返す
#
# 使い方:
#   oc-run.sh [options] <slug> [repo-dir]
#
#   タスクは /tmp/opencode-tasks/<slug>.md に先に書いておく。
#   出力:
#     <slug>.jsonl   生イベント
#     <slug>.err     stderr
#     <slug>.out.md  最終回答（最後の step の text）
#     <slug>.sid     セッション ID
#
# オプション:
#   -a <agent>      opencode エージェント（既定: oc-worker。読み取り専用は oc-explore）
#   -t <seconds>    タイムアウト秒（既定: 540。0 で無制限 = pueue 用）
#   -m <model>      モデル上書き（既定: エージェント定義の model）
#   -s <old-slug>   <old-slug> のセッションを継続する
#
# 終了コード:
#   0  成功
#   2  引数エラー・slug 重複・二重起動
#   3  最終回答が空、または error イベントあり
#   その他  opencode の終了コード（124 = 時間切れ）

TASK_DIR="${OC_TASK_DIR:-/tmp/opencode-tasks}"
AGENT="oc-worker"
TIMEOUT=540
MODEL=""
CONTINUE_SLUG=""

_err() { printf 'oc-run: %s\n' "$*" >&2; }

while getopts "a:t:m:s:h" opt; do
  case "$opt" in
    a) AGENT="$OPTARG" ;;
    t) TIMEOUT="$OPTARG" ;;
    m) MODEL="$OPTARG" ;;
    s) CONTINUE_SLUG="$OPTARG" ;;
    h) sed -n '3,26s/^# \{0,1\}//p' "$0"; exit 0 ;;
    *) exit 2 ;;
  esac
done
shift $((OPTIND - 1))

SLUG="${1:-}"
REPO="${2:-$PWD}"
if [[ -z "$SLUG" ]]; then
  _err "slug を指定してください（-h でヘルプ）"
  exit 2
fi

BASE="$TASK_DIR/$SLUG"
if [[ ! -f "$BASE.md" ]]; then
  _err "タスクファイルがありません: $BASE.md"
  exit 2
fi
if [[ -e "$BASE.jsonl" ]]; then
  _err "$BASE.jsonl が既にあります。別の slug を使ってください"
  exit 2
fi

ARGS=(run --standalone --format json --agent "$AGENT" --title "cc:$SLUG" -f "$BASE.md")
[[ -n "$MODEL" ]] && ARGS+=(-m "$MODEL")
if [[ -n "$CONTINUE_SLUG" ]]; then
  SID=$(cat "$TASK_DIR/$CONTINUE_SLUG.sid" 2>/dev/null || true)
  if [[ -z "$SID" ]]; then
    _err "$TASK_DIR/$CONTINUE_SLUG.sid がありません"
    exit 2
  fi
  ARGS+=(--session "$SID")
fi
ARGS+=("Follow the attached task file exactly. Do not ask questions. Do not delegate to subagents.")

TIMEOUT_CMD=()
if [[ "$TIMEOUT" != 0 ]]; then
  if command -v timeout &>/dev/null; then
    TIMEOUT_CMD=(timeout "$TIMEOUT")
  elif command -v gtimeout &>/dev/null; then
    TIMEOUT_CMD=(gtimeout "$TIMEOUT")
  else
    _err "timeout コマンドがありません（macOS は coreutils を入れるか -t 0）"
    exit 2
  fi
fi

# 同じ slug の二重起動を防ぐ（flock は macOS に無いので mkdir でロック）
LOCK="$BASE.lock"
if ! mkdir "$LOCK" 2>/dev/null; then
  _err "$SLUG は実行中です（残骸なら $LOCK を消す）"
  exit 2
fi
trap 'rmdir "$LOCK"' EXIT

cd "$REPO"
set +e
"${TIMEOUT_CMD[@]}" opencode "${ARGS[@]}" < /dev/null > "$BASE.jsonl" 2> "$BASE.err"
CODE=$?
set -e

# ── 結果の取り出し ─────────────────────────────────
jq -rs '
  (([.[] | select(.type=="step_start")] | last | .timestamp) // 0) as $ts
  | [.[] | select(.type=="text" and .timestamp >= $ts) | .part.text]
  | join("\n")
' "$BASE.jsonl" > "$BASE.out.md" 2>/dev/null || : > "$BASE.out.md"

jq -r 'select(.sessionID != null and .sessionID != "") | .sessionID' "$BASE.jsonl" 2>/dev/null \
  | head -1 > "$BASE.sid" || true

ERRORS=$(jq -r 'select(.type=="error") | .error.message // (.error | tostring)' "$BASE.jsonl" 2>/dev/null || true)
TOOL_ERRORS=$(jq -r 'select(.type=="tool_use" and .part.state.status=="error")
  | [.part.tool, (.part.state.error | tostring | .[0:200])] | @tsv' "$BASE.jsonl" 2>/dev/null || true)
ANSWER_CHARS=$(wc -m < "$BASE.out.md" | tr -d ' ')

echo "exit: $CODE"
echo "answer: $BASE.out.md ($ANSWER_CHARS chars)"
echo "session: $(cat "$BASE.sid")"
echo "errors: ${ERRORS:-none}"
if [[ -n "$TOOL_ERRORS" ]]; then
  echo "tool errors: $(printf '%s\n' "$TOOL_ERRORS" | wc -l | tr -d ' ')"
  printf '%s\n' "$TOOL_ERRORS" | head -5 | sed 's/^/  /'
fi
if [[ -s "$BASE.err" ]]; then
  echo "stderr (tail):"
  tail -n 5 "$BASE.err" | sed 's/^/  /'
fi

if [[ "$CODE" -ne 0 ]]; then
  exit "$CODE"
fi
if [[ "$ANSWER_CHARS" -eq 0 || -n "$ERRORS" ]]; then
  exit 3
fi
