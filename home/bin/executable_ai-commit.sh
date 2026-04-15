#!/usr/bin/env bash
set -euo pipefail

# ai-commit.sh — Copilot CLI でコミットメッセージを生成してコミット
#
# 使い方:
#   ai-commit.sh             # 1行 Conventional Commits でコミット
#   ai-commit.sh -c          # Contextual Commits（body に action lines）
#   ai-commit.sh --dry       # メッセージ生成のみ（コミットしない）
#   ai-commit.sh --all       # git add -A してからコミット
#   ai-commit.sh --no-edit   # エディタを開かず即コミット
#
# 前提:
#   - GitHub Copilot CLI (`copilot`) がインストール・認証済み
#   - git リポジトリ内で実行
#
# カスタマイズ:
#   GIT_AI_MODEL / GIT_AI_MAX_DIFF / GIT_AI_NO_EDIT を環境変数で上書き可能

MODEL="${GIT_AI_MODEL:-gpt-4.1}"
MAX_DIFF_LINES="${GIT_AI_MAX_DIFF:-800}"
EDITOR_FLAG="${GIT_AI_NO_EDIT:-}"  # 空なら -e (エディタ) 付き、"1" なら省略

# ── 色付きヘルパー ──────────────────────────────────
_info()  { printf '\033[1;34m▸\033[0m %s\n' "$*"; }
_ok()    { printf '\033[1;32m✔\033[0m %s\n' "$*"; }
_warn()  { printf '\033[1;33m⚠\033[0m %s\n' "$*"; }
_err()   { printf '\033[1;31m✘\033[0m %s\n' "$*" >&2; }

# ── 引数処理 ───────────────────────────────────────
DRY_RUN=0
ADD_ALL=0
CONTEXTUAL=0
for arg in "$@"; do
  case "$arg" in
    -c|--contextual) CONTEXTUAL=1 ;;
    --dry)     DRY_RUN=1 ;;
    --all)     ADD_ALL=1 ;;
    --no-edit) EDITOR_FLAG=1 ;;
    -h|--help)
      sed -n '3,18s/^# //p' "$0"
      exit 0
      ;;
    *)
      _err "Unknown option: $arg"
      exit 1
      ;;
  esac
done

# ── 事前チェック ───────────────────────────────────
if ! command -v copilot &>/dev/null; then
  _err "copilot コマンドが見つかりません。GitHub Copilot CLI をインストールしてください。"
  _err "  https://docs.github.com/en/copilot/how-tos/set-up/install-copilot-cli"
  exit 1
fi

if ! git rev-parse --is-inside-work-tree &>/dev/null; then
  _err "git リポジトリ内で実行してください。"
  exit 1
fi

# ── ステージング ───────────────────────────────────
if [[ "$ADD_ALL" -eq 1 ]]; then
  git add -A
  _info "git add -A 実行済み"
fi

DIFF=$(git diff --cached)
if [[ -z "$DIFF" ]]; then
  _err "ステージングされた変更がありません。git add してから再実行してください。"
  exit 1
fi

STAT=$(git diff --cached --stat)
DIFF_LINES=$(echo "$DIFF" | wc -l)
_info "staged diff: ${DIFF_LINES} 行"

# diff が長すぎる場合は truncate
if [[ "$DIFF_LINES" -gt "$MAX_DIFF_LINES" ]]; then
  _warn "diff が ${MAX_DIFF_LINES} 行を超えたため切り詰めます（精度が落ちる場合あり）"
  DIFF=$(echo "$DIFF" | head -n "$MAX_DIFF_LINES")
  DIFF="${DIFF}
... (truncated — full diff was ${DIFF_LINES} lines)"
fi

# ── プロンプト構築 ─────────────────────────────────

# デフォルト: シンプルな1行 Conventional Commits
read -r -d '' PROMPT_SIMPLE <<'PROMPT_END' || true
あなたは Git コミットメッセージの専門家です。
`git diff --cached` の内容から、1行のコミットメッセージを生成してください。

## ルール
- Conventional Commits 形式（英語、命令形、小文字始まり、ピリオドなし）
- type: feat, fix, refactor, docs, style, test, chore, perf, ci, build
- scope は省略可。ファイルやモジュール名が明確なら付ける
- 72文字以内
- 説明部分（type(scope): の後）は日本語で書く
- 出力は1行のコミットメッセージのみ。他の説明やフェンスは一切付けない

## 例
- feat(auth): Google OAuth プロバイダーを実装
- fix(payments): 通貨の丸め処理のエッジケースを修正
- refactor(notifications): ダイジェスト配信スケジュールのロジックを分離
- chore: ESLint の設定を更新
PROMPT_END

# -c: Contextual Commits（body に action lines）
read -r -d '' PROMPT_CONTEXTUAL <<'PROMPT_END' || true
あなたは Git コミットメッセージの専門家です。
`git diff --cached` の内容から Contextual Commits 形式のコミットメッセージを生成してください。

## フォーマット

```
type(scope): 英語の要約（命令形、小文字始まり、ピリオドなし）

action-type(scope): 日本語の説明
action-type(scope): 日本語の説明
```

### Subject Line（1行目）
- Conventional Commits 形式（英語、命令形）
- type: feat, fix, refactor, docs, style, test, chore, perf, ci, build
- scope は省略可。ファイルやモジュール名が明確なら付ける
- 72文字以内

### Body（action lines）— 日本語で書く
以下の action-type から該当するものだけ使う（1〜3行が目安。不要なら body なし）:

- intent(scope): 変更の意図・目的（なぜこの変更が必要か）
- decision(scope): 採用した方針と理由（代替案があった場合）
- rejected(scope): 検討して却下した案と却下理由
- constraint(scope): 実装に影響した制約条件
- learned(scope): 実装中に発見した知見

### ルール
- diff の内容だけから書く。推測で intent/rejected/constraint/learned を捏造しない
- 自明な修正（typo、フォーマット）なら subject line のみで OK
- 出力はコミットメッセージ本文のみ。説明や markdown フェンスは付けない
PROMPT_END

if [[ "$CONTEXTUAL" -eq 1 ]]; then
  PROMPT="$PROMPT_CONTEXTUAL"
  MODE_LABEL="Contextual Commits"
else
  PROMPT="$PROMPT_SIMPLE"
  MODE_LABEL="Conventional Commits (1行)"
fi

# ── Copilot CLI 呼び出し ──────────────────────────
_info "${MODE_LABEL} — Copilot CLI (model: ${MODEL}) で生成中..."

FULL_PROMPT="${PROMPT}

--- git diff --cached --stat ---
${STAT}

--- git diff --cached ---
${DIFF}"

COMMIT_MSG=$(copilot -p "$FULL_PROMPT" --model "$MODEL" 2>/dev/null) || {
  _err "Copilot CLI の呼び出しに失敗しました。認証状態とモデル名を確認してください。"
  _err "  copilot auth status"
  _err "  copilot --model ${MODEL}"
  exit 1
}

# 前後の空行・バッククォートフェンスを除去
COMMIT_MSG=$(echo "$COMMIT_MSG" | sed '/^```/d' | sed -e 's/^[[:space:]]*//' -e '/./,$!d' | sed -e :a -e '/^[[:space:]]*$/{ $d; N; ba; }')

if [[ -z "$COMMIT_MSG" ]]; then
  _err "空のメッセージが返されました。diff の内容を確認してください。"
  exit 1
fi

# ── 結果表示 ───────────────────────────────────────
echo ""
echo "─────────────────────────────────────────"
echo "$COMMIT_MSG"
echo "─────────────────────────────────────────"
echo ""

# ── Dry run ならここで終了 ─────────────────────────
if [[ "$DRY_RUN" -eq 1 ]]; then
  _ok "dry run 完了（コミットはしていません）"
  exit 0
fi

# ── コミット実行 ───────────────────────────────────
EDIT_OPT="-e"
if [[ -n "$EDITOR_FLAG" ]]; then
  EDIT_OPT=""
fi

# shellcheck disable=SC2086
git commit -m "$COMMIT_MSG" $EDIT_OPT
_ok "コミット完了"
