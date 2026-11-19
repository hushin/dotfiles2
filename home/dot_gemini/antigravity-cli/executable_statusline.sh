#!/usr/bin/env bash
# Antigravity CLI custom statusLine
# Based on official Antigravity statusline.sh with mode, cache hit rate, and quota gauges.
set -euo pipefail
shopt -s extglob

# ─── ANSI Helpers (Standard 16-color palette) ────────────────────────────────
R=$'\033[0m'
B=$'\033[1m'
D=$'\033[2m'
I=$'\033[3m'

FG_RED=$'\033[31m'
FG_GREEN=$'\033[32m'
FG_YELLOW=$'\033[33m'
FG_BLUE=$'\033[34m'
FG_MAGENTA=$'\033[35m'
FG_CYAN=$'\033[36m'
FG_WHITE=$'\033[37m'

FG_GRAY=$'\033[90m'
FG_BRIGHT_RED=$'\033[91m'
FG_BRIGHT_GREEN=$'\033[92m'
FG_BRIGHT_YELLOW=$'\033[93m'
FG_BRIGHT_BLUE=$'\033[94m'
FG_BRIGHT_MAGENTA=$'\033[95m'
FG_BRIGHT_CYAN=$'\033[96m'
FG_BRIGHT_WHITE=$'\033[97m'

NUM_COLOR="${FG_BRIGHT_WHITE}${B}"

# ─── Helpers ─────────────────────────────────────────────────────────────────
make_bar() {
  local pct=$1 len=$2 color=$3
  (( pct > 100 )) && pct=100
  (( pct < 0 )) && pct=0
  local filled=$(( pct * len / 100 ))
  local rem=$(( (pct * len) % 100 ))
  local bar="" i
  for (( i = 0; i < len; i++ )); do
    if (( i < filled )); then
      bar+="█"
    elif (( i == filled )); then
      if   (( rem >= 75 )); then bar+="▓"
      elif (( rem >= 50 )); then bar+="▒"
      elif (( rem >= 25 )); then bar+="░"
      else                       bar+="·"
      fi
    else
      bar+="·"
    fi
  done
  printf '%s%s%s' "$color" "$bar" "$R"
}

fmt_duration() {
  local sec=$1
  if (( sec <= 0 )); then
    return
  fi
  local d=$(( sec / 86400 ))
  local h=$(( (sec % 86400) / 3600 ))
  local m=$(( (sec % 3600) / 60 ))
  if   (( d > 0 )); then printf '%dd%dh' "$d" "$h"
  elif (( h > 0 )); then printf '%dh%02dm' "$h" "$m"
  else                   printf '%dm' "$m"
  fi
}

vis_len() {
  local plain="${1//$'\033'\[*([0-9;])m/}"
  printf '%d' "${#plain}"
}

# ─── Parse JSON from stdin (Single jq pass) ──────────────────────────────────
STATE="idle"
CYCLE_MODE=""
VIM_MODE=""
MODEL=""
EFFORT=""
CWD=""
VCS_BRANCH=""
VCS_DIRTY=""
SANDBOX="false"
USED_PCT="0"
CACHE_PCT="-1"
ARTIFACTS="0"
SUBAGENTS="0"
SUBAGENTS_RUNNING="0"
BG_TASKS="0"
QUOTA_RAW=""
COLS="80"

if command -v jq >/dev/null 2>&1; then
  JQ_OUT=$(
    jq -r '
      def clean: tostring | gsub("[\r\n]+"; " ");
      ((.model.display_name // .model.id // "") | clean) as $model |
      ($model | ascii_downcase) as $model_lc |
      (
        (.context_window.current_usage // null) as $cu |
        if $cu == null then -1
        else
          (($cu.input_tokens // 0) + ($cu.cache_read_input_tokens // 0) + ($cu.cache_creation_input_tokens // 0)) as $tot |
          if $tot > 0 then ((($cu.cache_read_input_tokens // 0) * 100 / $tot) | round)
          else -1 end
        end
      ) as $cache_pct |
      (
        ((.quota // {}) | to_entries | map(
          select((.value.disabled // false) != true and (.value.remaining_fraction != null))
        )) as $all_q |
        (
          if ($model_lc | contains("gemini")) then
            ($all_q | map(select(.key | ascii_downcase | startswith("gemini")))) as $sub |
            if ($sub | length) > 0 then $sub else $all_q end
          elif ($model_lc != "") then
            ($all_q | map(select(.key | ascii_downcase | startswith("3p")))) as $sub |
            if ($sub | length) > 0 then $sub else $all_q end
          else
            $all_q
          end
        ) |
        map(
          . + {
            _sec: (
              if (.value.reset_in_seconds != null) then (.value.reset_in_seconds | floor)
              elif (.value.reset_time != null) then
                (try (((.value.reset_time | sub("\\.[0-9]+Z$"; "Z") | fromdateiso8601) - now) | floor) catch 0)
              else 0 end
              | if . < 0 then 0 else . end
            )
          }
        ) |
        sort_by(if ._sec > 0 then ._sec else 999999999 end, .key) |
        map(
          (
            .key
            | if (. == "gemini-5h" or . == "3p-5h") then "5h"
              elif (. == "gemini-weekly" or . == "3p-weekly") then "7d"
              else (sub("^(gemini|3p)-"; "") | sub("-weekly$"; "-7d"))
              end
            | clean
          ) as $lbl |
          (((1 - .value.remaining_fraction) * 100) | round | if . < 0 then 0 elif . > 100 then 100 else . end) as $used |
          "\($lbl):\($used):\(._sec)"
        ) | join(",")
      ) as $quota_raw |
      ((.agent_state // "idle") | clean),
      ((.cycle_mode // "") | clean),
      ((.vim.mode // "") | clean),
      $model,
      ((.model.effort // "") | clean),
      ((.workspace.current_dir // .cwd // "") | clean),
      ((.vcs.branch // "") | clean),
      (if .vcs.dirty == true then "true" elif .vcs.dirty == false then "false" else "" end),
      (if .sandbox.enabled == true then "true" else "false" end),
      ((.context_window.used_percentage // 0) | clean),
      ($cache_pct | tostring),
      ((.artifact_count // 0) | tostring),
      ((if (.subagents | type) == "array" then (.subagents | length) else 0 end) | tostring),
      ((if (.subagents | type) == "array" then ([.subagents[] | select(.status == "running")] | length) else 0 end) | tostring),
      ((.task_count // 0) | tostring),
      $quota_raw,
      ((.terminal_width // 80) | floor | tostring)
    ' 2>/dev/null || true
  )

  if [[ -n "$JQ_OUT" ]]; then
    {
      read -r STATE
      read -r CYCLE_MODE
      read -r VIM_MODE
      read -r MODEL
      read -r EFFORT
      read -r CWD
      read -r VCS_BRANCH
      read -r VCS_DIRTY
      read -r SANDBOX
      read -r USED_PCT
      read -r CACHE_PCT
      read -r ARTIFACTS
      read -r SUBAGENTS
      read -r SUBAGENTS_RUNNING
      read -r BG_TASKS
      read -r QUOTA_RAW
      read -r COLS
    } <<< "$JQ_OUT" || true
  fi
fi

# ─── Computed Values ─────────────────────────────────────────────────────────
PCT_FMT=$(LC_NUMERIC=C printf "%.1f" "${USED_PCT:-0}" 2>/dev/null || printf "0.0")
PCT_INT=${USED_PCT%.*}
PCT_INT=${PCT_INT:-0}
[[ "$COLS" =~ ^[0-9]+$ ]] || COLS=80

# ─── State Indicator ─────────────────────────────────────────────────────────
case "$STATE" in
  idle)           S="${FG_BRIGHT_GREEN}${B}● READY${R}" ;;
  thinking)       S="${FG_BRIGHT_YELLOW}${B}◆ THINKING${R}" ;;
  working)        S="${FG_BRIGHT_CYAN}${B}⚙ WORKING${R}" ;;
  tool_use)       S="${FG_BRIGHT_MAGENTA}${B}🔧 TOOL${R}" ;;
  reviewing)      S="${FG_BRIGHT_YELLOW}${B}👀 REVIEW${R}" ;;
  authenticating) S="${FG_BRIGHT_YELLOW}${B}🔑 AUTH${R}" ;;
  initializing)   S="${FG_BRIGHT_WHITE}${B}⏳ INIT${R}" ;;
  error)          S="${FG_BRIGHT_RED}${B}✖ ERROR${R}" ;;
  *)              S="${FG_WHITE}${B}⏳ $(printf '%s' "$STATE" | tr '[:lower:]' '[:upper:]')${R}" ;;
esac

# ─── Mode Badges (cycle_mode & vim) ──────────────────────────────────────────
MODE=""
if [[ -n "$CYCLE_MODE" ]]; then
  case "$CYCLE_MODE" in
    auto)         MODE=" ${FG_BRIGHT_CYAN}[auto]${R}" ;;
    plan)         MODE=" ${FG_BRIGHT_BLUE}[plan]${R}" ;;
    accept-edits) MODE=" ${FG_BRIGHT_MAGENTA}[accept-edits]${R}" ;;
    *)            MODE=" ${FG_YELLOW}[${CYCLE_MODE}]${R}" ;;
  esac
fi
if [[ -n "$VIM_MODE" ]]; then
  MODE+=" ${FG_BRIGHT_YELLOW}[${VIM_MODE}]${R}"
fi

# ─── Model (+ effort if not already in display name) ─────────────────────────
M=""
if [[ -n "$MODEL" ]]; then
  MODEL_DISP="$MODEL"
  if [[ -n "$EFFORT" ]]; then
    MODEL_LC=$(printf '%s' "$MODEL" | tr '[:upper:]' '[:lower:]')
    EFFORT_LC=$(printf '%s' "$EFFORT" | tr '[:upper:]' '[:lower:]')
    if [[ "$MODEL_LC" != *"($EFFORT_LC)"* ]]; then
      MODEL_DISP="${MODEL} (${EFFORT})"
    fi
  fi
  M="${FG_GRAY} ╱ ${FG_BRIGHT_MAGENTA}${I}${MODEL_DISP}${R}"
fi

# ─── VCS Branch (with git fallback if vcs.branch is omitted) ─────────────────
if [[ -z "$VCS_BRANCH" && -n "$CWD" && -d "$CWD" ]]; then
  if VCS_BRANCH=$(git -C "$CWD" symbolic-ref --short -q HEAD 2>/dev/null); then
    :
  elif VCS_BRANCH=$(git -C "$CWD" rev-parse --short HEAD 2>/dev/null); then
    :
  else
    VCS_BRANCH=""
  fi
  if [[ -n "$VCS_BRANCH" && -z "$VCS_DIRTY" ]]; then
    if git -C "$CWD" diff-index --quiet HEAD -- 2>/dev/null; then
      VCS_DIRTY="false"
    else
      VCS_DIRTY="true"
    fi
  fi
fi

V=""
if [[ -n "$VCS_BRANCH" ]]; then
  if [[ "$VCS_DIRTY" == "true" ]]; then
    V="${FG_GRAY} ╱ ${FG_BRIGHT_RED}${VCS_BRANCH}${FG_BRIGHT_YELLOW}*${R}"
  else
    V="${FG_GRAY} ╱ ${FG_BRIGHT_BLUE}${VCS_BRANCH}${R}"
  fi
fi

# ─── Context Bar ─────────────────────────────────────────────────────────────
if (( PCT_INT >= 90 )); then
  CTX_BAR_COLOR="$FG_BRIGHT_RED"
elif (( PCT_INT >= 60 )); then
  CTX_BAR_COLOR="$FG_BRIGHT_YELLOW"
else
  CTX_BAR_COLOR="$FG_BRIGHT_WHITE"
fi

CTX_BAR=$(make_bar "$PCT_INT" 10 "$CTX_BAR_COLOR")
CTX="${FG_GRAY}ctx ${CTX_BAR} ${NUM_COLOR}${PCT_FMT}%${R}"

CTX_BAR_NARROW=$(make_bar "$PCT_INT" 8 "$CTX_BAR_COLOR")
CTX_NARROW="${FG_GRAY}ctx ${CTX_BAR_NARROW} ${NUM_COLOR}${PCT_INT}%${R}"

# ─── Cache Hit Rate ──────────────────────────────────────────────────────────
if (( CACHE_PCT >= 0 )); then
  if (( CACHE_PCT >= 75 )); then
    CACHE_COLOR="$FG_BRIGHT_GREEN"
  elif (( CACHE_PCT >= 40 )); then
    CACHE_COLOR="$FG_BRIGHT_YELLOW"
  else
    CACHE_COLOR="$FG_BRIGHT_RED"
  fi
  CACHE_FMT="${FG_GRAY}cache ${CACHE_COLOR}${B}${CACHE_PCT}%${R}"
else
  CACHE_FMT="${FG_GRAY}cache ${D}--${R}"
fi

# ─── Quota Gauges ────────────────────────────────────────────────────────────
DOT="${FG_GRAY} · ${R}"
QUOTA_FULL=""
QUOTA_NARROW=""

if [[ -n "$QUOTA_RAW" ]]; then
  IFS=',' read -r -a QUOTA_ITEMS <<< "$QUOTA_RAW"
  for item in "${QUOTA_ITEMS[@]}"; do
    [[ -z "$item" ]] && continue
    IFS=':' read -r q_lbl q_used q_sec <<< "$item"
    [[ "$q_used" =~ ^[0-9]+$ ]] || continue
    [[ "$q_sec" =~ ^[0-9]+$ ]] || q_sec=0

    if (( q_used >= 90 )); then
      q_color="$FG_BRIGHT_RED"
    elif (( q_used >= 75 )); then
      q_color="$FG_BRIGHT_YELLOW"
    else
      q_color="$FG_BRIGHT_GREEN"
    fi

    q_bar=$(make_bar "$q_used" 6 "$q_color")
    q_reset=$(fmt_duration "$q_sec")

    q_part="${FG_GRAY}${q_lbl} ${q_bar} ${q_color}${B}${q_used}%${R}"
    q_part_narrow="${FG_GRAY}${q_lbl} ${q_color}${q_used}%${R}"
    if [[ -n "$q_reset" ]]; then
      q_part+=" ${FG_GRAY}↻${q_reset}${R}"
    fi

    QUOTA_FULL+="${DOT}${q_part}"
    QUOTA_NARROW+="${DOT}${q_part_narrow}"
  done
fi

# ─── Counts & Sandbox Badge ──────────────────────────────────────────────────
ART_FMT="${FG_GRAY}artifacts ${NUM_COLOR}${ARTIFACTS}${R}"
if (( SUBAGENTS_RUNNING > 0 )); then
  SUB_FMT="${FG_GRAY}subagents ${FG_BRIGHT_CYAN}${B}${SUBAGENTS}${R}"
else
  SUB_FMT="${FG_GRAY}subagents ${NUM_COLOR}${SUBAGENTS}${R}"
fi
BG_FMT="${FG_GRAY}tasks ${NUM_COLOR}${BG_TASKS}${R}"

if [[ "$SANDBOX" == "true" ]]; then
  SB="${FG_GRAY}sandbox ${FG_BRIGHT_GREEN}${B}ON${R}"
else
  SB="${FG_GRAY}sandbox off${R}"
fi

# ─── Assemble Segments ───────────────────────────────────────────────────────
HEAD="${S}${MODE}${M}${V}"
METRICS="${CTX}${DOT}${CACHE_FMT}${QUOTA_FULL}"
META="${ART_FMT}${DOT}${SUB_FMT}${DOT}${BG_FMT}${DOT}${SB}"
FULL_LINE2="${METRICS}${DOT}${META}"

# ─── Adaptive Layout by Terminal Width ───────────────────────────────────────
LEN_HEAD=$(vis_len "$HEAD")
LEN_METRICS=$(vis_len "$METRICS")
LEN_META=$(vis_len "$META")
LEN_FULL_LINE2=$(vis_len "$FULL_LINE2")

if (( COLS >= 80 && LEN_HEAD + 5 + LEN_FULL_LINE2 + 2 <= COLS )); then
  # Wide: single line
  printf '%s%s  │  %s%s\n' "$HEAD" "$FG_GRAY" "$R" "$FULL_LINE2"
elif (( COLS >= 80 && 3 + LEN_FULL_LINE2 + 2 <= COLS )); then
  # Medium-wide: classic 2-line box
  printf '%s╭─%s %s\n' "$FG_GRAY" "$R" "$HEAD"
  printf '%s╰─%s %s\n' "$FG_GRAY" "$R" "$FULL_LINE2"
elif (( COLS >= 80 && 3 + LEN_HEAD + 3 + LEN_META + 2 <= COLS && 3 + LEN_METRICS + 2 <= COLS )); then
  # Medium (balanced 2-line box): meta on line 1, ctx/cache/quota on line 2
  printf '%s╭─%s %s%s%s\n' "$FG_GRAY" "$R" "$HEAD" "$DOT" "$META"
  printf '%s╰─%s %s\n' "$FG_GRAY" "$R" "$METRICS"
elif (( COLS >= 80 )); then
  # Medium (3-line box fallback when metrics + meta are wide)
  printf '%s╭─%s %s\n' "$FG_GRAY" "$R" "$HEAD"
  printf '%s│ %s %s\n' "$FG_GRAY" "$R" "$METRICS"
  printf '%s╰─%s %s\n' "$FG_GRAY" "$R" "$META"
else
  # Narrow (< 80 cols): compact 2-line, minimal chrome
  printf '%s%s%s\n' "$S" "$MODE" "$M"
  printf '%s%s%s%s%s\n' "$CTX_NARROW" "$DOT" "$CACHE_FMT" "$QUOTA_NARROW" "$DOT$BG_FMT"
fi
