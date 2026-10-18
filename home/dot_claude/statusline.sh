#!/usr/bin/env bash
# Claude Code statusLine
# 参考: https://zenn.dev/zozotech/articles/cb767af383bc78
# 1行目: 📁 dir  ⎇ branch  +added/-removed
# 2行目: 🧠 model  ⚡ effort  ctx gauge  5h gauge ↻  7d gauge ↻  🗜 compact回数

input=$(cat)

IFS=$'\x1f' read -r model effort cwd ctx_pct added removed \
  h5_pct h5_reset d7_pct d7_reset session_id < <(
  jq -r '[
    (.model.display_name // "?"),
    (.effort.level // ""),
    (.workspace.current_dir // .cwd // "."),
    ((.context_window.used_percentage // 0) | floor),
    (.cost.total_lines_added // 0),
    (.cost.total_lines_removed // 0),
    ((.rate_limits.five_hour.used_percentage // -1) | floor),
    ((.rate_limits.five_hour.resets_at // 0) | floor),
    ((.rate_limits.seven_day.used_percentage // -1) | floor),
    ((.rate_limits.seven_day.resets_at // 0) | floor),
    (.session_id // "nosession")
  ] | map(tostring) | join("\u001f")' <<<"$input"
)

# --- colors ---
RST=$'\e[0m'; DIM=$'\e[2m'; BOLD=$'\e[1m'
GREEN=$'\e[32m'; YELLOW=$'\e[33m'; ORANGE=$'\e[38;5;208m'; RED=$'\e[31m'
CYAN=$'\e[36m'; MAGENTA=$'\e[35m'; BLUE=$'\e[34m'; GRAY=$'\e[90m'

# 使用率 → 色
zone_color() {
  local p=$1
  if   (( p >= 90 )); then printf '%s' "$RED"
  elif (( p >= 75 )); then printf '%s' "$ORANGE"
  elif (( p >= 50 )); then printf '%s' "$YELLOW"
  else                     printf '%s' "$GREEN"
  fi
}

# 使用率 → 絵文字
zone_icon() {
  local p=$1
  if   (( p >= 90 )); then printf '🔴'
  elif (( p >= 75 )); then printf '🟠'
  elif (( p >= 50 )); then printf '🟡'
  else                     printf '🟢'
  fi
}

# gauge <percent> <width>: 半セル(▌)対応のゲージ
gauge() {
  local p=$1 w=$2 halves full half i out=""
  (( p > 100 )) && p=100
  (( p < 0 )) && p=0
  halves=$(( (p * w * 2 + 50) / 100 ))
  full=$(( halves / 2 )); half=$(( halves % 2 ))
  for (( i = 0; i < full; i++ )); do out+="█"; done
  (( half )) && out+="▌"
  local empty=$(( w - full - half ))
  local rest=""
  for (( i = 0; i < empty; i++ )); do rest+="░"; done
  printf '%s%s%s%s%s' "$(zone_color "$p")" "$out" "$GRAY" "$rest" "$RST"
}

# fmt_reset <unix秒>: 残り時間を 2d3h / 1h30m / 45m 形式で
fmt_reset() {
  local ts=$1 now diff d h m
  (( ts <= 0 )) && return
  now=$(date +%s)
  diff=$(( ts - now ))
  (( diff < 0 )) && diff=0
  d=$(( diff / 86400 )); h=$(( diff % 86400 / 3600 )); m=$(( diff % 3600 / 60 ))
  if   (( d > 0 )); then printf '%dd%dh' "$d" "$h"
  elif (( h > 0 )); then printf '%dh%02dm' "$h" "$m"
  else                   printf '%dm' "$m"
  fi
}

# --- line 1 ---
dir=${cwd/#$HOME/\~}
line1="${BLUE}📁 ${dir}${RST}"

if branch=$(git -C "$cwd" symbolic-ref --short -q HEAD 2>/dev/null); then
  line1+="  ${MAGENTA}⎇ ${branch}${RST}"
elif sha=$(git -C "$cwd" rev-parse --short HEAD 2>/dev/null); then
  line1+="  ${MAGENTA}⎇ ${sha}${RST}"
fi

if (( added > 0 || removed > 0 )); then
  line1+="  ${GREEN}+${added}${RST}${GRAY}/${RST}${RED}-${removed}${RST}"
fi

# --- compact 検知 ---
state_dir="${XDG_RUNTIME_DIR:-/tmp}/claude-statusline"
mkdir -p "$state_dir" 2>/dev/null
state_file="$state_dir/${session_id}"
prev_pct=0; compacts=0
[[ -f $state_file ]] && read -r prev_pct compacts <"$state_file"
# 使用率が 20pt 以上下がったら compact とみなす
if (( prev_pct - ctx_pct >= 20 )); then
  compacts=$(( compacts + 1 ))
fi
printf '%s %s\n' "$ctx_pct" "$compacts" >"$state_file" 2>/dev/null

# --- line 2 ---
line2="${CYAN}${BOLD}🧠 ${model}${RST}"
[[ -n $effort ]] && line2+="  ${YELLOW}⚡ ${effort}${RST}"
line2+="  $(zone_icon "$ctx_pct") $(gauge "$ctx_pct" 10) $(zone_color "$ctx_pct")${ctx_pct}%${RST}"

rate_part() {
  local label=$1 p=$2 ts=$3 r
  (( p < 0 )) && return
  r=$(fmt_reset "$ts")
  printf '  %s%s%s %s %s%d%%%s' "$DIM" "$label" "$RST" "$(gauge "$p" 6)" "$(zone_color "$p")" "$p" "$RST"
  [[ -n $r ]] && printf ' %s↻%s%s' "$GRAY" "$r" "$RST"
}
line2+="$(rate_part 5h "$h5_pct" "$h5_reset")"
line2+="$(rate_part 7d "$d7_pct" "$d7_reset")"

(( compacts > 0 )) && line2+="  ${GRAY}🗜 ${compacts}${RST}"

printf '%s\n%s\n' "$line1" "$line2"
