# ai-commit.ps1 — pi でコミットメッセージを生成してコミット（Windows / PowerShell 版）
#
# ai-commit.sh の PowerShell 実装。`ai-commit -h` で使い方を表示する。
# 前提:
#   - pi (`pi`) がインストール・認証済み
#   - git リポジトリ内で実行
#
# 注意: 大きな diff でも引数長の上限に当たらないよう、プロンプトは標準入力から渡す。

$ErrorActionPreference = 'Stop'

# git / pi の UTF-8 出力を文字化けさせない
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8
$OutputEncoding = New-Object System.Text.UTF8Encoding $false

$usageText = @'
ai-commit — pi でコミットメッセージを生成してコミット

使い方:
  ai-commit                # 1行 Conventional Commits でコミット
  ai-commit -c             # Contextual Commits（body に action lines）
  ai-commit --dry          # メッセージ生成のみ（コミットしない）
  ai-commit --all          # git add -A してからコミット
  ai-commit --no-edit      # エディタを開かず即コミット

前提:
  - pi (`pi`) がインストール・認証済み
  - git リポジトリ内で実行

カスタマイズ:
  GIT_AI_MODEL / GIT_AI_MAX_DIFF / GIT_AI_NO_EDIT を環境変数で上書き可能
  GIT_AI_MODEL 未指定なら pi の defaultModel（settings.json）を使う
'@

# ── 色付きヘルパー ──────────────────────────────────
function Write-Info([string]$Message) {
    Write-Host '▸' -NoNewline -ForegroundColor Blue
    Write-Host " $Message"
}
function Write-Ok([string]$Message) {
    Write-Host '✔' -NoNewline -ForegroundColor Green
    Write-Host " $Message"
}
function Write-Warn([string]$Message) {
    Write-Host '⚠' -NoNewline -ForegroundColor Yellow
    Write-Host " $Message"
}
function Write-Err([string]$Message) {
    Write-Host '✘' -NoNewline -ForegroundColor Red
    Write-Host " $Message"
}

# ── 設定（環境変数で上書き可能） ────────────────────
$Model = $env:GIT_AI_MODEL
$noEdit = [bool]$env:GIT_AI_NO_EDIT

$maxDiffLines = 800
if ($env:GIT_AI_MAX_DIFF) {
    $parsed = 0
    if ([int]::TryParse($env:GIT_AI_MAX_DIFF, [ref]$parsed) -and $parsed -gt 0) {
        $maxDiffLines = $parsed
    }
    else {
        Write-Warn "GIT_AI_MAX_DIFF の値が不正です: $env:GIT_AI_MAX_DIFF（既定の 800 を使用）"
    }
}

# ── 引数処理 ───────────────────────────────────────
$dryRun = $false
$addAll = $false
$contextual = $false

foreach ($arg in $args) {
    switch -Regex ($arg) {
        '^(-c|--contextual)$' { $contextual = $true }
        '^--dry$' { $dryRun = $true }
        '^--all$' { $addAll = $true }
        '^--no-edit$' { $noEdit = $true }
        '^(-h|--help)$' { Write-Host $usageText; exit 0 }
        default {
            Write-Err "Unknown option: $arg"
            exit 1
        }
    }
}

# ── 事前チェック ───────────────────────────────────
if (-not (Get-Command -Name 'pi' -ErrorAction SilentlyContinue)) {
    Write-Err 'pi コマンドが見つかりません。pi をインストールしてください。'
    Write-Err '  npm install -g @earendil-works/pi-coding-agent'
    exit 1
}

$null = git rev-parse --is-inside-work-tree 2>$null
if ($LASTEXITCODE -ne 0) {
    Write-Err 'git リポジトリ内で実行してください。'
    exit 1
}

# ── ステージング ───────────────────────────────────
if ($addAll) {
    git add -A
    if ($LASTEXITCODE -ne 0) {
        Write-Err 'git add -A に失敗しました。'
        exit 1
    }
    Write-Info 'git add -A 実行済み'
}

$diffLines = @(git --no-pager diff --cached --no-color)
if ($LASTEXITCODE -ne 0) {
    Write-Err 'git diff --cached の取得に失敗しました。'
    exit 1
}
$diff = $diffLines -join "`n"

if ([string]::IsNullOrWhiteSpace($diff)) {
    Write-Err 'ステージングされた変更がありません。git add してから再実行してください。'
    exit 1
}

$stat = (@(git --no-pager diff --cached --stat --no-color) -join "`n")
Write-Info "staged diff: $($diffLines.Count) 行"

# diff が長すぎる場合は truncate
if ($diffLines.Count -gt $maxDiffLines) {
    Write-Warn "diff が $maxDiffLines 行を超えたため切り詰めます（精度が落ちる場合あり）"
    $diff = (($diffLines | Select-Object -First $maxDiffLines) -join "`n") +
        "`n... (truncated — full diff was $($diffLines.Count) lines)"
}

# ── プロンプト構築 ─────────────────────────────────

# デフォルト: シンプルな1行 Conventional Commits
$promptSimple = @'
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
'@

# -c: Contextual Commits（body に action lines）
$promptContextual = @'
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
'@

if ($contextual) {
    $prompt = $promptContextual
    $modeLabel = 'Contextual Commits'
}
else {
    $prompt = $promptSimple
    $modeLabel = 'Conventional Commits (1行)'
}

# ── pi 呼び出し ────────────────────────────────────
$modelLabel = if ($Model) { $Model } else { '既定' }
Write-Info "$modeLabel — pi (model: $modelLabel) で生成中..."

$fullPrompt = @"
$prompt

--- git diff --cached --stat ---
$stat

--- git diff --cached ---
$diff
"@

# セッションやコンテキストファイルを読み込まず、ツールなしで一発生成させる
$piArgs = @(
    '--print'
    '--no-session'
    '--no-tools'
    '--no-context-files'
    '--no-skills'
    '--no-prompt-templates'
    '--offline'
)
if ($Model) {
    $piArgs += @('--model', $Model)
}

$rawMsg = @($fullPrompt | & pi @piArgs 2>$null)
$piExitCode = $LASTEXITCODE
if ($piExitCode -ne 0) {
    Write-Err 'pi の呼び出しに失敗しました。認証状態とモデル名を確認してください。'
    Write-Err '  pi --list-models'
    $modelHint = if ($Model) { "--model $Model " } else { '' }
    Write-Err "  pi ${modelHint}-p 'hello'"
    exit 1
}

# 前後の空行・バッククォートフェンスを除去
$cleanLines = New-Object System.Collections.Generic.List[string]
foreach ($rawLine in $rawMsg) {
    foreach ($part in ($rawLine -split "`r?`n")) {
        if ($part -match '^\s*```') { continue }
        $cleanLines.Add($part.Trim())
    }
}
while ($cleanLines.Count -gt 0 -and $cleanLines[0] -eq '') { $cleanLines.RemoveAt(0) }
while ($cleanLines.Count -gt 0 -and $cleanLines[$cleanLines.Count - 1] -eq '') {
    $cleanLines.RemoveAt($cleanLines.Count - 1)
}
$commitMsg = $cleanLines -join "`n"

if ([string]::IsNullOrWhiteSpace($commitMsg)) {
    Write-Err '空のメッセージが返されました。diff の内容を確認してください。'
    exit 1
}

# ── 結果表示 ───────────────────────────────────────
Write-Host ''
Write-Host '─────────────────────────────────────────'
Write-Host $commitMsg
Write-Host '─────────────────────────────────────────'
Write-Host ''

# ── Dry run ならここで終了 ─────────────────────────
if ($dryRun) {
    Write-Ok 'dry run 完了（コミットはしていません）'
    exit 0
}

# ── コミット実行 ───────────────────────────────────
# メッセージは一時ファイル経由で渡す（引用符・改行・文字コードの事故を避ける）
$tempFile = [System.IO.Path]::GetTempFileName()
try {
    [System.IO.File]::WriteAllText(
        $tempFile,
        $commitMsg + "`n",
        (New-Object System.Text.UTF8Encoding $false)
    )

    $commitArgs = @('commit', '-F', $tempFile)
    if (-not $noEdit) {
        $commitArgs += '-e'
    }

    & git @commitArgs
    if ($LASTEXITCODE -ne 0) {
        Write-Err "コミットに失敗しました（exit $LASTEXITCODE）。"
        exit 1
    }
}
finally {
    Remove-Item -LiteralPath $tempFile -ErrorAction SilentlyContinue
}

Write-Ok 'コミット完了'
