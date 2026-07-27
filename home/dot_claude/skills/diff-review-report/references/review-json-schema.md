# review.json の書き方

`collect_diff.py` が出した `hunks.json` に対して、**あなたが書くのはこのファイルだけ**。
diff の本文を書き写す必要はない。hunk id を並べれば、その中身はレポート側が埋める。

## 全体構造

```json
{
  "title": "app/system URL整理の未ステージ差分レビュー",
  "subtitle": "任意。ヘッダー下に出る一行",
  "base": "HEAD (未ステージ + ステージ済み)",
  "plan": "ai-out/plans/url-cleanup.md",
  "overview": "任意。この変更全体を 2〜4 行で。読み手が最初に頭に入れるべき前提を書く。",
  "groups": [ /* 後述 */ ]
}
```

`plan` は plan 照合フェーズを回したときだけ入れる。入れるとフィードバック markdown にも
plan のパスが載るので、受け取った側が同じ文書を参照できる。

## group

```json
{
  "title": "URL・ホスト判定の共通基盤",
  "kind": "refactor",
  "risk": "high",
  "intent": "deployment mode ごとの URL 生成と host 判定を一か所に集約し、legacy-path と subdomain を安全に切り替える。",
  "summary": "site root の検証、app URL 生成、user URL、subdomain label、request host 判定を共通 helper へ整理している。legacy-path でも通る基盤コードなので、影響範囲は広め。",
  "needs_improvement": false,
  "hunks": ["h079", "h080", "h081"],
  "hunk_notes": {
    "h080": "この hunk だけ挙動が変わる。既定値が false になった点に注意。"
  },
  "findings": [ /* 後述 */ ]
}
```

| フィールド | 必須 | 内容 |
|---|---|---|
| `title` | ○ | 「何をしたか」が一目で分かる短い日本語 |
| `intent` | ○ | **なぜ**この変更が必要か。diff を見れば分かることではなく、diff からは読み取れないこと |
| `summary` | | 何をどう変えたかの補足。一覧のサブテキストにも使われる |
| `risk` | ○ | `high` / `medium` / `low`。壊れたときの影響 × 壊れやすさ |
| `kind` | | `feat` / `fix` / `refactor` / `test` / `docs` / `chore` など |
| `needs_improvement` | | 意図が読み取れなかった、または設計として要改善と判断したとき `true` |
| `hunks` | ○ | この意図に属する hunk id。**全 hunk がちょうど 1 グループに属すること** |
| `hunk_notes` | | 個別 hunk に付ける一言解説。全部に付ける必要はない。目が留まるべき所だけ |
| `findings` | | 指摘。無ければ空配列 |

`intent` が書けない hunk 群があるなら、それ自体が発見。`needs_improvement: true` にして
「意図が読み取れない」と findings に書く。無理に物語を作らない。

## finding

```json
{
  "severity": "warning",
  "stage": "blind",
  "verdict": "demoted",
  "title": "省略可能な別フラグが context-only 処理の抜けを隠します",
  "location": "apps/web/src/app/api/[[...route]]/routes/ai-route-message-utils.ts (新L109〜) [h010]",
  "detail": "reqTranslateData.inlineContextOnly と別の isContextOnly を省略可能引数で持つため、渡し忘れ時は表示用 sourceText が通常の source として入ります。",
  "suggestion": "isContextOnly を必須引数にし、route の受け渡しテストも追加すると意図を型で守れます。",
  "verdict_note": "plan は検証済み boolean を1回作って後続へ渡す設計を意図し、現行唯一の production caller も正しく渡しているため demoted としますが、既定値 false はその保証を弱めています。"
}
```

| フィールド | 必須 | 内容 |
|---|---|---|
| `severity` | ○ | `error`(要修正) / `warning`(警告) / `info`(参考) |
| `stage` | ○ | `blind` = plan を見ない1回目の指摘 / `plan` = plan 照合で新たに出た指摘 |
| `title` | ○ | 指摘の要点を一文で |
| `location` | | `パス (新L109〜) [h010]` の形式。hunk id を添えると読み手が飛べる |
| `detail` | ○ | 何が問題か。「なぜまずいか」まで書く |
| `suggestion` | | 具体的な直し方 |
| `verdict` | | `kept` / `demoted`。plan 照合の結果 |
| `verdict_note` | | `demoted` のとき必須。plan のどこを読んで格下げしたか |

`demoted` はレポートから消えるのではなく、薄く「格下げ」バッジ付きで残る。
plan を読めば納得できるが実装としてはまだ改善余地がある、という指摘を握り潰さないため。

## 生成

```bash
python "$SKILL/scripts/build_report.py" --hunks hunks.json --review review.json -o report.html
```

hunk id の誤りや割り当て漏れがあると、ここで落ちてメッセージが出る。直して再実行する。
