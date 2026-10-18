# Markdown Preview: Light + Japanese (local extension)

VS Code の組み込み Markdown プレビューを **常にライト表示** にし、**日本語組版** を整えるための
ローカル拡張。chezmoi が `~/.vscode/extensions/local.markdown-preview-light-jp-1.0.0/` に配置する。

コードは持たず、`contributes.markdown.previewStyles` / `previewScripts` で CSS と 3 行の JS を
プレビューに差し込むだけの拡張。

## なぜ `markdown.styles` ではなく拡張なのか

ユーザー設定の `markdown.styles` では要件を満たせない。

| 方法 | 結果 |
| --- | --- |
| `markdown.styles` にホーム配下の CSS を絶対パス指定 | プレビューの webview は **ワークスペースフォルダ配下か拡張機能が提供するリソースしか読み込まない** ため `Could not load 'markdown.styles'` になる |
| `markdown.styles` に `https://` の URL | Strict なプレビュー CSP (`style-src` に `https:` が無い) でブロックされる |
| ワークスペース内に CSS を置く | そのワークスペースでしか効かない |
| `${userHome}` などの変数 | `markdown.styles` は変数展開に対応していない |

拡張機能の `markdown.previewStyles` は上記の制限を受けず、全ワークスペース・オフラインで効く。

## 適用

```sh
chezmoi apply
```

その後 VS Code で `Developer: Reload Window`（または再起動）。CSS を書き換えたときも同じ。

> Settings Sync の拡張機能同期には載せないように `settingsSync.ignoredExtensions` に
> `local.markdown-preview-light-jp` を入れてある（マーケットプレイスに無い拡張のため）。
> 別マシンへは chezmoi で配布する。

## 調整

`preview.css` の先頭にあるデザイントークンだけ触ればほぼ済む。

- 色: `--md-*`
- 書体: `--md-font-body`（明朝にしたい場合は `--md-font-body` を mincho 系へ差し替え）
- 本文サイズ・行間: `--md-font-size`, `--md-line-height`
- 行長: `--md-measure`（`max-width: var(--md-measure)` を消せば全幅）

日本語向けに入れている指定:

- `line-break: strict` — 禁則処理を厳格に
- `word-break: auto-phrase` — 文節単位の折り返し（`preview.js` が `<html lang="ja">` を付ける）
- `text-autospace: normal` — 和欧・和数の間のアキ
- `text-spacing-trim: normal` — 約物のカーニング
- 見出しは `font-feature-settings: "palt" 1` + `text-wrap: balance`
- コードブロック内は上記を打ち消す（等幅の桁を保つため）

## 注意

- ワークベンチのテーマに関係なく常にライトで描画する（それが目的）。
  暗いテーマで暗いプレビューに戻したい場合は、この拡張を削除する。
- ハイコントラストテーマは特別扱いしていない。
