// Markdown プレビューの文書言語を日本語にする。
//
// Chrome の `word-break: auto-phrase` (文節単位の折り返し) は、その要素の言語が
// 日本語と判定できるときだけ有効になる。プレビューの <html> には lang が無いので、
// ここで付けておく。書体のフォールバックや読み上げの判定にも効く。
document.documentElement.lang = "ja";
