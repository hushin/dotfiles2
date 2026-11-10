# グローバルルール

## Web 検索・取得

- **検索は Jina AI を Bash から使う。** 第一選択は `jsearch "<query>"`（`~/bin/jsearch`、chezmoi 管理）。
- **`web_search` ツールはこの環境では失敗する**（`DEEPSEEK_API_KEY` 未設定）。リトライせず
  `jsearch` に切り替える。
- 検索結果を使った回答では出典 URL を Markdown リンクで示す。
