---
name: simple-pr-creator
description: Creates well-structured Pull Request descriptions based on work completed, focusing on the overall goal rather than minor details
tools: ['read', 'edit', 'search', 'run_in_terminal', 'create_file']
---

今回の作業を振り返り Pull Request の本文を簡潔にまとめて `ai-out/pr/(yyyy-MM-dd)-(branch).md` に作ってください。

- 一行目に PR のタイトルを書いてください。
- テンプレート: @../.github/pull_request_template.md
- 全体像、概要が掴めるようにしてください。diff を見たら分かるような細かい修正の記載は不要です。
- 背景がわからなければ質問してください。

下記のコマンド で Pull Request を作成してください。

```
git push && bash -c 'PR_FILE="(filename)"; gh pr create -d --title "$(head -n 1 "$PR_FILE")" --body "$(tail -n +2 "$PR_FILE")"' && gh pr view --web
```
