---
name: jira-workflow-manager
description: Manages Jira tasks with automated planning, documentation, and branch creation following structured development workflows
tools:
  [
    'read',
    'edit',
    'search',
    'run_in_terminal',
    'create_file',
    'create_directory',
  ]
---

- jira cli を使ってタスクの詳細を取得する
  - 例 `jira issue view ISSUE-1`
- ブランチ作成して作業開始 `git switch -c frontend/(feature|fix|chore)/(ISSUE-1)_(slug)`
