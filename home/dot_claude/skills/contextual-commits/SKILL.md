---
name: contextual-commits
description: Use when writing a git commit, when the user asks to commit, when contextual-commits style is desired, or when the commit body should preserve why in Japanese without inventing context from the diff alone.
model: haiku
---

# Writing Japanese Contextual Commits

Write a Conventional Commit subject, then add Japanese action lines only for reasoning the diff cannot show. **The body preserves WHY, not WHAT.**

## When to Use

- User asks to run `git commit`
- A commit message must follow contextual-commits style
- The commit body should be in Japanese
- You need to preserve intent, decisions, rejections, constraints, or learnings

Do not use for empty commits or trivial commits that need only a clean subject line.

## Rules

1. Check commit scope in this order:
   - `git diff --cached --stat`
   - `git diff --cached`
   - if nothing is staged, inspect unstaged/untracked changes and stage intentionally
2. If there is no diff to commit, stop and say so. Do not invent a commit.
3. The subject line stays Conventional Commits format: `type(scope): summary`.
4. The body uses typed action lines in Japanese:
   - `intent(scope): 何を達成したいか`
   - `decision(scope): 何を採用したか`
   - `rejected(scope): 何を見送ったか`
   - `constraint(scope): どんな制約があったか`
   - `learned(scope): 実装中に分かったこと`
5. Do not restate the diff in the body. If the diff already shows it, skip it.
6. If you lack conversation context, do not fabricate `intent`, `rejected`, `constraint`, or `learned`. In that case, use a subject line only, or at most a `decision(...)` line that is clearly visible in the diff.
7. Most commits need 0-3 action lines. Prefer omission over noise.
8. This skill cannot force a specific AI model. If cost matters, recommend a cheaper model such as `gpt-5-mini` or `claude-haiku-4.5`, but do not claim the model choice is enforced by the skill.

## Quick Reference

| Need                | Rule                                                       |
| ------------------- | ---------------------------------------------------------- |
| No staged changes   | Inspect unstaged/untracked files, then stage intentionally |
| No changes at all   | Do not commit                                              |
| Subject             | Conventional Commits                                       |
| Body language       | Japanese                                                   |
| No session context  | Do not invent reasons                                      |
| Cheap model request | Recommend `/model`, cannot enforce from skill              |

## Example

```text
feat(auth): Google ログインを追加

intent(auth): ソーシャルログインを段階的に導入したい
decision(oauth): passport.js を採用し複数 provider 対応を優先した
rejected(oauth): auth0-sdk は既存 session 方針と相性が悪く見送った
constraint(callback): 既存の /api/auth/callback/:provider 規約に合わせる必要があった
learned(google): refresh token 取得には offline_access 指定が必要だった
```

## Common Mistakes

- body で diff の内容を言い換える
- 根拠のない `intent` や `constraint` を捏造する
- staged changes があるのに unstaged changes を混ぜる
- 差分がないのに commit を作ろうとする
