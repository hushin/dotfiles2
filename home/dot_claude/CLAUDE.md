## Conversation Settings

Think in English and always converse in Japanese

## GitHub Repository Handling

- When given a `https://github.com/` URL, use `gh` CLI to fetch repository information:
  ```bash
  gh repo view <owner/repo> --json name,description,owner,url,defaultBranchRef,stargazerCount,primaryLanguage,repositoryTopics,createdAt,pushedAt
  ```
- To explore the entire repository locally, use `ghq get --shallow`:
  ```bash
  ghq get --shallow git@github.com:<owner>/<repo>.git
  ```
  Example: `ghq get --shallow git@github.com:hushin/dotfiles2.git`
