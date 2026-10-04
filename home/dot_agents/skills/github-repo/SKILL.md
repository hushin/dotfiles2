---
name: github-repo
description: >-
  Fetches GitHub repository information with the `gh` CLI and clones repositories
  locally with `ghq` for exploration. Use this skill whenever the user shares a
  https://github.com/ URL or an owner/repo reference, asks about a repository's
  description, language, stars, topics, or default branch, or wants to browse or
  clone someone else's repository to read its code.
---

# GitHub Repository Handling

Use the GitHub CLI (`gh`) for remote metadata and `ghq` for local clones. Both respect the user's existing authentication, so prefer them over scraping the web or manually running `git clone` into arbitrary directories.

## Fetch repository information

When given a `https://github.com/<owner>/<repo>` URL, extract `owner/repo` and run:

```bash
gh repo view <owner/repo> --json name,description,owner,url,defaultBranchRef,stargazerCount,primaryLanguage,repositoryTopics,createdAt,pushedAt
```

## Clone for local exploration

To read the entire repository locally, use `ghq get --shallow` (shallow clone keeps it fast):

```bash
ghq get --shallow git@github.com:<owner>/<repo>.git
```

Example:

```bash
ghq get --shallow git@github.com:hushin/dotfiles2.git
```

`ghq` puts the clone under its root directory (e.g. `~/ghq/github.com/<owner>/<repo>`). Find the exact path with `ghq list --full-path --exact <owner>/<repo>` instead of guessing, then explore that path.
