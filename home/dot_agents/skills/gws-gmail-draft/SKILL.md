---
name: gws-gmail-draft
description: "Gmail: Create a draft email without sending it, so a human can review or edit it first. Use when the user asks for a draft, a 下書き, or wants to check an outgoing email before it goes out."
metadata:
  version: 0.22.5
  handwritten: true
  openclaw:
    category: "productivity"
    requires:
      bins:
        - gws
    cliHelp: "gws gmail +send --help"
---

# gmail draft

> **PREREQUISITE:** Read `../gws-shared/SKILL.md` for auth, global flags, and security rules.

Create a draft with the `DRAFT` label. Nothing is sent until someone sends it.

`gws` has no `+draft` helper. Draft creation is `+send` with `--draft`, which builds the RFC 5322 message and stores it instead of sending.

## Usage

```bash
gws gmail +send --to <EMAILS> --subject <SUBJECT> --body <TEXT> --draft
```

## Flags

| Flag | Required | Description |
|------|----------|-------------|
| `--to` | ✓ | Recipient email address(es), comma-separated |
| `--subject` | ✓ | Email subject |
| `--body` | ✓ | Email body (plain text, or HTML with `--html`) |
| `--draft` | ✓ | Save as draft. Without it the helper sends immediately |
| `--cc` | — | CC email address(es), comma-separated |
| `--bcc` | — | BCC email address(es), comma-separated |
| `--html` | — | Treat `--body` as an HTML fragment |
| `--from` | — | Send-as alias. Omit for the account default |
| `-a, --attach <PATH>` | — | Attach a file, repeatable, 25MB total |
| `--dry-run` | — | Print the request without creating the draft |

## Examples

```bash
gws gmail +send --to alice@example.com --subject 'Hello' --body 'Hi Alice!' --draft
gws gmail +send --to alice@example.com --subject 'Hello' --body 'Hi!' --cc bob@example.com --draft
gws gmail +send --to alice@example.com --subject 'Hello' --body '<p>Bold <b>text</b></p>' --html --draft
```

## Verify

```bash
gws gmail users drafts list --params '{"userId":"me","maxResults":5}' --format table
gws gmail users drafts get --params '{"userId":"me","id":"<DRAFT_ID>","format":"full"}'
```

Open it at `https://mail.google.com/mail/u/0/#drafts/<DRAFT_ID>`.

## Send later

Only after the user asks.

```bash
gws gmail users drafts send --params '{"userId":"me"}' --json '{"id":"<DRAFT_ID>"}'
```

## Tips

- `--body` is plain text. Pass HTML fragments with `--html`, no `<html>` wrapper.
- The draft lands in the authenticated account. `--from` needs a configured send-as alias.
- A draft is personal data. Keep bodies and draft IDs out of commits and shared logs.

> [!CAUTION]
> `+send` without `--draft` sends immediately. Pass `--draft` unless the user has approved sending.

## Troubleshooting

`403 insufficientPermissions` right after `gws auth login --scopes ...` means the cached access token predates the new scope. Drop the cache and retry.

```bash
mv ~/.config/gws/token_cache.json /tmp/token_cache.bak.json
```

The next call refreshes the token from the stored refresh token. Drafting needs `https://www.googleapis.com/auth/gmail.compose`. Confirm with `gws auth status`.

## See Also

- [gws-gmail](../gws-gmail/SKILL.md) — All Gmail resources and helpers
- [gws-shared](../gws-shared/SKILL.md) — Global flags and auth
