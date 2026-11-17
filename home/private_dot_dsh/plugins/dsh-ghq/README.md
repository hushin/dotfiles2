# dsh-ghq

DeepSeek Harness Web GUI bundle: open a repository that [ghq](https://github.com/x-motemen/ghq)
manages as a Workspace, and start a Session in it.

`ghq から開く` sits at the sidebar foot, directly above Settings. It opens a search
dialog listing every repository under the ghq root; choosing one registers it
as a Workspace (idempotent — an already registered path is reused) and opens a
new Session there, which is the same adoption the built-in **Add workspace**
flow performs after a directory pick.

## Behaviour

- The list comes from `ghq root` and `ghq list -p` on the Host. Nothing is
  cloned, moved, or written: an uncloned repository never appears.
- Rows show the path below the ghq root (`github.com/owner/repo`); the owner
  prefix is dimmed so the repository name reads first. A row whose path is
  already a registered Workspace carries a `登録済み` / Registered tag, and
  choosing it still opens a new Session in that Workspace.
- Typing filters by any part of the name (space-separated terms must all
  match); a new filter starts again at the first match. `↑`/`↓` move the
  active row, `Enter` opens it, `Esc` closes. Keys pressed during an input-method
  conversion belong to the conversion, not to the dialog.
- The listing is cached on the Host for a few seconds so reopening the dialog
  does not re-run ghq. Browser tabs share one run, and a tab closing cancels
  that run only when no other tab is still reading it.
- In the collapsed rail the action is a 36×36 icon; its label stays as the
  accessible name and the native tooltip.

## How the halves talk

A locally linked bundle cannot resolve `@deepseek-ai/*` imports, so this
package uses the gateway's source-reflection Remote path instead of generated
artifacts:

- `index.js` provides the `ghq` service with a `typertRemote` binding and the
  `@deepseek-ai/dsh-typert-protocol/remote-methods` prototype marker, and marks
  `list(signal)` as a direct Remote method.
- `client.js` calls `ghq/list` through the generic Connection RPC channel at
  `/api` (`ctx.connection.rpc.call`).

## Configuration

The row in `cordis.patch.yml` accepts an optional config, all fields optional:

| Field | Default | Meaning |
|---|---|---|
| `command` | `ghq` | Executable name or absolute path, resolved in the Host's PATH. |
| `timeoutMs` | `10000` | Ceiling for one ghq invocation. |
| `cacheTtlMs` | `5000` | Reuse window for one listing. |
| `cwd` | Host cwd | Directory the child process runs in. |

## Files

- `index.js` — Host half: the Remote service, the two ghq runs, the cache.
- `client.js` — Client half: the footer action, the search dialog, adoption.
- `cordis.patch.yml` — inserts the `ghq` row.
- `locale/*.json` — Plugin Manager title and description.

## Known limitations

- No `ghq get`: a repository must already be cloned.
- The list is fetched when the dialog opens; a clone made while the dialog is
  open shows up on the next open (or after the cache window).
- The dialog filters client-side, so the whole list is transferred once.
