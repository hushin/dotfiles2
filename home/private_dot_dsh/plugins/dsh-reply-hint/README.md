# dsh-reply-hint

DeepSeek Harness Web GUI bundle: predicts the message you are most likely to
send next and shows it as translucent ghost text in the empty composer.
`Tab` inserts it into the draft, `Escape` dismisses it.

This is the Claude-Code-style "next prompt suggestion" affordance, wired into
the resident composer of the Harness Web UI.

## Behaviour

- After an assistant turn ends (and the Session is idle), the Client asks the
  Host for one prediction. The Host runs one auxiliary model call over the
  last few messages and returns a single short message in the user's
  language. An explicit `SKIP` from the model shows nothing.
- The hint appears only while the composer is empty, in the plain phase, with
  no attachments, and the Session is not running. Typing, sending, or starting
  a new turn hides it.
- The ghost line sits exactly on the Host placeholder, copies its typography,
  and hides that placeholder while it shows. A small `Tab` keycap advertises
  the accept gesture.
- `Tab` yields to an open `/` or `@` completion menu, and only acts while the
  composer holds focus.

The prediction never becomes Session state: no event is appended, nothing
enters model context, and replay is unaffected.

## How the halves talk

A locally linked bundle cannot resolve `@deepseek-ai/*` imports, so this
package uses the gateway's source-reflection Remote path instead of generated
artifacts:

- `index.js` provides the `replyHint` service with a `typertRemote` binding and
  the `@deepseek-ai/dsh-typert-protocol/remote-methods` prototype marker, and
  marks `suggest(sessionId, signal)` as a direct Remote method.
- `client.js` calls `replyHint/suggest` through the generic Connection RPC
  channel at `/api` (`ctx.connection.rpc.call`).

## Cost and controls

One auxiliary call per idle turn, capped at 512 output tokens with the
route's lowest reasoning effort and a 20-second deadline. The route follows the
Session's latest `request/header`, so it uses the model the Session already
runs. Hints are cached per Session and invalidated as soon as the log moves.
The constants at the top of `index.js` (`TIMEOUT_MS`, `MAX_OUTPUT_TOKENS`,
`TAIL_MESSAGES`, `MAX_HINT_CHARS`, `IDLE_DELAY_MS`) are the tuning points.

## Files

- `index.js` — Host half: the Remote service, the model call, the cache.
- `client.js` — Client half: the overlay slot entry, the ghost renderer, the
  `Tab`/`Escape` handling.
- `cordis.patch.yml` — inserts the `reply-hint` row.
- `locale/*.json` — Plugin Manager title and description.

## Known limitations

- The ghost is placed by measuring the Host placeholder's box, because the
  conversation slot surface does not expose an inline decoration hook.
- A prediction failure is silent on screen; the Host logs a warning and the
  composer behaves as if no hint existed.
- The Session must have at least one completed model request (for its route)
  and end on an assistant message before a hint is attempted.
