/**
 * Host half of the Edit Last Message bundle.
 *
 * Registers one session command, `edit-last`, that withdraws the latest human
 * user message from the model-visible surface so the Client can put its text
 * back in the composer. The session log stays append-only: the withdrawal is
 * one replacement `user/message` whose `surfaceOp` spans the target through
 * the current surface tail and whose `sourceEventSeqs` names every shadowed
 * node. The same technique backs `/compact`, so replay, resume, and fork keep
 * deriving their model context from the log.
 *
 * The replacement node carries a placeholder text and the producer-owned
 * source kind `dsh-edit-last`; the Client hides that node (and the withdrawn
 * span) from the transcript and reads the old text from the Chat snapshot
 * before invoking this command.
 */

export const inject = ['commands']

/** Command name without the leading slash. */
const COMMAND_NAME = 'edit-last'

/** Message source kind owned by this plugin; the Client keys transcript hiding on it. */
const SOURCE_KIND = 'dsh-edit-last'

/**
 * Height of the placeholder. Any strict provider accepts a non-empty user
 * text, and an unmistakable placeholder is better than an empty node if a
 * later model request ever surfaces it.
 */
const PLACEHOLDER_TEXT = '(message withdrawn for editing)'

/** Upper bound on waiting for a cancelled turn to reach quiescence. */
const IDLE_TIMEOUT_MS = 20_000

/** Sessions with an edit in flight; a second click must not plan the same tail twice. */
const inFlight = new Set()

/**
 * Register the `edit-last` command for the lifetime of this plugin.
 * @param ctx - Host plugin context (commands service).
 * @returns the command registration disposer.
 */
export function apply(ctx) {
  return ctx.effect(
    () => ctx.commands.register({
      name: COMMAND_NAME,
      description: 'Edit the latest sent message: withdraw it from the model context and return its text to the composer',
      input: { hint: '[@seq]' },
      handler: invocation => execute(invocation),
    }),
    'edit-last command',
  )
}

/**
 * Resolve the requested target. An omitted or blank input means "the latest
 * message", so typing `/edit-last` and pressing the composer button agree.
 * @param rawInput - text after the command name.
 * @returns the requested seq, `undefined` for the latest, or `null` when the input is malformed.
 */
function parseTarget(rawInput) {
  const raw = rawInput.trim()
  if (raw === '') return undefined
  const match = /^@?(\d+)$/.exec(raw)
  if (match === null) return null
  const value = Number(match[1])
  return Number.isSafeInteger(value) ? value : null
}

/**
 * Withdraw the target message from the current surface.
 * @param invocation - command invocation carrying the receiving Agent and signal.
 * @returns the command result shown as the command row's outcome.
 */
async function execute(invocation) {
  const agent = invocation.agent
  const sessionId = String(agent.session.id)
  if (inFlight.has(sessionId)) {
    return failure('An edit is already running in this session.')
  }
  inFlight.add(sessionId)
  try {
    const requested = parseTarget(invocation.rawInput)
    if (requested === null) return failure('Usage: /edit-last [@seq]')

    if (agent.status !== 'idle') {
      agent.cancel({ kind: 'user' }, { keepInbox: true })
      if (!(await waitForIdle(agent, invocation.signal))) {
        return failure('The running turn did not stop in time. Try again in a moment.')
      }
    }
    if (invocation.signal.aborted) return failure('Edit cancelled.')

    // Pending steering belongs to the future being withdrawn: keeping it would
    // deliver it first on the next send. Queued next-turn messages are left
    // alone because the Queue dock already owns per-item edit/remove.
    for (const message of [...agent.inbox.nextStep]) agent.inbox.remove(message.id)

    const plan = planEdit(agent.session, requested)
    if ('failure' in plan) return failure(plan.failure)

    const marker = {
      id: crypto.randomUUID(),
      role: 'user',
      content: [{ type: 'text', text: PLACEHOLDER_TEXT }],
      source: { kind: SOURCE_KIND },
    }
    let event
    try {
      event = agent.session.append('user/message', marker, {
        surfaceOp: { op: 'replace', startSeq: plan.start, endSeq: plan.end },
        sourceEventSeqs: plan.shadowed,
      })
    } catch (error) {
      return failure(`Could not withdraw the message: ${messageOf(error)}`)
    }
    return {
      kind: 'success',
      text: `Withdrew @${String(plan.targetSeq)} from the conversation; its text is back in the composer. Edit it and send again.`,
      sourceEventSeq: event.seq,
    }
  } catch (error) {
    return failure(`Edit failed: ${messageOf(error)}`)
  } finally {
    inFlight.delete(sessionId)
  }
}

/**
 * Validate a target against the live log and surface.
 * @param session - session to read.
 * @param requested - explicit seq, or undefined for the latest human message.
 * @returns the replacement plan, or a human-readable failure.
 */
function planEdit(session, requested) {
  const events = session.snapshotEvents()
  const surface = session.surface.nodes
  if (surface.length === 0) return { failure: 'There is no message to edit yet.' }

  const eventAt = seq => events.find(event => event.seq === seq)
  const isHuman = event => event.type === 'user/message' && event.data.source?.kind === 'user'

  let target
  if (requested === undefined) {
    for (let index = surface.length - 1; index >= 0; index -= 1) {
      const event = eventAt(surface[index])
      if (event !== undefined && isHuman(event)) {
        target = event
        break
      }
    }
  } else {
    target = eventAt(requested)
  }
  if (target === undefined) return { failure: 'That message is not in this session.' }
  if (!isHuman(target)) return { failure: 'Only a user message can be edited.' }

  const targetIndex = surface.indexOf(target.seq)
  if (targetIndex < 0) return { failure: 'That message is no longer in the model context.' }

  // Only the latest human message is editable: an older target would also
  // withdraw every later exchange, which the product does not offer.
  for (let index = targetIndex + 1; index < surface.length; index += 1) {
    const later = eventAt(surface[index])
    if (later !== undefined && isHuman(later)) {
      return { failure: 'A newer message has arrived. Use /edit-last again to edit the latest one.' }
    }
  }

  return {
    targetSeq: target.seq,
    start: surface[targetIndex],
    end: surface[surface.length - 1],
    shadowed: surface.slice(targetIndex),
  }
}

/**
 * Wait for the Agent to reach quiescence after a cancel.
 * @param agent - receiving agent.
 * @param signal - command cancellation signal.
 * @returns whether idle was reached before the timeout or an abort.
 */
async function waitForIdle(agent, signal) {
  if (signal.aborted) return false
  let timer
  let onAbort
  try {
    await Promise.race([
      agent.whenIdle(),
      new Promise((_resolve, reject) => {
        timer = setTimeout(() => { reject(new Error('idle timeout')) }, IDLE_TIMEOUT_MS)
        onAbort = () => { reject(new Error('aborted')) }
        signal.addEventListener('abort', onAbort, { once: true })
      }),
    ])
    return true
  } catch {
    return false
  } finally {
    if (timer !== undefined) clearTimeout(timer)
    if (onAbort !== undefined) signal.removeEventListener('abort', onAbort)
  }
}

/** Build one error command result. */
function failure(text) {
  return { kind: 'error', text }
}

/** Render any thrown value as a one-line message. */
function messageOf(error) {
  return error instanceof Error ? error.message : String(error)
}
