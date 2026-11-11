/**
 * Host half of the Reply Hint bundle.
 *
 * Predicts the message the human is most likely to send next, so the Client
 * can show it as translucent ghost text above the empty composer and fill the
 * draft on Tab. One auxiliary model call per idle turn end, never a Session
 * event and never a model-visible message: the hint is browser presentation
 * only and disappears when the next turn starts.
 *
 * The Client reads the answer over the gateway's source-reflection Remote
 * path: this package registers the `replyHint` service with a `typertRemote`
 * binding and the `remote-methods` marker, and the Client calls
 * `replyHint/suggest` through `ctx.connection.rpc` with no generated
 * artifacts and no dependencies (a locally linked bundle cannot resolve
 * `@deepseek-ai/*` at import time).
 */

/** Property key the Gateway reads Remote markers from (dsh-typert-protocol). */
const REMOTE_METHODS = '@deepseek-ai/dsh-typert-protocol/remote-methods'

/** Recommended ceiling for the whole auxiliary call. */
const TIMEOUT_MS = 20_000
/** Output-token cap: a next message is a sentence or two, never an essay. */
const MAX_OUTPUT_TOKENS = 512
/** A hint older than this is regenerated even when the log looks unchanged. */
const HINT_TTL_MS = 10 * 60_000
/** Bound on the ghost text the Client has to render. */
const MAX_HINT_CHARS = 200
/** Messages of transcript tail handed to the model. */
const TAIL_MESSAGES = 8
/** Per-message and total input bounds. */
const MAX_MESSAGE_CHARS = 1_200
const MAX_INPUT_CHARS = 8_000
/** Settle delay after the turn goes idle, so the closing events are all logged. */
const IDLE_DELAY_MS = 800

const SYSTEM_PROMPT = [
  'You predict the next message a human user will send to this AI coding assistant.',
  'Read the conversation tail and write exactly the message you expect them to type next.',
  'Rules:',
  '- Match the language of the human messages.',
  '- One short message, at most 25 words, as the human would write it.',
  '- Plain text only: no quotes, labels, prefixes, explanations, Markdown, or code blocks.',
  '- If the assistant asked a question, answer it as the user would.',
  '- Prefer a concrete, useful next instruction over a generic one.',
  'Return exactly SKIP when no sensible next message exists.',
].join('\n')

/**
 * One predicted next message, keyed by Session on the Host.
 */
class ReplyHintService {
  /** Required Host services: the model runtime and the Session store. */
  static inject = ['llm', 'sessions']

  /**
   * @param ctx - Host plugin context.
   * @param config - unused today; the row is inserted with an empty object.
   */
  constructor(ctx, config) {
    void config
    this.ctx = ctx
    /** Session id → `{ hint, at, seq }`. */
    this.cache = new Map()
    /** Session id → `{ promise, controller }` for one in-flight prediction. */
    this.pending = new Map()

    // The Gateway's source-reflection discovery reads this binding and the
    // `remote-methods` prototype marker installed below.
    this.typertRemote = Object.freeze({ service: this, serviceKey: 'replyHint', namespace: 'replyHint' })
    ctx.provide('replyHint', this)

    ctx.effect(() => ctx.on('agent/status', payload => {
      // A running turn means the user already acted: the old hint is stale.
      if (payload.status === 'running') this.forget(String(payload.agent.session.id))
    }), 'reply-hint: forget a hint when the next turn starts')

    ctx.effect(() => () => {
      for (const entry of this.pending.values()) entry.controller.abort()
      this.pending.clear()
      this.cache.clear()
    }, 'reply-hint: abort pending predictions')
  }

  /**
   * Predict the human's next message for one Session.
   *
   * Returns a cached answer while it is fresh, otherwise awaits one model
   * call. Concurrent callers for the same Session share one call.
   * @param sessionId - Session identity from the Client.
   * @param signal - caller cancellation, injected by the Gateway.
   * @returns the predicted text, or `null` when there is nothing useful.
   */
  async suggest(sessionId, signal) {
    const key = String(sessionId)
    const cached = this.cache.get(key)
    const session = this.ctx.get('sessions')?.get(key)
    // A cached hint stays valid only while the log has not moved: any new
    // event means the user already acted or the assistant already answered.
    if (cached !== undefined && Date.now() - cached.at < HINT_TTL_MS
      && cached.seq === lastSeq(session)) return cached.hint
    const generation = this.generate(key, signal)
    if (generation === null) return null
    return await generation
  }

  /**
   * Share one in-flight prediction per Session.
   * @param sessionId - Session identity.
   * @param signal - caller cancellation.
   * @returns the shared promise, or `null` when a prediction is impossible now.
   */
  generate(sessionId, signal) {
    const existing = this.pending.get(sessionId)
    if (existing !== undefined) return existing.promise
    const controller = new AbortController()
    const forward = () => { controller.abort() }
    if (signal !== undefined) {
      if (signal.aborted) return null
      signal.addEventListener('abort', forward, { once: true })
    }
    const promise = (async () => {
      try {
        await sleep(IDLE_DELAY_MS, controller.signal)
        return await this.predict(sessionId, controller.signal)
      } catch (error) {
        if (!controller.signal.aborted) warn(error)
        return null
      } finally {
        if (signal !== undefined) signal.removeEventListener('abort', forward)
        this.pending.delete(sessionId)
      }
    })()
    this.pending.set(sessionId, { promise, controller })
    return promise
  }

  /**
   * Run one auxiliary model call over the Session's conversation tail.
   * @param sessionId - Session identity.
   * @param signal - cancellation for the call.
   * @returns normalized hint text, or `null` when the model declines.
   */
  async predict(sessionId, signal) {
    const sessions = this.ctx.get('sessions')
    const session = sessions?.get(sessionId)
    if (session === undefined) return null
    const events = typeof session.snapshotEvents === 'function' ? session.snapshotEvents() : []
    const route = lastRoute(events)
    if (route === null) return null
    const tail = transcriptTail(events)
    // Only a finished assistant answer invites a next user message.
    if (tail.length === 0 || tail[tail.length - 1].role !== 'assistant') return null

    const llm = this.ctx.get('llm')
    if (llm === undefined) return null
    const controller = new AbortController()
    const timer = setTimeout(() => { controller.abort() }, TIMEOUT_MS)
    const composed = AbortSignal.any([signal, controller.signal])
    try {
      const call = await llm.prepareCall({
        provider: route.provider,
        model: route.model,
        maxTokens: MAX_OUTPUT_TOKENS,
      }, composed, (controls, model) => {
        // A next-message hint needs no deliberation: pick the route's least
        // effort so the output cap measures the answer, not its thinking.
        const effort = model?.reasoning?.efforts?.[0]?.id
        return effort === undefined ? controls : { ...controls, reasoningEffort: effort }
      })
      const request = {
        ...call.config,
        messages: [{ role: 'user', content: [{ type: 'text', text: promptOf(tail) }] }],
        system: SYSTEM_PROMPT,
        sessionId: session.id,
        purpose: 'reply-hint',
        signal: composed,
      }
      let text = ''
      let finishKind = 'none'
      for await (const chunk of call.stream(request)) {
        if (chunk.type === 'text-delta') text += chunk.text
        else if (chunk.type === 'finish') finishKind = chunk.reason.kind
      }
      if (finishKind !== 'none' && finishKind !== 'stop') return null
      const hint = normalize(text)
      if (hint === null) return null
      this.cache.set(String(sessionId), { hint, at: Date.now(), seq: lastSeq(session) })
      return hint
    } finally {
      clearTimeout(timer)
    }
  }

  /**
   * Drop the hint and stop the prediction of one Session.
   * @param sessionId - Session identity.
   */
  forget(sessionId) {
    this.cache.delete(sessionId)
    const entry = this.pending.get(sessionId)
    if (entry !== undefined) entry.controller.abort()
    this.pending.delete(sessionId)
  }
}

// The Gateway reads Remote markers from the class prototype under this exact
// string key; the generated artifacts produce the same record.
Object.defineProperty(ReplyHintService.prototype, REMOTE_METHODS, {
  configurable: true,
  value: Object.freeze({
    version: 1,
    methods: Object.freeze([
      Object.freeze({ method: 'suggest', invocation: Object.freeze({ kind: 'direct' }) }),
    ]),
  }),
})

/**
 * The exact model route of the latest main request in one event log.
 * @param events - Session events, oldest first.
 * @returns `{ provider, model }`, or `null` before the first main request.
 */
function lastRoute(events) {
  for (let index = events.length - 1; index >= 0; index -= 1) {
    const event = events[index]
    if (event.type !== 'request/header') continue
    const config = event.data?.header?.config
    if (typeof config?.provider === 'string' && typeof config?.model === 'string') {
      return { provider: config.provider, model: config.model }
    }
    break
  }
  return null
}

/**
 * The last few human and assistant messages, oldest first.
 * @param events - Session events, oldest first.
 * @returns `{ role, text }` entries with empty messages dropped.
 */
function transcriptTail(events) {
  const tail = []
  for (let index = events.length - 1; index >= 0 && tail.length < TAIL_MESSAGES; index -= 1) {
    const event = events[index]
    const message = event.type === 'user/message'
      ? event.data
      : event.type === 'assistant/message' ? event.data?.message : undefined
    if (message === undefined) continue
    const text = textOf(message.content)
    if (text === '') continue
    tail.push({ role: message.role === 'assistant' ? 'assistant' : 'user', text: clamp(text, MAX_MESSAGE_CHARS) })
  }
  return tail.reverse()
}

/**
 * Join the text blocks of one message.
 * @param content - message content blocks.
 * @returns trimmed text.
 */
function textOf(content) {
  if (!Array.isArray(content)) return ''
  return content
    .filter(block => block?.type === 'text' && typeof block.text === 'string')
    .map(block => block.text)
    .join('\n')
    .trim()
}

/**
 * Frame the tail as JSON so user text cannot break structural delimiters.
 * @param tail - ordered transcript entries.
 * @returns the user input for the auxiliary call.
 */
function promptOf(tail) {
  const json = JSON.stringify(tail)
  const framed = `Next-message prediction from this JSON array of messages:\n${json}`
  return framed.length <= MAX_INPUT_CHARS ? framed : framed.slice(0, MAX_INPUT_CHARS)
}

/**
 * Accept one model answer as a hint.
 * @param raw - assembled model text.
 * @returns the normalized hint, or `null` when the model declined.
 */
function normalize(raw) {
  const text = raw.trim()
    .replace(/^```[a-z]*\n?/iu, '')
    .replace(/```$/u, '')
    .replace(/^["'“”`]+|["'“”`]+$/gu, '')
    .trim()
  if (text === '' || /^skip$/iu.test(text)) return null
  if (text.length > MAX_HINT_CHARS) return null
  return text
}

/**
 * Shorten one string to a character budget.
 * @param text - source text.
 * @param max - maximum characters.
 * @returns the unchanged text or a truncated prefix.
 */
function clamp(text, max) {
  return text.length <= max ? text : `${text.slice(0, max)}…`
}

/**
 * The last committed event sequence of one Session.
 * @param session - Session to read, when available.
 * @returns its newest event seq, or `-1` without a Session.
 */
function lastSeq(session) {
  if (session === undefined) return -1
  const events = typeof session.snapshotEvents === 'function' ? session.snapshotEvents() : []
  return events.length === 0 ? -1 : events[events.length - 1].seq
}

/**
 * Wait for a delay, rejecting when cancelled.
 * @param ms - delay in milliseconds.
 * @param signal - cancellation.
 * @returns a promise settled at the delay or on abort.
 */
function sleep(ms, signal) {
  return new Promise((resolve, reject) => {
    if (signal.aborted) {
      reject(new Error('aborted'))
      return
    }
    const timer = setTimeout(() => {
      signal.removeEventListener('abort', onAbort)
      resolve()
    }, ms)
    const onAbort = () => {
      clearTimeout(timer)
      reject(new Error('aborted'))
    }
    signal.addEventListener('abort', onAbort, { once: true })
  })
}

/**
 * Record one prediction failure without interrupting the turn.
 * @param error - thrown value.
 */
function warn(error) {
  const message = error instanceof Error ? error.message : String(error)
  console.warn(`[reply-hint] prediction failed: ${message}`)
}

export default ReplyHintService
