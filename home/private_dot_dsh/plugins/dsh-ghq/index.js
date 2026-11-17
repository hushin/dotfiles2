/**
 * Host half of the ghq bundle.
 *
 * Lists the repositories ghq manages so the Client can offer them as
 * workspaces. Nothing is cloned, moved, or written: this half only runs the
 * `ghq` executable in the Host's execution world and projects its output.
 *
 * The Client reads the answer over the gateway's source-reflection Remote
 * path: this package registers the `ghq` service with a `typertRemote` binding
 * and the `remote-methods` marker, and the Client calls `ghq/list` through
 * `ctx.connection.rpc`. A locally linked bundle cannot resolve `@deepseek-ai/*`
 * at import time, so the protocol marker is applied by hand exactly as the
 * generated artifacts would.
 */

/** Property key the Gateway reads Remote markers from (dsh-typert-protocol). */
const REMOTE_METHODS = '@deepseek-ai/dsh-typert-protocol/remote-methods'

/** Default executable name; `ghq` resolves through the subprocess PATH. */
const DEFAULT_COMMAND = 'ghq'
/** Ceiling for one ghq invocation. */
const DEFAULT_TIMEOUT_MS = 10_000
/** Reuse window for the repository listing, so a burst of opens costs one run. */
const DEFAULT_CACHE_TTL_MS = 5_000
/** Retained stdout cap: a large ghq root still fits comfortably. */
const MAX_STDOUT_BYTES = 8 * 1024 * 1024
/** Retained stderr tail for diagnostics. */
const MAX_STDERR_BYTES = 16 * 1024
/** Time a terminated ghq child gets to exit. */
const TERMINATE_GRACE_MS = 2_000
/** Segments shown when a repository lives outside the reported ghq root. */
const NAME_SEGMENTS = 3

/**
 * Host `ghq` service: the repository list behind the Client's picker.
 */
class GhqService {
  /** Required Host services: process execution only. */
  static inject = ['subprocess']

  /**
   * @param ctx - Host plugin context.
   * @param config - optional `{ command, timeoutMs, cacheTtlMs, cwd }` overrides.
   */
  constructor(ctx, config = {}) {
    this.ctx = ctx
    this.command = typeof config.command === 'string' && config.command !== '' ? config.command : DEFAULT_COMMAND
    this.timeoutMs = positive(config.timeoutMs, DEFAULT_TIMEOUT_MS)
    this.cacheTtlMs = positive(config.cacheTtlMs, DEFAULT_CACHE_TTL_MS)
    this.cwd = typeof config.cwd === 'string' && config.cwd.startsWith('/') ? config.cwd : process.cwd()
    /** `{ value, at }` for the last successful listing. */
    this.cache = undefined
    /** The single in-flight listing, shared by concurrent callers. */
    this.pending = undefined

    // The Gateway's source-reflection discovery reads this binding and the
    // `remote-methods` prototype marker installed below.
    this.typertRemote = Object.freeze({ service: this, serviceKey: 'ghq', namespace: 'ghq' })
    ctx.provide('ghq', this)
  }

  /**
   * List the repositories ghq manages.
   *
   * A cached listing is reused briefly so opening the picker twice in a row
   * does not re-run ghq. Concurrent callers share one run, and one caller
   * leaving — a closed dialog in another browser tab — cancels that run only
   * when no caller is left to read it.
   * @param signal - caller cancellation, injected by the Gateway.
   * @returns `{ root, repositories }`, where each repository carries its ghq
   *   name (`github.com/owner/repo`) and absolute path.
   */
  async list(signal) {
    const cached = this.cache
    if (cached !== undefined && Date.now() - cached.at < this.cacheTtlMs) return cached.value
    if (signal !== undefined && signal.aborted) throw new Error('ghq list was aborted')
    let entry = this.pending
    if (entry === undefined) {
      const control = new AbortController()
      const fresh = { control, readers: 0, promise: undefined }
      fresh.promise = this.inspect(control.signal).then(
        (value) => {
          this.cache = { value, at: Date.now() }
          return value
        },
      ).finally(() => {
        if (this.pending === fresh) this.pending = undefined
      })
      entry = fresh
      this.pending = fresh
    }
    const joined = entry
    joined.readers += 1
    /** Leave the shared run, cancelling it when no reader remains. */
    const leave = () => {
      joined.readers -= 1
      if (joined.readers <= 0) joined.control.abort()
    }
    if (signal === undefined) {
      try {
        return await joined.promise
      } finally {
        leave()
      }
    }
    let onAbort
    try {
      const cancelled = new Promise((_resolve, reject) => {
        onAbort = () => { reject(new Error('ghq list was aborted')) }
        signal.addEventListener('abort', onAbort, { once: true })
      })
      return await Promise.race([joined.promise, cancelled])
    } finally {
      if (onAbort !== undefined) signal.removeEventListener('abort', onAbort)
      leave()
    }
  }

  /**
   * Run ghq once and join its two answers.
   * @param signal - cancellation owned by the caller.
   * @returns the repository listing.
   */
  async inspect(signal) {
    const root = (await this.run(['root'], signal)).trim()
    const listed = await this.run(['list', '-p'], signal)
    const repositories = []
    for (const line of listed.split('\n')) {
      const path = line.trim()
      if (path === '') continue
      repositories.push({ name: ghqName(root, path), path })
    }
    repositories.sort((left, right) => left.name.localeCompare(right.name))
    return { root: root === '' ? null : root, repositories }
  }

  /**
   * Run the ghq executable and return its stdout.
   * @param args - ghq arguments; never shell-interpreted.
   * @param signal - cancellation inherited from the caller.
   * @returns collected stdout.
   */
  async run(args, signal) {
    const timeout = AbortSignal.timeout(this.timeoutMs)
    const control = AbortSignal.any([signal, timeout])
    const handle = this.ctx.subprocess.spawn({
      argv: [this.command, ...args],
      cwd: this.cwd,
      stdio: {
        stdin: 'ignore',
        stdout: { maxBytes: MAX_STDOUT_BYTES },
        stderr: { maxBytes: MAX_STDERR_BYTES },
      },
      graceMs: TERMINATE_GRACE_MS,
      signal: control,
    })
    const outcome = await handle.done
    if (signal.aborted) throw new Error('ghq list was aborted')
    if (timeout.aborted) throw new Error(`ghq ${args.join(' ')} timed out after ${this.timeoutMs}ms`)
    const stdout = handle.collected.stdout?.readFrom(0)
    const stderr = handle.collected.stderr?.readFrom(0)?.text ?? ''
    if (outcome.exitCode !== 0) {
      const detail = stderr.trim() === '' ? `exit code ${outcome.exitCode}` : stderr.trim()
      throw new Error(`ghq ${args.join(' ')} failed: ${detail}`)
    }
    if (stdout?.lossy === true) throw new Error('ghq output exceeded the retained size limit')
    return stdout?.text ?? ''
  }
}

// The Gateway reads Remote markers from the class prototype under this exact
// string key; the generated artifacts produce the same record.
Object.defineProperty(GhqService.prototype, REMOTE_METHODS, {
  configurable: true,
  value: Object.freeze({
    version: 1,
    methods: Object.freeze([
      Object.freeze({ method: 'list', invocation: Object.freeze({ kind: 'direct' }) }),
    ]),
  }),
})

/**
 * The display name of one repository: its path below the ghq root, or the last
 * three segments when the repository lives outside that root.
 * @param root - ghq root, or an empty string when ghq reported none.
 * @param path - absolute repository path.
 * @returns the name the picker shows.
 */
function ghqName(root, path) {
  const normalizedRoot = root.replace(/\/+$/u, '')
  if (normalizedRoot !== '' && path.startsWith(`${normalizedRoot}/`)) {
    return path.slice(normalizedRoot.length + 1)
  }
  const segments = path.split('/').filter(segment => segment !== '')
  return segments.slice(-NAME_SEGMENTS).join('/')
}

/**
 * Accept a configured positive number, or fall back.
 * @param value - configured value.
 * @param fallback - default.
 * @returns the usable number.
 */
function positive(value, fallback) {
  return typeof value === 'number' && Number.isFinite(value) && value > 0 ? value : fallback
}

export default GhqService
