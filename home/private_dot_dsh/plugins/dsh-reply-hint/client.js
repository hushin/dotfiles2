/**
 * Client half of the Reply Hint bundle.
 *
 * Renders the Host's predicted next message as translucent ghost text in the
 * empty composer and fills the draft on Tab. The prediction is browser
 * presentation only: nothing here writes a Session event, so the log, the
 * model context, and replay stay untouched.
 *
 * The Host answers over the gateway's source-reflection Remote path
 * (`replyHint/suggest`); this half calls it through the generic Connection RPC
 * channel carried at `/api`, so the bundle needs no generated artifacts and no
 * importable dependencies.
 */

window.__ModuleLoader__.load({
  id: 'dsh-reply-hint',
  factory(require) {
    const React = require('react')
    const h = React.createElement
    const { useCallback, useEffect, useLayoutEffect, useRef, useState } = React

    const NS = 'reply-hint'
    const OVERLAY_SLOT = 'conversation.input.overlay'
    const ENDPOINT = 'replyHint/suggest'

    /** Copy the Client locale service owns; the keycap itself is not translated. */
    const DICTS = {
      en: { 'accept': 'Press Tab to insert this suggestion' },
      ja: { 'accept': 'Tab キーでこの候補を入力' },
      zh: { 'accept': '按 Tab 键填入此建议' },
    }

    /**
     * Styles for the ghost line and the placeholder override. The Host
     * placeholder keeps its own element; the `:has()` rule hides it exactly
     * while the ghost carries its marker attribute.
     */
    const CSS = `
[data-composer-card]:has([data-reply-hint]) [data-composer-placeholder] { opacity: 0; }
.dsh-reply-hint-key {
  display: inline-block;
  margin-left: 6px;
  padding: 0 4px;
  border: 1px solid var(--dsw-alias-line-subtle, rgba(127, 127, 127, 0.35));
  border-radius: 4px;
  font-size: 0.75em;
  line-height: 1.4;
  color: var(--dsw-alias-label-tertiary, rgba(127, 127, 127, 0.9));
  letter-spacing: 0;
  vertical-align: 1px;
}
`

    /**
     * Ask the Host for the next-message prediction of one Session.
     * @param ctx - Client plugin context carrying the Connection service.
     * @param sessionId - Session identity.
     * @param signal - cancellation owned by the caller effect.
     * @returns the predicted text, or `null` when the Host has none.
     */
    async function ask(ctx, sessionId, signal) {
      try {
        const result = await ctx.connection.rpc.call(
          '/api',
          ENDPOINT,
          { args: { sessionId } },
          signal,
        )
        if (!result.ok) {
          console.warn(`[reply-hint] ${result.error.code}: ${result.error.message}`)
          return null
        }
        return typeof result.value === 'string' && result.value !== '' ? result.value : null
      } catch (error) {
        if (signal.aborted) return null
        console.warn('[reply-hint] prediction request failed', error)
        return null
      }
    }

    /**
     * Build the ghost component for one Client context.
     * @param ctx - Client plugin context.
     * @returns the overlay slot component.
     */
    function createGhost(ctx) {
      return function ReplyHintGhost(props) {
        const { sessionId, useInput, inputActions, useSession, t } = props
        const input = useInput(state => state)
        const running = useSession(snapshot => snapshot.running)
        const draft = input?.draft ?? ''
        const phase = input?.phase ?? 'inert'
        const attachments = input?.attachmentIds?.length ?? 0

        const [hint, setHint] = useState(null)
        const [dismissed, setDismissed] = useState(false)
        const [layout, setLayout] = useState(null)
        const rootRef = useRef(null)
        const idleSerial = useRef(0)
        const lastRunning = useRef(running)
        const askedRef = useRef(null)

        // Every new turn invalidates the previous prediction.
        useEffect(() => {
          if (running && !lastRunning.current) {
            idleSerial.current += 1
            setHint(null)
            setDismissed(false)
          }
          lastRunning.current = running
        }, [running])

        // One request per idle window, only while the composer is genuinely
        // available: empty, plain, attachment-free, and not running.
        useEffect(() => {
          if (sessionId === undefined || running || draft !== '' || phase !== 'plain' || attachments > 0) return undefined
          const key = `${String(sessionId)}:${idleSerial.current}`
          if (askedRef.current === key) return undefined
          askedRef.current = key
          const controller = new AbortController()
          void ask(ctx, sessionId, controller.signal).then((text) => {
            if (controller.signal.aborted || text === null) return
            setHint(text)
            setDismissed(false)
          })
          return () => { controller.abort() }
        }, [sessionId, running, draft, phase, attachments])

        const visible = hint !== null && !dismissed && draft === '' && phase === 'plain'
          && attachments === 0 && !running

        // Measure the Host's placeholder box — it sits exactly on the text
        // origin — and mirror its typography on the ghost line.
        useLayoutEffect(() => {
          if (!visible) {
            setLayout(null)
            return undefined
          }
          const measure = () => {
            const root = rootRef.current
            const card = root?.closest('[data-composer-card]')
            if (root === null || card === null || card === undefined) return
            const target = card.querySelector('[data-composer-placeholder]')
              ?? card.querySelector('[data-input-scroll]')
            if (target === null) return
            const rootRect = root.getBoundingClientRect()
            const rect = target.getBoundingClientRect()
            const style = window.getComputedStyle(target)
            setLayout({
              left: rect.left - rootRect.left,
              top: rect.top - rootRect.top,
              width: rect.width,
              fontFamily: style.fontFamily,
              fontSize: style.fontSize,
              lineHeight: style.lineHeight,
              fontWeight: style.fontWeight,
              letterSpacing: style.letterSpacing,
            })
          }
          measure()
          window.addEventListener('resize', measure)
          return () => { window.removeEventListener('resize', measure) }
        }, [visible, hint, draft, attachments])

        const accept = useCallback(() => {
          if (hint === null) return
          const span = inputActions.captureInsertion()
          const applied = inputActions.insertText(hint, span)
          if (applied) {
            setHint(null)
            setDismissed(false)
          }
        }, [hint, inputActions])

        // Tab accepts, Escape dismisses; both only while the composer holds
        // focus, and Tab yields to an open `/` or `@` completion menu.
        useEffect(() => {
          if (!visible) return undefined
          const onKeyDown = (event) => {
            const root = rootRef.current
            const card = root?.closest('[data-composer-card]')
            if (card === null || card === undefined) return
            const active = document.activeElement
            if (active === null || !(active instanceof Node) || !card.contains(active)) return
            if (event.key === 'Escape') {
              event.preventDefault()
              event.stopPropagation()
              setDismissed(true)
              return
            }
            if (event.key !== 'Tab' || event.shiftKey || event.ctrlKey || event.altKey || event.metaKey) return
            if (event.isComposing) return
            if (card.querySelector('[role="listbox"]') !== null) return
            event.preventDefault()
            event.stopPropagation()
            accept()
          }
          window.addEventListener('keydown', onKeyDown, true)
          return () => { window.removeEventListener('keydown', onKeyDown, true) }
        }, [visible, accept])

        if (sessionId === undefined) return null
        const label = typeof t === 'function' ? t('accept') : 'Press Tab to insert this suggestion'
        return h('div', {
          ref: rootRef,
          'data-reply-hint': visible ? '' : undefined,
          style: { position: 'absolute', inset: 0, pointerEvents: 'none' },
        }, visible && layout !== null
          ? h('div', {
            'aria-hidden': true,
            title: label,
            style: {
              position: 'absolute',
              left: `${layout.left}px`,
              top: `${layout.top}px`,
              width: `${layout.width}px`,
              fontFamily: layout.fontFamily,
              fontSize: layout.fontSize,
              lineHeight: layout.lineHeight,
              fontWeight: layout.fontWeight,
              letterSpacing: layout.letterSpacing,
              whiteSpace: 'pre-wrap',
              wordBreak: 'break-word',
              overflowWrap: 'anywhere',
              color: 'var(--dsw-alias-label-caption)',
              opacity: 0.68,
              pointerEvents: 'none',
              userSelect: 'none',
              transition: 'opacity 120ms ease-out',
            },
          },
          hint,
          h('span', { className: 'dsh-reply-hint-key', 'aria-hidden': true }, 'Tab'))
          : null)
      }
    }

    return {
      inject: ['slots', 'locale', 'connection'],
      apply(ctx) {
        ctx.effect(() => ctx.locale.register(NS, DICTS), 'reply-hint: dictionaries')
        ctx.slots.inject(OVERLAY_SLOT, () => ctx.slots.register({
          name: OVERLAY_SLOT,
          id: 'reply-hint',
          order: 5,
          locale: NS,
        }, createGhost(ctx)))
        // Component-local styles are rendered with the tree so unmounting
        // removes them; the placeholder override lives here too.
        ctx.effect(() => {
          const style = document.createElement('style')
          style.textContent = CSS
          document.head.append(style)
          return () => { style.remove() }
        }, 'reply-hint: styles')
      },
    }
  },
})
