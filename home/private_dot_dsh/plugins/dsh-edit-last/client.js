/**
 * Client half of the Edit Last Message bundle.
 *
 * A pencil in the latest user message's action row (immediately left of Copy)
 * withdraws that message and everything after it from the model context and
 * puts its text back into the composer, ready to correct and send again.
 *
 * Two private seams make the row action possible, both with defensive guards
 * and full restore on unload:
 *
 * - The Chat row ledger exposes no user-message action slot, so the `user` and
 *   `steering` slot components are wrapped: the official renderer runs
 *   unchanged and the actions element it builds gains one `extraActions` child.
 * - The transcript is log-ordered, so hiding the withdrawn turn needs the
 *   registered `chat` view builder: nodes inside a successful edit's
 *   `[target .. marker]` span (and the plugin's placeholder node) become
 *   `visibility: 'hidden'`, which the Chat builder already honours for order,
 *   locations, turn navigation, grouping, and virtualization.
 *
 * The command's settlement is watched through the Chat snapshot rather than a
 * row component: the row unmounts as soon as the withdrawal hides it, so the
 * composer refill must outlive the button that started it.
 */

window.__ModuleLoader__.load({
  id: 'dsh-edit-last',
  factory(require) {
    const React = require('react')
    const h = React.createElement
    const { cloneElement, isValidElement, useCallback, useEffect, useMemo, useRef, useState } = React

    const NS = 'edit-last'
    const COMMAND = 'edit-last'
    const SOURCE_KIND = 'dsh-edit-last'
    const OUTCOME_TIMEOUT_MS = 20_000
    const CHAT_TARGET = 'chat'
    const CHAT_NODE_SLOT = 'conversation.chat.node'
    const TICK_KEY = 'dsh-edit-last:tick'
    const SHIPPED_PRIORITY = 0

    const DICTS = {
      en: {
        'edit': 'Edit this message',
        'disabled.draft': 'Clear the composer to edit this message',
        'disabled.attachments': 'Remove draft attachments to edit this message',
        'disabled.images': 'Messages with attachments cannot be edited',
        'disabled.sending': 'Wait for the current send to finish',
        'failed': 'Edit failed',
        'unavailable': 'The edit command is unavailable in this session',
        'timeout': 'Edit did not finish; try again',
        'busy': 'An edit is already running in this session',
        'editing': 'Editing the last message — sending replaces the original turn',
        'editingShort': 'Editing',
        'applying': 'Replacing…',
        'apply': 'Replace and send',
        'cancelEdit': 'Cancel edit',
        'empty': 'Type the message to send',
      },
      ja: {
        'edit': 'このメッセージを編集',
        'disabled.draft': '入力欄を空にするとこのメッセージを編集できます',
        'disabled.attachments': '添付を外すとこのメッセージを編集できます',
        'disabled.images': '添付付きのメッセージは編集できません',
        'disabled.sending': '送信中は編集できません',
        'failed': '編集に失敗しました',
        'unavailable': 'このセッションでは編集コマンドを利用できません',
        'timeout': '編集が完了しませんでした。もう一度お試しください',
        'busy': 'このセッションでは別の編集が実行中です',
        'editing': '直近のメッセージを編集中 — 送信すると元のターンを置き換えます',
        'editingShort': '編集中',
        'applying': '置き換え中…',
        'apply': '置き換えて送信',
        'cancelEdit': '編集をキャンセル',
        'empty': '送信する文を入力してください',
      },
    }

    /** Pencil matching the message action row's 28px glyph cells. */
    const CSS = `
.dsh-edit-last-wrap { order: -1; position: relative; display: inline-flex; align-items: center; }
.dsh-edit-last-action {
  display: inline-flex; align-items: center; justify-content: center;
  width: calc(28px + var(--dsh-content-font-delta, 0px));
  height: calc(28px + var(--dsh-content-font-delta, 0px));
  margin: 0; padding: 6px; border: none;
  border-radius: var(--dsw-radius-sm);
  background: transparent; color: var(--dsw-alias-label-tertiary);
  font: inherit; cursor: pointer;
}
.dsh-edit-last-action svg { width: calc(15px + var(--dsh-content-font-delta, 0px)); height: calc(15px + var(--dsh-content-font-delta, 0px)); flex: none; }
.dsh-edit-last-action:disabled { opacity: 0.4; cursor: default; }
@media (hover: hover) {
  .dsh-edit-last-action:not(:disabled):hover {
    background: var(--dsw-alias-interactive-bg-hover);
    color: var(--dsw-alias-label-secondary);
  }
}
.dsh-edit-last-action:focus-visible { outline: none; box-shadow: 0 0 0 2px var(--dsw-focus-ring-color, var(--dsw-alias-brand-primary)); }
.dsh-edit-last-action[aria-disabled] { opacity: 0.4; cursor: default; }
.dsh-edit-last-working { animation: dsh-edit-last-pulse 1.2s ease-in-out infinite; }
@keyframes dsh-edit-last-pulse { 0%, 100% { opacity: 1 } 50% { opacity: 0.35 } }
@media (prefers-reduced-motion: reduce) { .dsh-edit-last-working { animation: none } }
.dsh-edit-last-action-active { color: var(--dsw-alias-brand-primary); background: var(--dsw-alias-bg-layer-2); }
.dsh-edit-last-bar {
  display: flex; align-items: center; gap: 8px;
  margin: 0 0 6px; padding: 6px 10px;
  border: 0.5px solid var(--dsw-alias-border-l1); border-radius: 10px;
  background: var(--dsw-alias-bg-layer-2);
  color: var(--dsw-alias-label-secondary); font-size: 12px; line-height: 1.4;
}
.dsh-edit-last-bar-text { flex: 1 1 auto; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.dsh-edit-last-bar-error { color: var(--dsw-alias-state-error-primary); max-width: 40%; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.dsh-edit-last-bar-actions { display: inline-flex; gap: 6px; flex: none; }
.dsh-edit-last-bar-actions button {
  height: 24px; padding: 0 10px; border-radius: 999px;
  border: 0.5px solid var(--dsw-alias-border-l1);
  background: transparent; color: inherit; font: inherit; cursor: pointer;
}
.dsh-edit-last-bar-actions button:focus-visible { outline: none; box-shadow: 0 0 0 2px var(--dsw-focus-ring-color, var(--dsw-alias-brand-primary)); }
.dsh-edit-last-bar-actions button:disabled { opacity: 0.5; cursor: default; }
.dsh-edit-last-bar-apply { border-color: var(--dsw-alias-brand-primary) !important; color: var(--dsw-alias-brand-primary) !important; }
@media (hover: hover) { .dsh-edit-last-bar-actions button:not(:disabled):hover { background: var(--dsw-alias-interactive-bg-hover); } }
.dsh-edit-last-note {
  position: absolute; top: calc(100% + 6px); right: 0; z-index: 30;
  width: max-content; max-width: 260px; padding: 6px 8px;
  border: 0.5px solid var(--dsw-alias-border-l1); border-radius: 6px;
  background: var(--dsw-alias-bg-overlay); color: var(--dsw-alias-state-error-primary);
  font-size: 12px; line-height: 1.4; text-align: left; white-space: normal;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.12);
}
`

    const PENCIL = h('svg', {
      width: 15, height: 15, viewBox: '0 0 16 16', fill: 'none', 'aria-hidden': true,
    }, h('path', {
      d: 'M10.9 2.3a1.5 1.5 0 0 1 2.1 2.1L5.6 11.9l-2.9.7.7-2.9 7.5-7.4Z',
      stroke: 'currentColor', strokeWidth: 1.25, strokeLinejoin: 'round',
    }))

    /** Fallback input state for a slot entry that does not hand over `useInput`. */
    const EMPTY_INPUT = { draft: '', phase: 'plain', attachmentIds: [] }

    /** Per-session edit in flight; a second click must not run the same command twice. */
    const pending = new Set()

    /** Join a user message's text blocks the same way the transcript renders them. */
    function messageText(data) {
      const content = Array.isArray(data?.content) ? data.content : []
      const parts = []
      for (const block of content) {
        if (block?.type === 'text' && typeof block.text === 'string') parts.push(block.text)
      }
      return parts.join('')
    }

    /** Whether a message carries anything a text edit cannot restore. */
    function hasAttachments(data) {
      const content = Array.isArray(data?.content) ? data.content : []
      return content.some(block => block?.type !== 'text')
    }

    /** Latest visible user-authored row, or null when a command is the newest input. */
    function latestUserSeq(chat) {
      const order = chat?.order
      const nodes = chat?.nodes
      if (!Array.isArray(order) || nodes === undefined || nodes === null) return null
      for (let index = order.length - 1; index >= 0; index -= 1) {
        const node = nodes.get(order[index])
        if (node === undefined) continue
        if (node.kind === 'command' || node.kind === 'command-input') return null
        if (node.kind !== 'user' && node.kind !== 'steering') continue
        return typeof node.data?.seq === 'number' ? node.data.seq : null
      }
      return null
    }

    /** Every materialized Chat node, including rows the visibility layer hid. */
    function chatNodes(chat) {
      const nodes = chat?.nodes
      if (nodes === undefined || nodes === null) return []
      const values = nodes.values
      if (typeof values === 'function') return [...values.call(nodes)]
      const order = Array.isArray(chat?.order) ? chat.order : []
      const out = []
      for (const key of order) {
        const node = nodes.get(key)
        if (node !== undefined) out.push(node)
      }
      return out
    }

    /** Newest settled outcome of this plugin's command for one target seq. */
    function outcomeFor(chat, seq) {
      let found
      for (const node of chatNodes(chat)) {
        if (node.kind !== 'command') continue
        const data = node.data
        if (data === null || typeof data !== 'object' || data.name !== COMMAND) continue
        if (data.outcome === null || data.outcome === undefined) continue
        const match = /@(\d+)/.exec(typeof data.args === 'string' ? data.args : '')
        if (match === null || Number(match[1]) !== seq) continue
        if (found === undefined || (data.seq ?? 0) > (found.data.seq ?? 0)) found = node
      }
      return found === undefined ? null : found.data.outcome ?? null
    }

    /** The retained conversation binding's named view, when available. */
    function chatView(ctx, sessionId) {
      try {
        return ctx.get('uiConversation')?.binding?.(sessionId)?.target?.(CHAT_TARGET)
      } catch {
        return undefined
      }
    }

    /** Per-session composer facade: draft write, focus, notice, and live input state. */
    function composerFacade(ctx, sessionId) {
      try {
        const conversation = ctx.get('conversation')
        const input = conversation?.input
        const scope = ctx.get('sessions')?.scope?.(sessionId)
        if (input === undefined || scope === undefined || scope === null) return undefined
        const shell = () => input.for(scope)
        return {
          setDraft: text => { shell().setDraft(text) },
          focus: () => { shell().focus() },
          notify: (level, text) => { shell().notify(level, text) },
          submit: (mode) => { shell().submit(mode) },
          state: () => shell().state?.getSnapshot?.(),
        }
      } catch {
        return undefined
      }
    }

    // ---- edit mode ---------------------------------------------------------

    /** Per-session edit mode: target, previous draft, and apply status. */
    const edits = new Map()
    const editListeners = new Set()

    /** Notify the edit bar and row controls after a mode change. */
    function publishEdits() {
      for (const listener of [...editListeners]) {
        try { listener() } catch { /* a stale subscriber must not break the mode */ }
      }
    }

    /** @param listener - mode-change subscriber. @returns unsubscribe. */
    function subscribeEdits(listener) {
      editListeners.add(listener)
      return () => { editListeners.delete(listener) }
    }

    /**
     * Enter edit mode for one message. The conversation is untouched: only the
     * composer draft changes, so everything above (and below) stays visible
     * until the user sends or cancels.
     * @param ctx - client root context.
     * @param sessionId - receiving session.
     * @param seq - target user message.
     * @param text - text to load into the composer.
     * @param t - plugin translator.
     * @returns null on success, otherwise the message to show.
     */
    function startEdit(ctx, sessionId, seq, text, t) {
      try {
        const current = edits.get(sessionId)
        if (current?.seq === seq) {
          composerFacade(ctx, sessionId)?.focus()
          return null
        }
        const facade = composerFacade(ctx, sessionId)
        const state = facade?.state()
        if (state !== undefined) {
          if (state.phase !== 'plain') return t('disabled.sending')
          if ((state.attachmentIds?.length ?? 0) > 0) return t('disabled.attachments')
          if (state.draft.trim() !== '') return t('disabled.draft')
        }
        edits.set(sessionId, {
          seq,
          originalText: text,
          previousDraft: state?.draft ?? '',
          applying: false,
          error: null,
        })
        facade?.setDraft(text)
        facade?.focus()
        publishEdits()
        return null
      } catch (error) {
        return error instanceof Error ? error.message : String(error)
      }
    }

    /** Leave edit mode without touching the conversation and restore the previous draft. */
    function cancelEdit(ctx, sessionId) {
      const edit = edits.get(sessionId)
      if (edit === undefined || edit.applying) return
      try {
        const facade = composerFacade(ctx, sessionId)
        facade?.setDraft(edit.previousDraft ?? '')
        facade?.focus()
      } catch {
        // The session may be gone; dropping the mode is still correct.
      }
      edits.delete(sessionId)
      publishEdits()
    }

    /**
     * Withdraw one message and wait for the Host's settled outcome.
     * @param ctx - client root context.
     * @param sessionId - receiving session.
     * @param seq - target user message.
     * @param t - plugin translator.
     * @returns null on success, otherwise the message to show.
     */
    async function withdraw(ctx, sessionId, seq, t) {
      if (pending.has(sessionId)) return t('busy')
      pending.add(sessionId)
      try {
        const view = chatView(ctx, sessionId)
        return await new Promise(resolve => {
          let settled = false
          let unsubscribe
          let timer
          const finish = (outcome, failureText) => {
            if (settled) return
            settled = true
            pending.delete(sessionId)
            try { if (typeof unsubscribe === 'function') unsubscribe() } catch { /* torn down */ }
            if (timer !== undefined) clearTimeout(timer)
            if (outcome?.kind === 'success') resolve(null)
            else resolve(outcome?.text ?? failureText ?? t('failed'))
          }
          const check = () => {
            if (settled) return
            try {
              const outcome = outcomeFor(view?.getSnapshot?.(), seq)
              if (outcome !== null) finish(outcome)
            } catch {
              // A view torn down mid-read settles by timeout.
            }
          }
          try { unsubscribe = view?.subscribe?.(check) } catch { unsubscribe = undefined }
          timer = setTimeout(() => { finish(undefined, t('timeout')) }, OUTCOME_TIMEOUT_MS)
          check()

          const command = ctx.get('sessions')?.binding?.(sessionId)?.session?.command?.(`/${COMMAND} @${seq}`)
          if (command === undefined) {
            finish(undefined, t('unavailable'))
            return
          }
          command.then(
            value => {
              if (value?.ok !== true) finish(undefined, value?.error?.message)
              else if (value.value?.matched !== true) finish(undefined, t('unavailable'))
            },
            error => { finish(undefined, error instanceof Error ? error.message : String(error)) },
          )
        })
      } finally {
        pending.delete(sessionId)
      }
    }

    /**
     * Apply the edit: withdraw the old turn first, then submit the edited draft.
     * Runs outside React so the bar and the row can unmount without losing it.
     * @param ctx - client root context.
     * @param sessionId - receiving session.
     * @param t - plugin translator.
     */
    async function applyEdit(ctx, sessionId, t) {
      const edit = edits.get(sessionId)
      if (edit === undefined || edit.applying) return
      const facade = composerFacade(ctx, sessionId)
      const draft = facade?.state()?.draft ?? ''
      if (draft.trim() === '') {
        try { facade?.notify('error', t('empty')) } catch { /* no composer */ }
        return
      }
      edit.applying = true
      edit.error = null
      publishEdits()
      const failure = await withdraw(ctx, sessionId, edit.seq, t)
      if (failure !== null) {
        edit.applying = false
        edit.error = failure
        publishEdits()
        try { facade?.notify('error', failure) } catch { /* no composer */ }
        return
      }
      try {
        facade?.focus()
        facade?.submit?.('queue')
      } catch (error) {
        edit.applying = false
        edit.error = error instanceof Error ? error.message : String(error)
        publishEdits()
        return
      }
      edits.delete(sessionId)
      publishEdits()
    }

    /** Pencil rendered into the latest user row's action strip; enters edit mode. */
    function EditRowAction({ sessionId, seq, text, ctx, t, messageHasAttachments, isEditing, useInput }) {
      const useInputSafe = typeof useInput === 'function' ? useInput : selector => selector(EMPTY_INPUT)
      const draft = useInputSafe(state => state.draft)
      const phase = useInputSafe(state => state.phase)
      const attachments = useInputSafe(state => state.attachmentIds.length)
      const [note, setNote] = useState(null)
      const reason = isEditing
        ? null
        : messageHasAttachments
          ? 'images'
          : attachments > 0
            ? 'attachments'
            : phase !== 'plain'
              ? 'sending'
              : draft.trim() !== ''
                ? 'draft'
                : null
      const title = isEditing ? t('editingShort') : reason === null ? t('edit') : t(`disabled.${reason}`)
      const onClick = () => {
        const message = startEdit(ctx, sessionId, seq, text, t)
        setNote(message === null ? null : message)
      }
      return h('span', { className: 'dsh-edit-last-wrap' },
        h('button', {
          type: 'button',
          className: `dsh-edit-last-action${isEditing ? ' dsh-edit-last-action-active' : ''}`,
          title,
          'aria-label': t('edit'),
          'aria-pressed': isEditing || undefined,
          'data-edit-last': seq,
          onClick,
        }, PENCIL),
        note === null ? null : h('span', {
          className: 'dsh-edit-last-note',
          role: 'status',
          title: note,
        }, note),
      )
    }

    /** Subscribe to one Session's edit mode for the row and bar controls. */
    function useEditState(sessionId) {
      const [state, setState] = useState(() => edits.get(sessionId))
      useEffect(() => {
        setState(edits.get(sessionId))
        return subscribeEdits(() => { setState(edits.get(sessionId)) })
      }, [sessionId])
      return state
    }

    /**
     * Edit-mode bar above the composer. Enter and the primary send control are
     * intercepted so a normal send turns into "replace and send" instead of
     * appending a duplicate message after the old turn.
     */
    function EditBar({ sessionId, ctx, t }) {
      const edit = useEditState(sessionId)
      const active = edit !== undefined
      useEffect(() => {
        if (!active) return undefined
        const onKeyDown = (event) => {
          if (event.key !== 'Enter' || event.shiftKey || event.isComposing || event.keyCode === 229) return
          const target = event.target
          if (!(target instanceof Element) || target.closest('[data-composer-input]') === null) return
          if (document.querySelector('[role="listbox"], [role="menu"]') !== null) return
          const draft = composerFacade(ctx, sessionId)?.state()?.draft ?? ''
          if (draft.trim() === '' || draft.trimStart().startsWith('/')) return
          event.preventDefault()
          event.stopPropagation()
          void applyEdit(ctx, sessionId, t)
        }
        const onClick = (event) => {
          const target = event.target instanceof Element ? event.target.closest('button') : null
          if (target === null) return
          const card = target.closest('[data-composer-card]')
          if (card === null) return
          const buttons = [...card.querySelectorAll('button')]
          // The primary send control is the composer card's last button.
          if (buttons.length === 0 || buttons[buttons.length - 1] !== target) return
          const draft = composerFacade(ctx, sessionId)?.state()?.draft ?? ''
          if (draft.trim() === '' || draft.trimStart().startsWith('/')) return
          event.preventDefault()
          event.stopPropagation()
          void applyEdit(ctx, sessionId, t)
        }
        document.addEventListener('keydown', onKeyDown, true)
        document.addEventListener('click', onClick, true)
        return () => {
          document.removeEventListener('keydown', onKeyDown, true)
          document.removeEventListener('click', onClick, true)
        }
      }, [active, ctx, sessionId, t])

      if (edit === undefined) return null
      return h('div', { className: 'dsh-edit-last-bar', role: 'region', 'aria-label': t('editing') },
        h('span', { className: 'dsh-edit-last-bar-text' }, edit.applying ? t('applying') : t('editing')),
        edit.error === null ? null : h('span', { className: 'dsh-edit-last-bar-error', role: 'status' }, edit.error),
        h('span', { className: 'dsh-edit-last-bar-actions' },
          h('button', {
            type: 'button',
            className: 'dsh-edit-last-bar-apply',
            disabled: edit.applying,
            onClick: () => { void applyEdit(ctx, sessionId, t) },
          }, t('apply')),
          h('button', {
            type: 'button',
            className: 'dsh-edit-last-bar-cancel',
            disabled: edit.applying,
            onClick: () => { cancelEdit(ctx, sessionId) },
          }, t('cancelEdit')),
        ),
      )
    }

    /** Subscribe to one session's Chat snapshot for the row's recency gate. */
    function useChatSnapshot(ctx, sessionId) {
      const view = useMemo(() => (sessionId === undefined ? undefined : chatView(ctx, sessionId)), [ctx, sessionId])
      const read = useCallback(() => {
        try {
          return view?.getSnapshot?.()
        } catch {
          return undefined
        }
      }, [view])
      const [snapshot, setSnapshot] = useState(read)
      useEffect(() => {
        setSnapshot(read())
        let unsubscribe
        try { unsubscribe = view?.subscribe?.(() => { setSnapshot(read()) }) } catch { unsubscribe = undefined }
        return typeof unsubscribe === 'function' ? unsubscribe : undefined
      }, [view, read])
      return snapshot
    }

    /** The bare render function of a registered slot component, unwrapping memo/forwardRef. */
    function rendererOf(component) {
      if (typeof component === 'function') return component
      if (component === null || typeof component !== 'object') return undefined
      const type = component.type
      return typeof type === 'function' ? type : undefined
    }

    /**
     * Wrap one official user-row renderer so its actions row gains the pencil.
     * The official renderer is hook-free, so calling it directly preserves its
     * element identity and every prop this wrapper does not touch.
     */
    function rowWithAction(official, ctx, t) {
      return function RowWithEditAction(props) {
        const sessionId = typeof props.sessionId === 'string' ? props.sessionId : undefined
        const chat = useChatSnapshot(ctx, sessionId)
        const editState = useEditState(sessionId)
        const latestSeq = useMemo(() => latestUserSeq(chat), [chat])
        const node = props.node
        const data = node?.data
        const seq = typeof data?.seq === 'number' ? data.seq : undefined
        const row = official(typeof props.renderSlotChain === 'function'
          ? props
          : { ...props, renderSlotChain: () => null })
        if (!isValidElement(row) || sessionId === undefined || seq === undefined) return row
        if ((node.kind !== 'user' && node.kind !== 'steering') || latestSeq !== seq) return row
        const actions = row.props.actions
        if (typeof actions !== 'function') return row
        try {
          const withEdit = (text) => {
            const actionsRow = actions(text)
            if (!isValidElement(actionsRow)) return actionsRow
            return cloneElement(actionsRow, {
              extraActions: h(EditRowAction, {
                sessionId,
                seq,
                text,
                ctx,
                t,
                messageHasAttachments: hasAttachments(data),
                isEditing: editState?.seq === seq,
                useInput: props.useInput,
              }),
            })
          }
          return cloneElement(row, { actions: withEdit })
        } catch {
          // A shape mismatch leaves the official row intact.
          return row
        }
      }
    }

    /**
     * Take over the shipped user/steering row components and keep them wrapped
     * as the slot ledger changes.
     * @param ctx - client root context.
     * @param t - plugin translator.
     * @returns teardown restoring every taken-over identity.
     */
    function installRowAction(ctx, t) {
      const slots = ctx.slots
      if (slots === undefined || typeof slots.entries !== 'function') return () => {}
      const taken = new Map()
      const tick = () => {
        try { slots.register?.({ name: CHAT_NODE_SLOT, key: TICK_KEY }, () => null)() } catch {
          // The tick only shortens the wait for a re-render.
        }
      }
      const takeOver = (entry) => {
        if (taken.has(entry)) return false
        const official = rendererOf(entry.component)
        if (official === undefined) return false
        const replacement = rowWithAction(official, ctx, t)
        taken.set(entry, { original: entry.component, replacement })
        entry.component = replacement
        return true
      }
      const sync = () => {
        let changed = false
        let entries = []
        try { entries = slots.entries(CHAT_NODE_SLOT) } catch { entries = [] }
        for (const entry of entries) {
          const key = entry?.options?.key
          if (key !== 'user' && key !== 'steering') continue
          if ((entry.options.priority ?? SHIPPED_PRIORITY) !== SHIPPED_PRIORITY) continue
          changed = takeOver(entry) || changed
        }
        if (changed) tick()
      }
      sync()
      let unsubscribe
      try { unsubscribe = slots.subscribe?.(CHAT_NODE_SLOT, sync) } catch { unsubscribe = undefined }
      return () => {
        try { if (typeof unsubscribe === 'function') unsubscribe() } catch { /* torn down */ }
        for (const [entry, record] of taken) {
          if (entry.component === record.replacement) entry.component = record.original
        }
        taken.clear()
        tick()
      }
    }

    /** One executed edit's hidden span: target seq through marker seq, inclusive. */
    function cutsOf(nodes) {
      const cuts = []
      for (const node of nodes) {
        if (node?.kind !== 'command') continue
        const data = node.data
        if (data === null || typeof data !== 'object' || data.name !== COMMAND) continue
        const outcome = data.outcome
        if (outcome === null || outcome === undefined || outcome.kind !== 'success') continue
        const marker = outcome.sourceEventSeq
        if (!Number.isSafeInteger(marker)) continue
        // The client always passes @seq, while a user-typed `/edit-last` omits
        // it; the Host echoes the effective target as @seq in its result text.
        const args = typeof data.args === 'string' ? data.args : ''
        const text = typeof outcome.text === 'string' ? outcome.text : ''
        const match = /@(\d+)/.exec(args) ?? /@(\d+)/.exec(text)
        if (match === null) continue
        const target = Number(match[1])
        if (!Number.isSafeInteger(target) || target > marker) continue
        cuts.push({ start: target, end: marker })
      }
      return cuts
    }

    function signatureOf(cuts) {
      return cuts.map(cut => `${String(cut.start)}-${String(cut.end)}`).join(',')
    }

    /**
     * Wrapper over the registered Chat builder. It preserves every node object
     * and only forces `visibility: 'hidden'` on nodes inside an executed edit
     * span or on this plugin's own placeholder node.
     */
    class EditVisibilityBuilder {
      constructor(inner) {
        this.inner = inner
        this.originals = new Map()
        this.cuts = []
        this.signature = ''
      }

      get empty() {
        return this.inner.empty
      }

      replace(input) {
        this.originals = new Map()
        for (const node of input.nodes) this.originals.set(node.key, node)
        this.cuts = cutsOf(this.originals.values())
        this.signature = signatureOf(this.cuts)
        return this.inner.replace({ nodes: this.deriveAll(), timeline: input.timeline })
      }

      apply(input) {
        for (const node of input.upserts) this.originals.set(node.key, node)
        if (input.upserts.some(node => node.kind === 'command')) {
          this.cuts = cutsOf(this.originals.values())
          const signature = signatureOf(this.cuts)
          if (signature !== this.signature) {
            this.signature = signature
            return this.inner.replace({ nodes: this.deriveAll(), timeline: input.timeline })
          }
        }
        return this.inner.apply({
          upserts: input.upserts.map(node => this.hide(node)),
          timeline: input.timeline,
          ...(input.changedTurns === undefined ? {} : { changedTurns: input.changedTurns }),
        })
      }

      groupInput() {
        return this.inner.groupInput?.()
      }

      publish() {
        this.inner.publish?.()
      }

      deriveAll() {
        return [...this.originals.values()].map(node => this.hide(node))
      }

      hide(node) {
        if (node.visibility === 'hidden') return node
        const source = node.kind === 'context' ? node.data?.source : undefined
        const inCut = source?.kind === SOURCE_KIND
          || this.cuts.some(cut => node.anchorSeq >= cut.start && node.anchorSeq <= cut.end)
        return inCut ? { ...node, visibility: 'hidden' } : node
      }
    }

    /** Rebuild every retained conversation so an installed wrapper reaches open Sessions. */
    function rebuildOpenSessions(ctx) {
      try {
        const ui = ctx.get('uiConversation')
        const list = ctx.get('sessions')?.list?.getSnapshot?.()
        const ids = new Set([...(list?.ids ?? []), ...Object.keys(list?.byId ?? {})])
        for (const id of ids) {
          try {
            ui?.binding?.(id)?.rebuild?.()
          } catch {
            // A Session may be mid-teardown; the next registration retries.
          }
        }
      } catch {
        // Listing is best-effort: new Sessions still pick up the wrapper.
      }
    }

    /**
     * Wrap the registered `chat` view builder for this plugin's lifetime.
     * @param ctx - client root context.
     * @param ui - the uiConversation service.
     * @returns cleanup restoring the registry definition.
     */
    function installVisibility(ctx, ui) {
      if (ui === undefined || ui === null || ui.views === undefined) return () => {}
      const views = ui.views
      const patches = new Map()

      const ensure = () => {
        try {
          const definition = views.entries().find(candidate => candidate?.target === CHAT_TARGET)
          if (definition === undefined || typeof definition.create !== 'function') return
          const current = patches.get(definition)
          if (current !== undefined && definition.create === current.wrapper) return
          const original = definition.create
          const wrapper = function wrappedChatBuilder() {
            return new EditVisibilityBuilder(original.call(definition))
          }
          definition.create = wrapper
          patches.set(definition, { original, wrapper })
          rebuildOpenSessions(ctx)
        } catch {
          // A closed registry shape degrades to an unhidden transcript, never a crash.
        }
      }

      let unsubscribe
      try {
        unsubscribe = views.subscribe?.(ensure)
      } catch {
        unsubscribe = undefined
      }
      ensure()

      return () => {
        try { if (typeof unsubscribe === 'function') unsubscribe() } catch { /* torn down */ }
        for (const [definition, record] of patches) {
          if (definition.create === record.wrapper) definition.create = record.original
        }
        rebuildOpenSessions(ctx)
      }
    }

    return {
      inject: ['slots', 'locale'],
      apply(ctx) {
        ctx.effect(() => ctx.locale.register(NS, DICTS), 'edit-last locale')
        ctx.effect(() => {
          const style = document.createElement('style')
          style.dataset.plugin = 'dsh-edit-last'
          style.textContent = CSS
          document.head.append(style)
          return () => { style.remove() }
        }, 'edit-last styles')
        const t = ctx.locale.bind(NS)
        ctx.effect(() => installRowAction(ctx, t), 'edit-last message action')
        ctx.slots.inject('conversation.input.dock', () => ctx.slots.register({
          name: 'conversation.input.dock',
          id: 'edit-last-bar',
          order: 5,
        }, props => h(EditBar, { ...props, ctx, t })))
        if (typeof ctx.inject === 'function') {
          ctx.inject(['uiConversation'], child => child.effect(
            () => installVisibility(ctx, child.get('uiConversation')),
            'edit-last transcript hiding',
          ))
        }
      },
    }
  },
})
