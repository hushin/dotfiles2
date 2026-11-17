/**
 * Client half of the ghq bundle.
 *
 * A sidebar-foot action beside Settings opens a search dialog listing the
 * repositories ghq manages on the Host; choosing one registers it as a
 * Workspace (idempotent) and opens a new Session there, exactly as the
 * built-in Add workspace flow does after a directory pick.
 *
 * The Host answers over the gateway's source-reflection Remote path
 * (`ghq/list`); this half calls it through the generic Connection RPC channel
 * at `/api`, so the bundle needs no generated artifacts and no importable
 * dependencies beyond the browser module table.
 */

window.__ModuleLoader__.load({
  id: 'dsh-ghq',
  factory(require) {
    const React = require('react')
    const ReactDOM = require('react-dom')
    const h = React.createElement
    const { useCallback, useEffect, useMemo, useRef, useState } = React

    const NS = 'ghq'
    /** Slot the sidebar foot declares for actions beside Settings. */
    const FOOTER_SLOT = 'sidebar.footer.action'
    const ENDPOINT = 'ghq/list'
    /** Id prefix of this dialog's list and options, for the combobox wiring. */
    const DIALOG_ID = 'dsh-ghq-picker'

    const DICTS = {
      en: {
        'action': 'Open from ghq',
        'dialog.title': 'Open a ghq repository',
        'dialog.description': 'Pick a repository to open as a workspace and start a session in it.',
        'search.placeholder': 'Filter by owner, repository, or path',
        'search.label': 'Filter repositories',
        'close': 'Close',
        'loading': 'Reading ghq…',
        'empty': 'No repository matches.',
        'empty.none': 'ghq manages no repository under its root yet. Clone one with ghq get first.',
        'failed': 'Could not read ghq.',
        'retry': 'Retry',
        'registered': 'Registered',
        'opening': 'Opening…',
        'no.permission': 'Your peer may not create workspaces.',
        'no.session': 'The workspace was registered, but this build cannot open a session for it.',
        'open.failed': 'Could not open the repository.',
        'no.workspaces': 'The workspace service is unavailable in this build.',
        'count.one': '1 repository',
        'count': '{count} repositories',
      },
      ja: {
        'action': 'ghq から開く',
        'dialog.title': 'ghq リポジトリを開く',
        'dialog.description': 'リポジトリを選ぶとワークスペースとして登録し、その中でセッションを開始します。',
        'search.placeholder': 'オーナー・リポジトリ・パスで絞り込む',
        'search.label': 'リポジトリを絞り込む',
        'close': '閉じる',
        'loading': 'ghq を読み込み中…',
        'empty': '一致するリポジトリがありません。',
        'empty.none': 'ghq の管理下にリポジトリがまだありません。先に ghq get で clone してください。',
        'failed': 'ghq を読み込めませんでした。',
        'retry': '再試行',
        'registered': '登録済み',
        'opening': '開いています…',
        'no.permission': 'この接続ではワークスペースを作成できません。',
        'no.session': 'ワークスペースは登録しましたが、このビルドではセッションを開けませんでした。',
        'open.failed': 'リポジトリを開けませんでした。',
        'no.workspaces': 'このビルドではワークスペース機能を利用できません。',
        'count.one': '1 件',
        'count': '{count} 件',
      },
      zh: {
        'action': '从 ghq 打开',
        'dialog.title': '打开 ghq 仓库',
        'dialog.description': '选择一个仓库即可将其注册为工作区并在其中开始会话。',
        'search.placeholder': '按所有者、仓库或路径筛选',
        'search.label': '筛选仓库',
        'close': '关闭',
        'loading': '正在读取 ghq…',
        'empty': '没有匹配的仓库。',
        'empty.none': 'ghq 根目录下还没有仓库。请先用 ghq get 克隆。',
        'failed': '无法读取 ghq。',
        'retry': '重试',
        'registered': '已注册',
        'opening': '正在打开…',
        'no.permission': '当前连接无法创建工作区。',
        'no.session': '工作区已注册，但此版本无法为其打开会话。',
        'open.failed': '无法打开该仓库。',
        'no.workspaces': '此版本无法使用工作区功能。',
        'count.one': '1 个仓库',
        'count': '{count} 个仓库',
      },
    }

    /** Folder glyph for the footer action and the dialog's leading mark. */
    const ICON = h('svg', {
      width: '1em', height: '1em', viewBox: '0 0 16 16', fill: 'none', 'aria-hidden': true,
    }, h('path', {
      d: 'M1.75 4.25a1 1 0 0 1 1-1h3.1a1 1 0 0 1 .78.37l.72.9h5.9a1 1 0 0 1 1 1v6.23a1 1 0 0 1-1 1H2.75a1 1 0 0 1-1-1V4.25Z',
      stroke: 'currentColor', strokeWidth: 1.2, strokeLinejoin: 'round',
    }))

    const CSS = `
/* The footer action stacks directly above Settings, so it copies that row's
   geometry: 42px tall, the sidebar's own row padding with a transparent
   inlay border, and the medium radius. The variables come from the sidebar
   root, which every occupant of this area inherits. */
.dsh-ghq-layer { position: relative; flex: none; display: flex; align-items: center; width: 100%; min-width: 0; margin: 4px 0; }
.dsh-ghq-layer[data-rail] { width: 36px; height: 36px; margin: 8px 0 10px; flex: none; }
.dsh-ghq-trigger {
  flex: 1; min-width: 0; display: flex; align-items: center;
  gap: var(--dsh-sidebar-label-gap, 8px); width: 100%; height: 42px;
  margin: 0; padding: 0 calc(var(--dsh-sidebar-row-padding, 12px) - var(--dsh-sidebar-paint-inset, 3px));
  box-sizing: border-box; border: none; border-radius: var(--dsw-radius-md);
  border-inline: var(--dsh-sidebar-paint-inset, 3px) solid transparent;
  background: transparent; background-clip: padding-box;
  color: var(--dsw-alias-label-primary);
  font-family: inherit; font-size: 14px; line-height: 22px; cursor: pointer; overflow: hidden;
}
@media (hover: hover) {
  .dsh-ghq-trigger:hover { background-color: var(--dsw-alias-interactive-bg-hover); }
}
.dsh-ghq-trigger:focus-visible { outline: none; box-shadow: 0 0 0 2px var(--dsw-focus-ring-color, var(--dsw-alias-brand-primary)); }
/* The rail control uses the same 36x36 box and 12px rounding as its neighbours. */
.dsh-ghq-trigger[data-rail] {
  flex: none; justify-content: center; gap: 0; width: 36px; height: 36px;
  margin: 0; padding: 0; border-inline: 0; background-clip: border-box;
}
.dsh-ghq-trigger svg { width: 16px; height: 16px; flex: none; }
.dsh-ghq-trigger[data-rail] svg { width: 18px; height: 18px; }
.dsh-ghq-trigger-label { min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }

.dsh-ghq-root { position: fixed; inset: 0; z-index: 1000; display: flex; align-items: center; justify-content: center; padding: max(24px, var(--dsh-frame-overlay-top, 24px)) 24px; }
.dsh-ghq-mask { position: absolute; inset: var(--dsh-frame-chrome-top, 0px) 0 0; border: none; padding: 0; background: var(--dsw-alias-bg-mask-1); backdrop-filter: var(--dsw-mask-blur); cursor: default; }
.dsh-ghq-card {
  position: relative; z-index: 1; display: flex; flex-direction: column;
  width: min(560px, 100%); max-height: min(560px, 100%);
  border-radius: var(--dsw-radius-panel); background: var(--dsw-alias-bg-layer-2);
  box-shadow: var(--dsw-elevation-prominent); overflow: hidden;
}
.dsh-ghq-head { display: flex; align-items: flex-start; gap: 12px; padding: 22px 16px 12px 24px; }
.dsh-ghq-head-text { flex: 1 1 auto; min-width: 0; }
.dsh-ghq-title { margin: 0; font-size: 16px; line-height: 24px; font-weight: 500; color: var(--dsw-alias-label-primary); }
.dsh-ghq-desc { margin: 2px 0 0; font-size: 12px; line-height: 18px; color: var(--dsw-alias-label-secondary); }
.dsh-ghq-close { flex: none; display: inline-flex; align-items: center; justify-content: center; width: 28px; height: 28px; padding: 0; border: none; border-radius: var(--dsw-radius-sm); background: transparent; color: var(--dsw-alias-label-secondary); cursor: pointer; }
.dsh-ghq-close:hover { background: var(--dsw-alias-interactive-bg-hover); }
.dsh-ghq-close:focus-visible { outline: none; box-shadow: 0 0 0 2px var(--dsw-focus-ring-color, var(--dsw-alias-brand-primary)); }

.dsh-ghq-search { margin: 0 24px 8px; display: flex; align-items: center; gap: 8px; height: 34px; padding: 0 10px; border: 0.5px solid var(--dsw-alias-border-l4); border-radius: var(--dsw-radius-md); background: var(--dsw-alias-bg-layer-1); }
.dsh-ghq-search:focus-within { border-color: var(--dsw-alias-state-business-primary); }
.dsh-ghq-search input { flex: 1 1 auto; min-width: 0; border: none; outline: none; background: transparent; color: var(--dsw-alias-label-primary); font: inherit; font-size: 14px; line-height: 22px; }
.dsh-ghq-search input::placeholder { color: var(--dsw-alias-label-dimmed); }

.dsh-ghq-meta { padding: 0 24px 6px; font-size: 12px; line-height: 16px; color: var(--dsw-alias-label-tertiary); }
.dsh-ghq-list { flex: 1 1 auto; min-height: 0; overflow: auto; margin: 0; padding: 4px 12px 12px; list-style: none; }
.dsh-ghq-row {
  display: flex; align-items: center; gap: 8px; width: 100%;
  margin: 0; padding: 7px 12px; border: none; border-radius: var(--dsw-radius-md);
  background: transparent; color: var(--dsw-alias-label-primary);
  font: inherit; font-size: 13px; line-height: 18px; text-align: left; cursor: pointer;
}
.dsh-ghq-row[data-active] { background: var(--dsw-alias-interactive-bg-hover); }
.dsh-ghq-row:focus-visible { outline: none; box-shadow: 0 0 0 2px var(--dsw-focus-ring-color, var(--dsw-alias-brand-primary)); }
.dsh-ghq-row:disabled { opacity: 0.55; cursor: default; }
.dsh-ghq-row-name { flex: 1 1 auto; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.dsh-ghq-row-owner { color: var(--dsw-alias-label-tertiary); }
.dsh-ghq-tag { flex: none; max-width: 40%; padding: 1px 7px; border-radius: 999px; background: var(--dsw-alias-bg-layer-1); color: var(--dsw-alias-label-secondary); font-size: 11px; line-height: 16px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.dsh-ghq-row-arrow { flex: none; color: var(--dsw-alias-label-dimmed); font-size: 12px; }

.dsh-ghq-note { margin: 0; padding: 4px 24px 20px; font-size: 12px; line-height: 18px; color: var(--dsw-alias-label-secondary); }
.dsh-ghq-note[data-error] { color: var(--dsw-alias-state-error-primary); }
.dsh-ghq-retry { margin-left: 6px; border: none; padding: 0; background: transparent; color: var(--dsw-alias-brand-primary); font: inherit; text-decoration: underline; cursor: pointer; }
`

    /**
     * Ask the Host for the repository list.
     * @param ctx - Client plugin context carrying the Connection service.
     * @param signal - cancellation owned by the caller effect.
     * @returns the listing, or the failure to show.
     */
    async function fetchRepositories(ctx, signal) {
      const connection = ctx.get('connection')
      if (connection === undefined || connection.rpc === undefined) {
        return { ok: false, message: 'the connection service is unavailable' }
      }
      try {
        const result = await connection.rpc.call('/api', ENDPOINT, { args: {} }, signal)
        if (!result.ok) return { ok: false, message: `${result.error.code}: ${result.error.message}` }
        const value = result.value
        const repositories = Array.isArray(value?.repositories) ? value.repositories : []
        return {
          ok: true,
          root: typeof value?.root === 'string' ? value.root : null,
          repositories: repositories
            .filter(entry => typeof entry?.path === 'string' && typeof entry?.name === 'string')
            .map(entry => ({ name: entry.name, path: entry.path })),
        }
      } catch (error) {
        if (signal.aborted) return { ok: false, aborted: true }
        return { ok: false, message: error instanceof Error ? error.message : String(error) }
      }
    }

    /**
     * Split a ghq name into its owner prefix and repository name.
     * @param name - `github.com/owner/repo` or a trailing-segment fallback.
     * @returns the parts the row renders separately.
     */
    function splitName(name) {
      const segments = name.split('/')
      const repository = segments.length > 0 ? segments[segments.length - 1] : name
      const prefix = segments.slice(0, -1).join('/')
      return { prefix, repository }
    }

    /**
     * Register a repository as a Workspace and open a Session in it.
     * @param ctx - Client plugin context.
     * @param path - absolute repository path.
     * @returns the message key of a failure, or null on success.
     */
    async function openRepository(ctx, path) {
      const workspaces = ctx.get('workspaces')
      if (workspaces === undefined) return 'unavailable'
      const workspace = await workspaces.create({ path })
      const uiWorkspace = ctx.get('uiWorkspace')
      // Registration is durable even when this build cannot start a Session,
      // so the row now shows the registered tag either way.
      if (uiWorkspace === undefined) return 'no.session'
      uiWorkspace.startSession(workspace.workspaceId)
      return null
    }

    /**
     * The picker dialog.
     * @param props.ctx - Client plugin context.
     * @param props.t - plugin translator.
     * @param props.useWorkspaces - standard hook over the Workspace snapshot.
     * @param props.useRemoteAccess - standard hook over the peer's permissions.
     * @param props.onClose - close request.
     */
    function GhqDialog({ ctx, t, useWorkspaces, useRemoteAccess, onClose }) {
      const snapshot = useWorkspaces(state => state)
      const canCreate = useRemoteAccess('workspace/create')
      const canStart = useRemoteAccess('session/create')
      const [state, setState] = useState({ phase: 'loading', repositories: [], message: null })
      const [query, setQuery] = useState('')
      const [active, setActive] = useState(0)
      const [opening, setOpening] = useState(null)
      const [failure, setFailure] = useState(null)
      const inputRef = useRef(null)
      const listRef = useRef(null)
      const cardRef = useRef(null)
      /** True while an arrow key moved the highlight, so only that move scrolls. */
      const keyboardDriven = useRef(false)
      /** The one in-flight listing request, replaced and aborted on each load. */
      const requestRef = useRef(null)

      /**
       * Tab order of this dialog: its visible, enabled, tabbable controls, in
       * document order. The close-on-mask button is excluded — it is mouse
       * chrome and carries `tabindex="-1"`.
       * @returns the first and last stops the Tab key cycles between.
       */
      const trapItems = () => [...(cardRef.current?.querySelectorAll('button:not(:disabled), input:not(:disabled)') ?? [])]
        .filter(item => !item.closest('[inert], [hidden]') && item.tabIndex >= 0)

      /**
       * Fetch the listing, replacing any request already in flight so a slow
       * retry can never overwrite a newer answer. The dialog owns the request's
       * lifetime: closing it aborts whatever this started.
       */
      const load = useCallback(() => {
        requestRef.current?.abort()
        const control = new AbortController()
        requestRef.current = control
        setState(current => ({ ...current, phase: 'loading', message: null }))
        void fetchRepositories(ctx, control.signal).then((result) => {
          // A superseded or unmounted request must not publish anything.
          if (result.aborted === true || requestRef.current !== control) return
          if (!result.ok) {
            setState({ phase: 'failed', repositories: [], message: result.message })
            return
          }
          setState({ phase: 'ready', repositories: result.repositories, message: null })
        })
      }, [ctx])

      useEffect(() => {
        load()
        return () => {
          requestRef.current?.abort()
          requestRef.current = null
        }
      }, [load])

      useEffect(() => {
        // Return focus to whatever opened the dialog once it unmounts, so the
        // footer action keeps its keyboard position.
        const previous = document.activeElement
        inputRef.current?.focus()
        return () => {
          if (previous instanceof HTMLElement && previous.isConnected) {
            try { previous.focus({ preventScroll: true }) } catch { /* the opener left */ }
          }
        }
      }, [])

      const registered = useMemo(() => {
        const paths = new Set()
        for (const workspace of snapshot.items ?? []) paths.add(workspace.path)
        return paths
      }, [snapshot.items])

      const filtered = useMemo(() => {
        const needle = query.trim().toLowerCase()
        if (needle === '') return state.repositories
        const terms = needle.split(/\s+/u)
        return state.repositories.filter((entry) => {
          const haystack = entry.name.toLowerCase()
          return terms.every(term => haystack.includes(term))
        })
      }, [state.repositories, query])

      // A new filter names a new first candidate: keeping the old index would
      // make Enter open whichever row now happens to sit at that position.
      useEffect(() => { setActive(0) }, [query])

      useEffect(() => {
        setActive(current => (filtered.length === 0 ? 0 : Math.min(current, filtered.length - 1)))
      }, [filtered.length])

      const disabled = !canCreate || !canStart

      const choose = useCallback(async (entry) => {
        if (entry === undefined || disabled || opening !== null) return
        setOpening(entry.path)
        setFailure(null)
        try {
          const problem = await openRepository(ctx, entry.path)
          if (problem === null) {
            onClose()
            return
          }
          setFailure(problem === 'no.session'
            ? t('no.session')
            : problem === 'unavailable' ? t('no.workspaces') : t('open.failed'))
        } catch (error) {
          setFailure(`${t('open.failed')} ${error instanceof Error ? error.message : String(error)}`)
        } finally {
          setOpening(null)
        }
      }, [ctx, disabled, onClose, opening, t])

      useEffect(() => {
        const onKeyDown = (event) => {
          // A newer layer (a menu, a nested dialog) owns the keyboard.
          const layers = document.querySelectorAll('[role="dialog"][aria-modal="true"], [role="menu"]')
          if (layers[layers.length - 1] !== cardRef.current) return
          // An input-method composition owns Enter, the arrows, and Escape while
          // it is converting text; the dialog must not act on those keys.
          if (event.isComposing || event.keyCode === 229) return
          if (event.key === 'Escape' && !event.shiftKey) {
            if (event.defaultPrevented) return
            event.preventDefault()
            onClose()
            return
          }
          if (event.key === 'Tab') {
            // Portaled menus own their traversal while they contain focus.
            const current = document.activeElement
            if (current !== null && current !== undefined && current.closest?.('[role="menu"]')) return
            const items = trapItems()
            if (items.length === 0) return
            const first = items[0]
            const last = items[items.length - 1]
            const atEdge = event.shiftKey ? current === first : current === last
            if (atEdge || current === null || current === undefined || !cardRef.current?.contains(current)) {
              event.preventDefault()
              ;(event.shiftKey ? last : first).focus()
            }
            return
          }
          if (filtered.length === 0) return
          if (event.key === 'ArrowDown') {
            event.preventDefault()
            keyboardDriven.current = true
            setActive(current => (current + 1) % filtered.length)
            return
          }
          if (event.key === 'ArrowUp') {
            event.preventDefault()
            keyboardDriven.current = true
            setActive(current => (current - 1 + filtered.length) % filtered.length)
            return
          }
          if (event.key === 'Enter' && document.activeElement === inputRef.current) {
            event.preventDefault()
            void choose(filtered[Math.min(active, filtered.length - 1)])
          }
        }
        document.addEventListener('keydown', onKeyDown, true)
        return () => { document.removeEventListener('keydown', onKeyDown, true) }
      }, [active, choose, filtered, onClose])

      useEffect(() => {
        // Hover follows the pointer, so it must not also scroll the list: the
        // scrolled row would slide under the pointer and re-trigger hover.
        if (!keyboardDriven.current) return
        keyboardDriven.current = false
        const row = listRef.current?.querySelector('[data-active]')
        if (row !== undefined && row !== null && typeof row.scrollIntoView === 'function') {
          row.scrollIntoView({ block: 'nearest' })
        }
      }, [active])

      const status = state.phase
      /**
       * Stable id of one row, so the search input can name the highlighted
       * option with `aria-activedescendant` instead of moving focus.
       * @param index - row position in the filtered list.
       * @returns the element id.
       */
      const optionId = index => `${DIALOG_ID}-option-${index}`
      const rows = filtered.map((entry, index) => {
        const { prefix, repository } = splitName(entry.name)
        const isRegistered = registered.has(entry.path)
        return h('li', { key: entry.path, role: 'presentation' }, h('button', {
          type: 'button',
          id: optionId(index),
          role: 'option',
          className: 'dsh-ghq-row',
          'aria-selected': index === active,
          'data-active': index === active ? '' : undefined,
          disabled: disabled || opening !== null,
          title: entry.path,
          tabIndex: -1,
          onMouseEnter: () => { setActive(index) },
          onClick: () => { void choose(entry) },
        }, h('span', { className: 'dsh-ghq-row-name' },
          prefix === '' ? null : h('span', { className: 'dsh-ghq-row-owner' }, `${prefix}/`),
          repository),
        isRegistered ? h('span', { className: 'dsh-ghq-tag' }, t('registered')) : null,
        h('span', { className: 'dsh-ghq-row-arrow', 'aria-hidden': true }, '↵')))
      })

      // Assistive technology hears how many candidates the filter left, and
      // that the count changed, without reading the whole list. English needs
      // its own singular form; every dictionary carries `count.one` so no
      // language has to special-case a count of one.
      const countText = status !== 'ready'
        ? ''
        : filtered.length === 1 ? t('count.one') : t('count', { count: filtered.length })

      return ReactDOM.createPortal(h('div', { className: 'dsh-ghq-root', role: 'presentation' },
        // The mask is mouse chrome: Escape is the keyboard dismissal, so it
        // stays out of the Tab order and out of the accessibility tree.
        h('div', { className: 'dsh-ghq-mask', 'aria-hidden': true, onClick: onClose }),
        h('div', {
          className: 'dsh-ghq-card',
          ref: cardRef,
          role: 'dialog',
          'aria-modal': true,
          'aria-label': t('dialog.title'),
          'aria-busy': opening === null ? undefined : true,
          tabIndex: -1,
        },
        h('div', { className: 'dsh-ghq-head' },
          h('div', { className: 'dsh-ghq-head-text' },
            h('h2', { className: 'dsh-ghq-title' }, t('dialog.title')),
            h('p', { className: 'dsh-ghq-desc' }, t('dialog.description'))),
          h('button', {
            type: 'button', className: 'dsh-ghq-close', 'aria-label': t('close'), onClick: onClose,
          }, '✕')),
        h('div', { className: 'dsh-ghq-search' },
          h('input', {
            ref: inputRef,
            type: 'search',
            value: query,
            placeholder: t('search.placeholder'),
            'aria-label': t('search.label'),
            role: 'combobox',
            'aria-expanded': status === 'ready' && filtered.length > 0,
            'aria-controls': status === 'ready' && filtered.length > 0 ? `${DIALOG_ID}-list` : undefined,
            'aria-activedescendant': status === 'ready' && filtered.length > 0 ? optionId(active) : undefined,
            'aria-autocomplete': 'list',
            autoComplete: 'off',
            spellCheck: false,
            onChange: (event) => { setQuery(event.currentTarget.value) },
          })),
        // One live line carries the count, the opening state, and a refusal,
        // so assistive technology learns about each without a re-read.
        h('div', { className: 'dsh-ghq-meta', role: 'status', 'aria-live': 'polite' },
          disabled
            ? t('no.permission')
            : opening !== null
              ? t('opening')
              : countText === '' ? '\u00a0' : countText),
        status === 'loading' ? h('p', { className: 'dsh-ghq-note', role: 'status' }, t('loading')) : null,
        status === 'failed'
          ? h('p', { className: 'dsh-ghq-note', 'data-error': '', role: 'alert' },
            `${t('failed')} ${state.message ?? ''}`,
            h('button', {
              type: 'button', className: 'dsh-ghq-retry', onClick: load,
            }, t('retry')))
          : null,
        status === 'ready' && state.repositories.length === 0
          ? h('p', { className: 'dsh-ghq-note' }, t('empty.none'))
          : null,
        status === 'ready' && state.repositories.length > 0 && filtered.length === 0
          ? h('p', { className: 'dsh-ghq-note' }, t('empty'))
          : null,
        status === 'ready' && filtered.length > 0
          ? h('ul', {
            id: `${DIALOG_ID}-list`,
            className: 'dsh-ghq-list',
            ref: listRef,
            role: 'listbox',
            'aria-label': t('search.label'),
          }, rows)
          : null,
        failure === null ? null : h('p', { className: 'dsh-ghq-note', 'data-error': '', role: 'alert' }, failure))),
      document.body)
    }

    /**
     * Build the sidebar-foot action component for one plugin context: the slot
     * supplies only owner props, hooks, and the translator, so the Connection
     * and Workspace services arrive through this closure.
     * @param ctx - Client plugin context.
     * @returns the component the footer slot renders.
     */
    function createGhqFooterAction(ctx) {
      /**
       * A labeled button, or a rail icon; opening it raises the picker dialog.
       * @param props.t - plugin translator.
       * @param props.wide - whether the sidebar renders wide content.
       * @param props.useWorkspaces - standard Workspaces hook.
       * @param props.useRemoteAccess - standard permission hook.
       */
      return function GhqFooterAction({ t, wide, useWorkspaces, useRemoteAccess }) {
        const [open, setOpen] = useState(false)
        const label = t('action')
        return h('div', { className: 'dsh-ghq-layer', 'data-rail': wide ? undefined : '' },
          h('button', {
            type: 'button',
            className: 'dsh-ghq-trigger',
            'data-rail': wide ? undefined : '',
            'aria-label': label,
            'aria-haspopup': 'dialog',
            'aria-expanded': open,
            title: label,
            onClick: () => { setOpen(value => !value) },
          },
          ICON,
          wide ? h('span', { className: 'dsh-ghq-trigger-label' }, label) : null),
          open
            ? h(GhqDialog, {
              ctx,
              t,
              useWorkspaces,
              useRemoteAccess,
              onClose: () => { setOpen(false) },
            })
            : null)
      }
    }

    return {
      inject: ['slots', 'locale', 'connection'],
      apply(ctx) {
        ctx.effect(() => ctx.locale.register(NS, DICTS), 'ghq: dictionaries')
        ctx.effect(() => {
          const style = document.createElement('style')
          style.dataset.plugin = 'dsh-ghq'
          style.textContent = CSS
          document.head.append(style)
          return () => { style.remove() }
        }, 'ghq: styles')
        ctx.slots.inject(FOOTER_SLOT, () => ctx.slots.register({
          name: FOOTER_SLOT,
          id: 'ghq',
          order: 1,
          locale: NS,
        }, createGhqFooterAction(ctx)))
      },
    }
  },
})
