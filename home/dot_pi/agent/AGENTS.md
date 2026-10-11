# AGENTS.md (User Root)

## Communication and Language

- User communication: Japanese (日本語)

## Process Guidelines

### SubAgent Delegation

- Use SubAgents to stay focused on the essential task. The Agent’s (your) context is a finite resource, so it is important to balance delegation and direct execution effectively.
- Delegate to SubAgents:
  - Yak-shaving work that is necessary to complete the task but falls outside the core request.
  - Work that benefits from an independent perspective, such as review or advice.
- Keep with the Agent; do not delegate:
  - The core substance of the request. Delegating work beyond ancillary tasks leaks context that the Agent must retain, so it is equally important not to delegate critical work.
  - Management and coordination of the overall task.
- In short, delegate ancillary work appropriately while remaining focused on the main line of work. You are responsible for producing an output that satisfies the requested scope.
- Anti-patterns:
  - Performing ancillary work yourself rather than delegating it.
  - Delegating core work for reasons outside these criteria, such as because it seems easy.
  - Stopping work while the task remains incomplete.

## CLI Tools

### pueue: Long-running Tasks and Development Servers

- Do not start long-running processes such as development servers, watchers, or daemons directly from the CLI; use **`pueue`** instead.
- Start them with `pueue add -- <command>`, and use `pueue status` / `pueue log` / `pueue follow` / `pueue kill` / `pueue remove` to check status or manage them.

### Timeout Discipline for Long-running Commands

- Whenever you run a command that may take a while (including `pueue wait` / `pueue follow`), estimate how long it should take and **always pass an explicit `timeout` (in seconds) to the bash tool with margin**. Example: a lint that normally finishes in ~1 minute → `timeout: 90`.
- For very long tasks, do not settle into a long wait up front: wait ~1 minute, check progress (`pueue status` / `pueue log`), confirm the task is actually advancing, and only then start a longer wait with an explicit timeout.
- Never wait 5+ minutes without an intermediate status check.
