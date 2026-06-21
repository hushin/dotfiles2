# AGENTS.md (User Scoped)

## Communication and Language

- User communication: Japanese (日本語)
- Documentation and code comments: Preserve the existing language; do not translate them.

## Skills Guidelines

- AGENTS.md assumes progressive disclosure: it contains only the minimum information needed, while task-specific knowledge and guidelines live elsewhere.
- Select and load the necessary skills as needed for each task.

## Coding Style

- Maintain separation of concerns.
- Separate state from logic.
- Prioritize readability and maintainability.
- Follow t-wada-style TDD: implement while continuously verifying behavior with type checking and tests.
- Define contract layers (APIs/types) rigorously using ADTs, and keep implementation layers regenerable.
- Rules that can be checked statically should be expressed with the environment’s linter or ast-grep, not in prompts.

## Responsibility Boundaries and Autonomy

- The user defines the goal; the Agent owns the process and execution path to achieve it. This boundary is non-negotiable.
- When achieving the goal proves difficult and the goal itself needs to change, ask the user for a decision.
- When the goal is clear, do not ask the user about the process step by step. Plan the best path to achieve the goal while maintaining high code quality, and proceed autonomously without seeking approval.
- **Process Guidelines compliance**: The process is delegated to the Agent, but the "Process Guidelines" below encode proven practices that consistently produce effective output. Maximize adherence to these guidelines — treat them as the default playbook. Within that compliance envelope, choose whatever approach works best. The guidelines are a means to high-quality results, not a constraint.

## Process Guidelines

### Progressive Disclosure

- AGENTS.md assumes progressive disclosure: it contains only the minimum information needed, while task-specific knowledge and guidelines live elsewhere.
- Select and load the necessary skills as needed for each task.
- Use `scout` subagents: Thoroughly understanding the codebase before making changes is critical.

### SubAgent Delegation

- Actively delegate yak shaving and work outside the main scope to subagents.
- Your responsibility is to achieve the goal with the best cost-performance while maintaining high quality. To this end, it is critical to delegate non-essential work to subagents (to conserve context — in other words, to maintain focus). By delegating decomposable subtasks to subagents, you can concentrate on the main scope, while subagents also focus on smaller-scoped tasks, improving output quality compared to executing directly.
  - Good example: When asked to implement something, delegate design, review, or behavior verification to other agents.
  - Bad example: When encountering a deep-rooted error, trying to solve it yourself without launching a debugging agent.
  - Bad example: Running scout yourself because it is easy (the decision criterion is context management, so ease of execution is not a relevant factor)

## Web Fetch

- Use Jina AI from Bash to fetch article/page content with `r.jina.ai`:
  ```bash
  curl -H "Authorization: Bearer ${JINA_API_KEY}" \
  -fsSL 'https://r.jina.ai/<target-url>'
  ```

## Research external/web

- For direct web search, use Jina AI from Bash with `s.jina.ai`:
  ```bash
  curl -H "X-Respond-With: no-content" \
   -H "Authorization: Bearer ${JINA_API_KEY}" \
   -fsSL 'https://s.jina.ai/<search-query>'
  ```

## Long-running Tasks and Development Servers

- Do not start long-running processes such as development servers, watchers, or daemons directly from the CLI; use **`pueue`** instead.
- Start them with `pueue add -- <command>`, and use `pueue status` / `pueue log` / `pueue follow` / `pueue kill` / `pueue remove` to check status or manage them.
- For parallel agent delegation, queue tasks via pueue:
  ```bash
  pueue add -i --print-task-id -- "pi ... -p '<instruction>' < /dev/null"
  ```
  ```bash
  pueue status
  pueue wait <task-id> # blocks when there is no other parallel work
  pueue log <task-id> # check results/status
  ```

## Documentation Lookup

Always use `ctx7` for library/framework/API docs (even familiar ones — training data may be stale). Prefer over web search. Not for: refactoring, debugging business logic, code review.

```bash
ctx7 library <official-name> "<question>"  # resolve → pick best /org/project
ctx7 docs <libraryId> "<question>"          # fetch & answer
```

Max 3 commands. Never silently fall back to training data on quota error — suggest `ctx7 login` or `CONTEXT7_API_KEY`. See `/skill:find-docs` for full details.
