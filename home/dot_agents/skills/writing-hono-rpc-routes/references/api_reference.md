# Hono RPC troubleshooting

## Symptom: `hc<AppType>` only sees `client.api.$get()`

Typical cause: the root app or a mounted child router was built with imperative mutations instead of a single chain.

Check all of these:

1. The root app exported as `AppType` is chain-built.
2. Every router mounted by `.route(...)` is also chain-built.
3. The root app uses relative paths correctly under `.basePath('/api')`.

## Symptom: runtime works but TypeScript loses nested properties

This usually means the route exists at runtime but the type-level route tree was degraded.

Refactor from:

```ts
const app = new Hono()
app.use('*', middleware)
app.get('/me', handler)
```

To:

```ts
const app = new Hono()
  .use('*', middleware)
  .get('/me', handler)
```

## Symptom: mounted child routes vanish after adding middleware

Move the middleware into the chain on the child router itself:

```ts
const child = new Hono()
  .use('*', middleware)
  .get('/item', handler)
```

Avoid mutating `child` after declaration if it will be mounted into an RPC-exported parent.

## Checklist after refactor

1. Run type-checking.
2. Confirm the expected path exists on the client, such as `client.api.admin.exhibitors`.
3. Re-run build/tests so the runtime path still matches the inferred path.
