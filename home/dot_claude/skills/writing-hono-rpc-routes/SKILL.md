---
name: writing-hono-rpc-routes
description: Preserves Hono RPC type inference when building or refactoring Hono plus hc code. Use when the hc client loses nested routes, when converting Hono apps to method chains, or when editing root and child route files that are mounted with .route(...).
---

# Writing Hono RPC Routes

Keep all exported Hono apps in method-chain form when they participate in RPC typing.

The critical rule: if a parent app exports `type AppType = typeof app` and mounts child routers with `.route(...)`, each mounted child router must also be defined as a method chain. Mixing `const app = new Hono(); app.use(...); app.get(...); export default app` can degrade `hc<AppType>` inference so nested paths disappear.

## Workflow

1. Identify the exported RPC root app.
2. Ensure the root app is a single chain ending in `.route(...)` calls.
3. Convert every mounted child router under `.route(...)` to the same pattern.
4. Re-run type-checking and confirm nested client paths exist again.

## Root app pattern

Prefer this shape for the app whose type is consumed by `hc<AppType>`:

```ts
const app = new Hono<AppEnv>()
  .basePath('/api')
  .on(['POST', 'GET'], '/auth/*', handler)
  .get('/', rootHandler)
  .route('/invite', inviteRoute)
  .route('/submissions', submissionsRoute)
  .route('/admin', adminRoute)

export default app
export type AppType = typeof app
```

Do not split the exported route-bearing app across imperative statements unless you confirm `hc<AppType>` still infers the full tree.

## Child route pattern

Mounted child routers must also be method chains:

```ts
const submissionsRoute = new Hono<RouteEnv>()
  .use('*', authed, role('exhibitor', 'admin'))
  .get('/me', getHandler)
  .put('/me', putHandler)

export default submissionsRoute
```

Avoid this pattern for routers that are mounted with `.route(...)`:

```ts
const submissionsRoute = new Hono<RouteEnv>()
submissionsRoute.use('*', authed, role('exhibitor', 'admin'))
submissionsRoute.get('/me', getHandler)
submissionsRoute.put('/me', putHandler)
```

That form may still work at runtime but can break RPC route inference.

## Practical rules

- If you use `.basePath('/api')`, define the API root as `.get('/')`, not `.get('/api')`.
- Export `typeof app` from the fully chained root app.
- Keep path prefixes on the parent; keep child routers relative (`'/me'`, `'/exhibitors'`, `'/'`).
- After refactoring, prefer `hc<AppType>('/')` and access nested routes through the inferred client tree.
- If nested properties like `client.api.submissions` disappear, inspect every router in the mount chain, not just the root app.

## Troubleshooting

See [references/api_reference.md](references/api_reference.md).
