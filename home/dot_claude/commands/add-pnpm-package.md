# pnpm Package Addition Workflow

## Basic Process

1. Check the latest installation method for the library
   - Use `/gemini-search` to reference the latest Installation instructions from official documentation
2. Add dependencies
   - Use `pnpm add <package-name>` to add packages
   - For dev dependencies, use `pnpm add -D <package-name>`
3. Regarding package.json editing
   - Do not directly edit package.json
   - Use `npm pkg set` command when necessary

## Scripts Configuration Examples

```sh
# Add to the scripts section of package.json
npm pkg set scripts.prepare="husky install"
npm pkg set scripts.build="vite build"
npm pkg set scripts.dev="vite dev"
```

## Important Notes

- Always check the latest documentation before installation
- Avoid manual editing of package.json, use CLI commands
- Properly classify between development and production dependencies
