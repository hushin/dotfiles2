---
name: workspace-ts-ast-refactor
description: Generates disposable scripts using Deno and ts-morph to perform TypeScript AST manipulation and refactoring
tools: ['read', 'edit', 'search', 'create_file', 'run_in_terminal']
---

script を生成してプロジェクトの TypeScript を AST 操作してリファクタリングする。

## script 方針

- `./apps/*/`, `./packages/*/` に `tsconfig.json` がある
- Deno, ts-morph を使う
- `./scripts/(slug)/main.ts` に script ファイルを作成する
- `deno run --no-lock -R -W ./scripts/(slug)/main.ts` で実行
- script ができたら実行して動くことを確認する（すでに他のファイルは commit 済みなので変更して OK）
  - 修正対象の apps or packages を 1 つに限定して script を実行
  - typecheck, lint を実行してエラーがなくなるまで確認
  - 失敗したらファイルを restore し、script を修正して再実行
  - 1 つのパッケージで成功したら プロジェクト全体で実行し成功するまでトライアンドエラー
- 不明点があったらユーザーに確認する

## script 雛形

```ts
import { Project } from 'jsr:@ts-morph/ts-morph@27.0.2';

async function getTsconfigPaths(): Promise<string[]> {
  const paths: string[] = [];
  const baseDirectories = ['./apps', './packages'];

  for (const baseDir of baseDirectories) {
    try {
      for await (const entry of Deno.readDir(baseDir)) {
        if (entry.isDirectory) {
          const tsconfigPath = `${baseDir}/${entry.name}/tsconfig.json`;
          try {
            await Deno.stat(tsconfigPath);
            paths.push(tsconfigPath);
          } catch {
            // tsconfig.jsonが存在しない場合はスキップ
          }
        }
      }
    } catch (err) {
      console.warn(
        `Warning: Could not read directory ${baseDir}:`,
        err.message
      );
    }
  }

  return paths;
}

const tsconfigPaths = await getTsconfigPaths();
console.log(
  `Found ${tsconfigPaths.length} tsconfig.json files:`,
  tsconfigPaths
);
console.log('');

for (const tsconfigPath of tsconfigPaths) {
  console.log(`\nProcessing project: ${tsconfigPath}`);

  const project = new Project({
    tsConfigFilePath: tsconfigPath,
  });

  const sourceFiles = project.getSourceFiles();

  sourceFiles.forEach((sourceFile, index) => {
    const filePath = sourceFile.getFilePath();
    console.log(`Processing file ${index + 1}: ${filePath}`);
  });
}
```
