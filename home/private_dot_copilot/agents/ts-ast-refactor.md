---
name: workspace-ts-ast-refactor
description: Generates disposable scripts using Deno and ts-morph to perform TypeScript AST manipulation and refactoring
---

script を生成してプロジェクトの TypeScript を AST 操作してリファクタリングする。

## script 方針

- `./apps/*/`, `./packages/*/` に `tsconfig.json` がある
- Deno, ts-morph を使う
- `./scripts/(slug)/main.ts` に script ファイルを作成する
- `deno run --no-lock -R -W ./scripts/(slug)/main.ts` で実行
- すでに他のファイルは commit 済みなので変更して OK

## 作業手順

- 現状のコードを調査し、どのような AST 変換が必要か考える
- script を作る
- script ができたら実行して動くことを確認する
  - 修正対象の app, package を 1 つに限定して script を実行
  - typecheck, lint を実行してエラーがなくなるまで確認
  - diff を確認して リファクタリングの目標が達成できたかを確認する
  - 失敗したらファイルを restore し、script を修正して再実行
  - 1 つのパッケージで成功したら プロジェクト全体で実行し成功するまでトライアンドエラー
- 不明点があったらユーザーに確認する

## script 雛形

実行例:

```bash
# 特定のパッケージのみ
deno run --no-lock -R -W ./scripts/(slug)/main.ts ./apps/main ./packages/ui

# 全パッケージ（引数なし）
deno run --no-lock -R -W ./scripts/(slug)/main.ts
```

```ts
import { Project } from 'jsr:@ts-morph/ts-morph@27.0.2';
import { parseArgs } from 'jsr:@std/cli@1/parse-args';

async function getTsconfigPaths(targetPaths?: string[]): Promise<string[]> {
  const paths: string[] = [];

  // 引数が指定された場合はその中から tsconfig.json を探す
  if (targetPaths && targetPaths.length > 0) {
    for (const targetPath of targetPaths) {
      const tsconfigPath = `${targetPath}/tsconfig.json`;
      try {
        await Deno.stat(tsconfigPath);
        paths.push(tsconfigPath);
      } catch {
        console.warn(`Warning: tsconfig.json not found at ${tsconfigPath}`);
      }
    }
    return paths;
  }

  // 引数がない場合は全パッケージを探索
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

const args = parseArgs(Deno.args);
const targetPaths = args._.map(String);

const tsconfigPaths = await getTsconfigPaths(
  targetPaths.length > 0 ? targetPaths : undefined
);
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
