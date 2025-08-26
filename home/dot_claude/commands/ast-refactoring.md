使い捨ての script を生成してプロジェクトの TypeScript を AST 操作してリファクタリングする。

## script 方針

- カレントディレクトリ に `tsconfig.json` がある前提
- Deno, ts-morph を使う
- `./tmp-ast/(slug)/main.ts` に script ファイルを作成する
- `deno run --no-lock -R -W ./tmp-ast/(slug)/main.ts` で実行
- 実行後、script ファイルは削除せずに残す

## script 雛形

```ts
import { Project } from 'jsr:@ts-morph/ts-morph';

const project = new Project({
  tsConfigFilePath: `./tsconfig.json`,
});

const sourceFiles = project.getSourceFiles();

sourceFiles.forEach((sourceFile, index) => {
  const filePath = sourceFile.getFilePath();
  console.log(`Processing file ${index + 1}: ${filePath}`);
});
```
