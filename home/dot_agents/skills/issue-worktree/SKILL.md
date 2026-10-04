---
name: issue-worktree
description: Execute a development task defined by a GitHub issue end-to-end, in any repository. Read the issue with `gh`, create a `git-wt` worktree on a Conventional-Commits-style branch, research the codebase (detecting the project's own conventions and quality-check commands rather than assuming any), form a concrete implementation plan, and confirm it with the user before writing any code. Use this skill whenever the user references a GitHub issue to work on — by number ("issue #36", "#36 をやって", "36 番お願い") or by full issue URL (https://github.com/.../issues/36) — and wants it implemented. Trigger even when the user just pastes an issue link with a short instruction like "これお願い" or "この issue やって", as long as a real GitHub issue is referenced. Do NOT use for general feature requests or bug fixes that aren't tied to a tracked GitHub issue.
---

# Issue-Driven Worktree Workflow

GitHub issue を起点にして、worktree を切り、issue を読み、実装方針を立て、ユーザーに確認してから実装し、commit するまでを担う汎用スキル。特定のプロジェクトを前提にせず、リポジトリごとの規約（品質チェックコマンド、デフォルトブランチ、コーディング規約）はその場で検出する。

worktree は `git-wt`（k1LoW/git-wt）で管理する。issue 単位の作業は毎回専用の worktree で行うことで、メインの作業ツリーを汚さず、複数の issue を並行できる。

## 全体フロー

1. issue 参照を解決する（番号 or URL）
2. `git-wt` が入っているか確認する（無ければ導入を促す）
3. issue を `gh` で読む（タイトル・本文・コメント・ラベル）
4. ブランチ名を決める（Conventional Commits の prefix + issue 番号 + slug）
5. worktree を切って移動する
6. リポジトリの規約を検出し、コードベースを調査して実装方針を立てる
7. 情報不足ならユーザーに質問する
8. **方針をユーザーに提示し、確認が取れるまで実装に入らない**（ここが一番重要なゲート）
9. 確認後に実装する
10. 品質チェックを通してから Conventional Commits で commit する
11. push / PR 作成はユーザーに尋ねてから行う

## Step 1: issue 参照を解決する

ユーザーの入力から issue を取り出す。両方の形式を受け付ける。

- 番号のみ（`#36`, `36`, `36 番`, `issue 36`）
- URL（`https://github.com/<owner>/<repo>/issues/36`）

URL の場合は owner/repo と番号をパースする。番号のみの場合は現在のリポジトリ（`gh repo view --json nameWithOwner`）を使う。

```bash
# URL から owner/repo と番号を抽出する正規表現の目安
# github\.com/([^/]+)/([^/]+)/issues/(\d+)
```

## Step 2: git-wt が入っているか確認する

`git-wt` はこのスキルの前提ツール。最初に確認する。

```bash
command -v git-wt || git wt -h >/dev/null 2>&1
```

入っていなければ、実装に入る前にユーザーに導入を促す。Go 製の git-wt をこのように案内する:

```
git-wt が見つかりません。導入してください:
  go install github.com/k1LoW/git-wt/cmd/git-wt@latest
または https://github.com/k1LoW/git-wt の手順に従ってインストールしてください。
インストール後に再度お願いします。
```

入っていれば次へ。

## Step 3: issue を読む

`gh` で issue を取得する。人間可読で全体を把握し、必要なら構造化も使う。

```bash
# 人間可読（コメント含む）。これが主軸。
gh issue view 36 --comments
# URL 指定のときは --repo を使う
gh issue view 36 --repo <owner>/<repo> --comments

# 必要なら構造化（labels/comments を機械的に扱いたいとき）
gh issue view 36 --json number,title,body,labels,state,comments --repo <owner>/<repo>
```

読むべき対象: タイトル・本文・すべてのコメント・ラベル。コメントは要件の補足や議論の経緯を含むことが多く、無視しない。

## Step 4: ブランチ名を決める

Conventional Commits の prefix をブランチ名にも使う。issue の性質（ラベル・タイトル・本文）から判断する:

- バグ修正 → `fix/`
- 新機能・拡張 → `feature/`
- リファクタ・chore・ドキュメント → `chore/` / `docs/` / `refactor/`（実態に合わせる）

形式: `<prefix>/<issue番号>-<slug>`

- `fix/36-staff-permission-edit`
- `feature/42-notification-on-approve`

slug は issue タイトルから短く ASCII で切る（kebab-case）。タイトルが日本語のみのときは、内容を表す短い英語 slug を自分で作る。プロジェクト名やモジュール名のような接頭辞は slug に入れず、本質だけを残す。長くしすぎない（3〜5 語程度）。

判断に迷う（バグか機能か曖昧、など）ときは、方針提示のタイミングでブランチ名案も含めてユーザーに確認すればよい。実装前に直せる。

## Step 5: worktree を切って移動する

最新を取り、リポジトリのデフォルトブランチを起点に worktree を作る。デフォルトブランチは `main`/`master`/`staging` などプロジェクトごとに異なるので固定で決め打ちしない。

```bash
git fetch origin --prune
# デフォルトブランチを検出する（`git default-branch` alias を使う）
default_branch=$(git default-branch)
git wt feature/36-staff-permission-edit "origin/${default_branch}"
```

`git default-branch` は事前に登録済みの git alias 前提。無い環境では `gh repo view --json defaultBranchRef -q .defaultBranchRef.name` や `git remote show origin | sed -n '/HEAD branch/s/.*: //p'` で代替する。

`git wt` は worktree を `.wt/<branch>` 以下に作り、作成後にそのパスを出力する。このパスがこれからの作業ディレクトリになる。以降のファイル編集・コマンドはすべてその worktree パス配下で行う（read/edit/write には絶対パスを使う、bash は `cd <worktree-path> && ...` で包む）。

> 注意: 「初期化されたディレクトリ配下のみ編集する」という原則に対し、**この worktree だけは例外として作業場所にする**。それがスキルの目的だから。worktree 以外の既存ブランチの作業ツリーには触れない。

作成後に確認:

```bash
cd <worktree-path> && git branch --show-current && git log -1 --oneline
```

## Step 6: リポジトリの規約を検出し、実装方針を立てる

worktree に入ったら、まずこのリポジトリ固有の作法を把握する。プロジェクトごとに異なるので、決め打ちせず毎回検出する。

**コーディング規約・アーキテクチャ**: `CLAUDE.md` / `AGENTS.md` / `.cursorrules` / `CONTRIBUTING.md` / `README.md` があれば読み、レイヤー構造・ドメイン用語・命名規則・禁止事項を把握する。これらのファイルに「詳細は `docs/xxx.md` を見よ」のような progressive disclosure の指示があれば、タスクに関連する範囲だけ辿って読む。

**品質チェックコマンド**（Step 10 で使う）: 以下の優先順位で探す。

1. `CLAUDE.md` / `AGENTS.md` に「commit 前に実行」「品質チェック」等の指示があればそれを最優先で使う（プロジェクトが独自の理由でラップしていることがある — 例: 特定のツール経由でないと lint が正しく動かない、など）
2. `package.json` の `scripts` に `lint` / `typecheck` / `test` があれば、そのリポジトリのパッケージマネージャ（`pnpm-lock.yaml` → pnpm、`yarn.lock` → yarn、`package-lock.json` → npm、無ければ package.json の `packageManager` フィールド）経由で実行する
3. `Makefile` に `lint` / `test` ターゲットがあれば `make lint` / `make test`
4. Python なら `pyproject.toml` の `[tool.ruff]` / `[tool.pytest.ini_options]` や `tox.ini` を見て `ruff check` / `pytest` 等
5. それでも判断できなければ、実装前にユーザーに「品質チェックはどのコマンドで行いますか」と聞く

**変更範囲の特定**: 影響範囲が広いときは Explore agent（または同等の調査用 subagent）に調査を委譲する。自分で全ファイルを総当たりで grep しない。変更するファイル・追加するファイル・触るレイヤー/モジュールを明確にする。

## Step 7: 情報不足ならユーザーに質問する

issue だけでは作業に着手できないときは、実装に入る前にユーザーに聞く。典型的な「情報不足」:

- 再現手順・入力条件・期待挙動が書かれていない（バグ報告）
- 要件が曖昧・複数の解釈ができる（例: 「権限を設定できる」が、誰が・どの権限・どこで・どう保存するか不明）
- 受け入れ基準（どうなれば完了か）が不明
- 技術的制約・対象画面・対象 API が不明
- コメント内で要件が更新されているのに本文と矛盾している

質問は具体的に、自分の解釈案を添えて出す（「A と B のどちらですか。私見では A ですが」）。質問が尽きるまで待つ。

## Step 8: 方針を提示して確認を取る（ここで止まる）

実装方針をまとめてユーザーに見せ、**確認が取れるまで実装に着手しない**。これがこのスキルで最も重要なゲート。飛ばすと手戻りが大きくなる。

提示する内容:

- issue の要約（自分の言葉で1〜3行）
- ブランチ名案（`<prefix>/<n>-<slug>`）
- 実装方針: 変更対象ファイル/レイヤー・追加/修正の要点・順序
- 懸念・リスク・フォローアップ項目（テスト方針・DB migration の有無・既存機能への影響など）
- 「この方針で進めてよいですか？修正点があれば教えてください」

ユーザーが OK するか修正を指示してくるまで待つ。修正指示があれば方針を更新して再度確認。

## Step 9: 実装する（確認後のみ）

方針が承認されたら実装に入る。Step 6 で把握したこのリポジトリの規約（レイヤー構造・関心事の分離・命名規則など）に従う。

## Step 10: commit する

コミットは必ず行う。Conventional Commits に従う。

```
<type>(<scope>): <要約>

<必要なら本文>

refs #<n>
```

- type はブランチの prefix と整合させる（fix/ → fix、feature/ → feat、など）
- コミットメッセージの末尾に必ず `refs #<n>` を入れる。これにより GitHub 上でコミットと issue が自動紐付く
- scope は変更対象のモジュール/ディレクトリ名
- 要約は命令形・英語が一般的だが、本文は日本語可（そのプロジェクトのコミットログの慣習に合わせる。`git log --oneline -20` で確認するとよい）
- 1論点1コミットを心がける。方針で示したまとまりで区切る

コミット前に、Step 6 で検出した品質チェックコマンドを実行し、通ることを確認する。

## Step 11: push / PR 作成はユーザーに尋ねる

commit まで終わったら、ユーザーに尋ねる:

> commit まで完了しました。push して PR を作成しますか？

`yes` なら:

```bash
git fetch origin
git rebase origin/$(git default-branch)
git push -u origin <branch>
```

rebase でコンフリクトした場合はユーザーに報告し、解決方法を確認する。

PR 作成（`gh pr create`）の body には表題と、マージで issue を自動クローズするため **`Closes #<n>`** を含める。これが「マージしたら issue が閉じる」仕掛け。PR 本文の雛形:

```markdown
## 目的

<issue の要約・なぜやるか>

## 変更内容

- ...
- ...

## 関連 issue

Closes #<n>
```

PR の base はリポジトリのデフォルトブランチ（Step 5 で検出した `${default_branch}`）。

ユーザーが push/PR を望まない場合は、worktree に commit が置かれたまま終わる。勝手に push しない。

## 補足: 失敗・やり直し

- worktree の作成先がすでに存在する場合: `git wt` が教えてくれる。別 slug にするか、既存 worktree を `git wt -d <branch>` で削ってから作り直す（削る前にユーザーに一言）。
- ブランチ prefix を間違えた: 実装前なら `git wt -d` で消して作り直す。コミット後なら `git branch -m <new>` を worktree 内で。
