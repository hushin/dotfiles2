---
name: dsh-customize
description: >-
  Edit DeepSeek Harness (DSH) customizations the chezmoi way: local plugins under
  ~/.dsh/plugins, the web profile manifest, and DSH skills/rules. Use whenever
  asked to change, add, or debug a DSH plugin (dsh-*), a cordis.patch.yml in
  ~/.dsh, or the DSH profile composition. Also triggers on 「DSH をカスタマイズ」
  「DSH のプラグインを直して」「profile を変えて」.
---

# DSH のカスタマイズ手順

## 原則

- DSH のカスタマイズは **chezmoi 管理下に置く**。デプロイ先 (`~/.dsh/...`) と
  chezmoi の source が食い違ったまま放置しない。
- ただし **試行錯誤はデプロイ先でやってよい**。動いたものを `chezmoi add` で
  取り込むのが速い。source を先に書くことを強制しない。
- どちらの向きで進めても、最後は `chezmoi diff` が空（= source と target が一致）に
  なっている状態で終える。
- **コミットは動作確認の後**。「良さそう」を確認できたら、コミット内容を
  ユーザーに提案してから実行する。確認前にコミットしない。

| 種類 | デプロイ先 | chezmoi source（`~/.local/share/chezmoi/` 配下） |
|---|---|---|
| プラグイン本体 | `~/.dsh/plugins/<name>/` | `home/private_dot_dsh/plugins/<name>/` |
| web プロファイル manifest | `~/.dsh/profiles/web/package.json` | `home/private_dot_dsh/profiles/web/private_package.json` |
| DSH スキル | `~/.dsh/skills/<name>/SKILL.md` | `home/private_dot_dsh/skills/<name>/private_SKILL.md` |
| グローバルルール | `~/.dsh/AGENTS.md` | `home/private_dot_dsh/private_AGENTS.md` |

## 進め方 A: デプロイ先で直して取り込む（試行錯誤向き）

1. `~/.dsh/...` を直接編集する
2. `dsh web` を再起動して動作確認する
3. 良ければ取り込む。管理済みなら `chezmoi re-add <target>`、未管理なら `chezmoi add <target>`
4. `chezmoi diff <target>` が空になったことを確認する
5. 変更内容をユーザーに示してコミットを提案する

## 進め方 B: source を直して反映する（差分が読みやすい）

1. source を編集する（`chezmoi edit <target>` でもよい）
2. `chezmoi apply <target>` で**必要なファイルだけ**反映する（全体 apply は
   他の未コミット差分を巻き込む）
3. `dsh web` を再起動して動作確認する
4. `chezmoi diff <target>` が空であることを確認する
5. コミットを提案する

クライアント側（`client.js`）を変えた場合、ビルドスクリプトを持つプラグインでは
再ビルドも必要。ホスト側 `index.js` と profile の変更は再起動しないと効かない。

## 新しいプラグインを足すとき

1. `home/private_dot_dsh/plugins/<name>/` を作る（`index.js`, `client.js`,
   `cordis.patch.yml`, `package.json`, `locale/`, `icon.svg`）
2. `package.json` に `dsh.bundle.patch`（と web なら `dsh.client`）を書く
3. `chezmoi apply ~/.dsh/plugins/<name>`
4. `~/.dsh/profiles/web/package.json` の `bundles` と `dependencies` に追加する。
   link は `link:~/.dsh/plugins/<name>` 形式にする
5. `cd ~/.dsh/profiles/web && pnpm install --prefer-offline`
6. `dsh web` を再起動して確認する

## 注意

- **ホームの絶対パスをコミットしない。** `/home/<user>/...` ではなく
  `link:~/.dsh/plugins/...` か相対パスを使う
- `plugin_manager`（UI）は `package.json` を書き換える。UI で bundle を増減したら
  `chezmoi re-add ~/.dsh/profiles/web/package.json` で取り込み直す
- `node_modules/`、`pnpm-lock.yaml`、`~/.dsh/{sessions,storages,logs,cache}` は管理しない
- `~/.dsh/profiles/web/cordis.patch.yml`（モデル・権限プリセット）は現状未管理。
  触った場合は何を変えたかを報告する
- `chezmoi apply` で管理下の `package.json` を戻すと UI の変更が消える
- push はユーザーに確認する（force push は特に）

## 検証

```sh
chezmoi diff ~/.dsh                      # 空なら source と target が一致
chezmoi status | grep -v '^ R'           # 想定外の差分が無いか
ls -l ~/.dsh/profiles/web/node_modules   # symlink の向き先
node --input-type=module -e "console.log(import.meta.resolve('dsh-reply-hint'))"
```

`dsh web` の再起動後、プラグインのロード失敗がログに出ていないことも確認する。
