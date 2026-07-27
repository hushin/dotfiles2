#!/usr/bin/env python3
"""git diff を hunk 単位の JSON に変換する。

レビュー担当の LLM が diff の各行を手で書き写すと、量に比例してトークンを食う上に
写し間違いが起きる。差分の「本文」は機械的に取れるので、ここで一度だけ構造化して
おき、LLM には hunk の一覧（id とファイル名）だけを見せて、グループ分けと解説の
執筆に集中してもらう。

使い方:
    python collect_diff.py -o hunks.json                 # 既定: HEAD との差分
    python collect_diff.py -o hunks.json -- --cached     # ステージ済みのみ
    python collect_diff.py -o hunks.json -- main...HEAD  # ブランチ差分

stdout には hunk の索引を出力する。これをそのまま読んで hunk id を割り当てる。
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys

HUNK_HEADER = re.compile(r"^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@(.*)$")


def run_git_diff(args: list[str], context: int) -> str:
    # mnemonicPrefix 等のローカル設定に影響されないよう prefix を固定する
    cmd = ["git", "diff", f"-U{context}", "--no-color", "--no-ext-diff",
           "--src-prefix=a/", "--dst-prefix=b/", *args]
    proc = subprocess.run(cmd, capture_output=True, text=True)
    if proc.returncode != 0:
        sys.stderr.write(proc.stderr)
        raise SystemExit(f"git diff が失敗しました: {' '.join(cmd)}")
    return proc.stdout


def parse_diff(text: str) -> tuple[list[dict], list[dict]]:
    files: list[dict] = []
    hunks: list[dict] = []
    cur_file: dict | None = None
    cur_hunk: dict | None = None
    old_no = new_no = 0

    def close_hunk() -> None:
        nonlocal cur_hunk
        if cur_hunk is not None:
            hunks.append(cur_hunk)
            cur_hunk = None

    for line in text.splitlines():
        if line.startswith("diff --git "):
            close_hunk()
            cur_file = {
                "path": None,
                "old_path": None,
                "new_path": None,
                "status": "modified",
                "binary": False,
                "additions": 0,
                "deletions": 0,
                "hunks": [],
            }
            files.append(cur_file)
            continue

        if cur_file is None:
            continue

        if line.startswith("new file mode"):
            cur_file["status"] = "added"
        elif line.startswith("deleted file mode"):
            cur_file["status"] = "deleted"
        elif line.startswith("rename from "):
            cur_file["status"] = "renamed"
            cur_file["old_path"] = line[len("rename from "):]
        elif line.startswith("rename to "):
            cur_file["status"] = "renamed"
            cur_file["new_path"] = line[len("rename to "):]
            cur_file["path"] = cur_file["new_path"]
        elif line.startswith("--- "):
            close_hunk()
            p = line[4:]
            cur_file["old_path"] = None if p == "/dev/null" else p[2:] if p.startswith(("a/", "b/")) else p
        elif line.startswith("+++ "):
            p = line[4:]
            cur_file["new_path"] = None if p == "/dev/null" else p[2:] if p.startswith(("a/", "b/")) else p
            cur_file["path"] = cur_file["new_path"] or cur_file["old_path"]
        elif line.startswith("Binary files "):
            cur_file["binary"] = True
        elif line.startswith("@@"):
            m = HUNK_HEADER.match(line)
            if not m:
                continue
            close_hunk()
            old_no = int(m.group(1))
            new_no = int(m.group(3))
            cur_hunk = {
                "id": None,
                "file": cur_file["path"],
                "status": cur_file["status"],
                "header": line[: line.find("@@", 2) + 2],
                "section": m.group(5).strip(),
                "old_start": old_no,
                "new_start": new_no,
                "additions": 0,
                "deletions": 0,
                "lines": [],
            }
        elif cur_hunk is not None:
            if line.startswith("\\"):  # \ No newline at end of file
                continue
            tag, body = (line[:1], line[1:]) if line else (" ", "")
            if tag == "+":
                cur_hunk["lines"].append({"t": "add", "o": None, "n": new_no, "text": body})
                cur_hunk["additions"] += 1
                cur_file["additions"] += 1
                new_no += 1
            elif tag == "-":
                cur_hunk["lines"].append({"t": "del", "o": old_no, "n": None, "text": body})
                cur_hunk["deletions"] += 1
                cur_file["deletions"] += 1
                old_no += 1
            elif tag == " ":
                cur_hunk["lines"].append({"t": "ctx", "o": old_no, "n": new_no, "text": body})
                old_no += 1
                new_no += 1

    close_hunk()

    width = max(3, len(str(len(hunks))))
    by_path: dict[str, dict] = {f["path"]: f for f in files if f["path"]}
    for i, h in enumerate(hunks, start=1):
        h["id"] = "h" + str(i).zfill(width)
        owner = by_path.get(h["file"])
        if owner is not None:
            owner["hunks"].append(h["id"])

    return files, hunks


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("-o", "--out", required=True, help="hunks.json の出力先")
    ap.add_argument("-U", "--context", type=int, default=3, help="diff の前後行数 (既定 3)")
    ap.add_argument("git_args", nargs="*", help="git diff にそのまま渡す引数 (既定: HEAD)")
    ns = ap.parse_args()

    git_args = ns.git_args or ["HEAD"]
    files, hunks = parse_diff(run_git_diff(git_args, ns.context))

    if not hunks:
        print("差分がありません。git diff の引数を確認してください。", file=sys.stderr)
        return 1

    # 未追跡ファイルは git diff に出ない。黙って落とすとレビューに穴が開く。
    untracked = subprocess.run(
        ["git", "ls-files", "--others", "--exclude-standard"],
        capture_output=True, text=True).stdout.split()
    if untracked:
        head = ", ".join(untracked[:10]) + (" …" if len(untracked) > 10 else "")
        print(f"警告: 未追跡ファイル {len(untracked)} 件は差分に含まれていません: {head}\n"
              f"      レビュー対象に含めるなら `git add -N .` を実行してから再実行してください。",
              file=sys.stderr)

    payload = {
        "diff_args": git_args,
        "stats": {
            "files": len(files),
            "hunks": len(hunks),
            "additions": sum(f["additions"] for f in files),
            "deletions": sum(f["deletions"] for f in files),
        },
        "files": files,
        "hunks": hunks,
    }
    with open(ns.out, "w", encoding="utf-8") as fp:
        json.dump(payload, fp, ensure_ascii=False, indent=1)

    s = payload["stats"]
    print(f"# {s['files']} files / {s['hunks']} hunks +{s['additions']} -{s['deletions']}  -> {ns.out}")
    print("# id / file / +add -del / section")
    for h in hunks:
        section = f"  {h['section']}" if h["section"] else ""
        print(f"{h['id']}\t{h['file']}\t+{h['additions']} -{h['deletions']}\t{h['header']}{section}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
