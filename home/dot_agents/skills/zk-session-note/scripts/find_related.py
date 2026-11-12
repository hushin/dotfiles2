#!/usr/bin/env python3
"""Find existing notes that a new zk note should link to or update.

Usage:
  python3 find_related.py --query "cloudflare d1 backup"
  python3 find_related.py --query "Hono RPC 型" --limit 5 --json

The output is a ready-to-paste org-roam link plus the file path and the terms
that matched. Matching is deliberately simple (substring, weighted by where it
appears); the agent still decides whether the note is the same topic.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

from zk import iter_notes, repo_root, slugify


def terms_from(query: str) -> list[str]:
    terms = [t.strip().lower() for t in re.split(r"[\s,、。]+", query)]
    # One-character Japanese terms match too much; keep short ASCII terms (D1).
    return [t for t in dict.fromkeys(terms) if len(t) >= 2 or t.isascii()]


def score_note(note: dict, terms: list[str], query_slug: str) -> tuple[int, list[str], list[str], list[str]]:
    title = note["title"].lower()
    rel = note["rel"].lower()
    body = note["text"].lower()
    note_slug = slugify(note["title"]) if note["title"] else ""

    score = 0
    in_title: list[str] = []
    in_path: list[str] = []
    in_body: list[str] = []

    for term in terms:
        if term in title:
            score += 12
            in_title.append(term)
        elif term in rel:
            score += 4
            in_path.append(term)
        elif term in body:
            score += 1
            in_body.append(term)

    # Reward titles that cover more of the query. An exact title is a
    # duplicate signal, not just a relation. Substring matches are too noisy
    # here: "Cloudflare" should not outrank "Cloudflare Backup" for
    # "cloudflare d1 backup" just because the shorter title is contained in
    # the query.
    if terms:
        score += int(10 * len(in_title) / len(terms))
    if query_slug and note_slug and query_slug == note_slug:
        score += 100

    return score, in_title, in_path, in_body


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--query", required=True, help="title or keywords, e.g. 'cloudflare backup'")
    parser.add_argument("--repo", help="notes repository root (default: ZK_NOTES_ROOT or ~/ghq/.../notes)")
    parser.add_argument("--limit", type=int, default=8)
    parser.add_argument("--json", action="store_true")
    parser.add_argument(
        "--include-skipped",
        action="store_true",
        help="also search daily/, archives/ and other non-note directories",
    )
    args = parser.parse_args()

    root = repo_root(args.repo)
    query = " ".join(args.query.split())
    terms = terms_from(query)
    if not terms:
        print("error: query has no usable terms", file=sys.stderr)
        return 2

    query_slug = slugify(query)
    results = []
    for note in iter_notes(root, include_skipped=args.include_skipped):
        if not note["title"]:
            continue
        score, in_title, in_path, in_body = score_note(note, terms, query_slug)
        if score:
            results.append(
                {
                    "score": score,
                    "id": note["id"],
                    "title": note["title"],
                    "path": note["rel"],
                    "matched": {"title": in_title, "path": in_path, "body": in_body},
                }
            )

    results.sort(key=lambda r: (-r["score"], r["path"]))
    results = results[: max(0, args.limit)]

    if args.json:
        print(json.dumps({"query": query, "results": results}, ensure_ascii=False, indent=2))
        return 0

    print(f"query: {query}")
    if not results:
        print("no matches. Create a new note.")
        return 0

    for item in results:
        matched = []
        if item["matched"]["title"]:
            matched.append("title: " + ", ".join(item["matched"]["title"]))
        if item["matched"]["path"]:
            matched.append("path: " + ", ".join(item["matched"]["path"]))
        if item["matched"]["body"]:
            matched.append("body: " + ", ".join(item["matched"]["body"]))
        if item["id"]:
            print(f"[[id:{item['id']}][{item['title']}]]")
        else:
            print(f"[no id] {item['title']}")
        print(f"  {item['path']}  (score {item['score']}; {'; '.join(matched)})")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
