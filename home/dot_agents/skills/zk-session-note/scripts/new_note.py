#!/usr/bin/env python3
"""Create a zk note with the header org-roam expects.

Usage:
  python3 new_note.py --title "Cloudflare D1 のバックアップ" --body /tmp/body.org
  python3 new_note.py --title "..." --body - --dry-run

The body file contains everything from the first heading onward. This script
owns the parts that are easy to get wrong by hand: the timestamped filename,
the org-roam slug, a unique UUID, the property drawer, and filetags.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
import uuid
from datetime import datetime, timedelta
from pathlib import Path

from zk import NOTES_DIR, iter_notes, repo_root, slugify


def parse_tags(raw: str) -> list[str]:
    tags = [t.strip().strip(":") for t in re.split(r"[,:\s]+", raw) if t.strip().strip(":")]
    for tag in tags:
        if re.search(r"[\s:]", tag):
            raise SystemExit(f"error: invalid tag {tag!r}")
    return list(dict.fromkeys(tags))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--title", required=True, help="note title (#+title), no newlines")
    parser.add_argument("--body", required=True, help="file with the body, or - for stdin")
    parser.add_argument("--repo", help="notes repository root (default: ZK_NOTES_ROOT or ~/ghq/.../notes)")
    parser.add_argument("--tags", default="draft", help="comma/colon separated tags; default: draft")
    parser.add_argument("--slug", help="override the generated slug (rarely needed)")
    parser.add_argument("--timestamp", help="YYYYMMDDHHMMSS override for tests")
    parser.add_argument("--id", help="UUID override for tests")
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--json", action="store_true")
    args = parser.parse_args()

    root = repo_root(args.repo)
    notes_dir = root / NOTES_DIR

    title = " ".join(args.title.split())
    if not title:
        raise SystemExit("error: title is empty")
    if "\n" in title or "\r" in title:
        raise SystemExit("error: title must be one line")

    slug = args.slug or slugify(title)
    if not slug:
        raise SystemExit("error: title produced an empty slug; pass --slug")

    if args.body == "-":
        body = sys.stdin.read()
    else:
        body_path = Path(args.body).expanduser()
        if not body_path.is_file():
            raise SystemExit(f"error: body file not found: {body_path}")
        body = body_path.read_text(encoding="utf-8")

    body = body.lstrip("\n").rstrip() + "\n"
    if not body.strip():
        raise SystemExit("error: body is empty")
    if re.search(r"^#\+title:", body, re.MULTILINE):
        raise SystemExit("error: body must not contain a #+title: line")
    if body.startswith(":PROPERTIES:"):
        raise SystemExit("error: body must not contain the file-level property drawer")

    # Collect IDs across the whole repo, including archives: an ID collision
    # breaks org-roam links even if the other file is excluded from the DB.
    existing_ids = {note["id"] for note in iter_notes(root, include_skipped=True) if note["id"]}

    if args.id:
        if not re.match(r"^[0-9a-fA-F-]{36}$", args.id):
            raise SystemExit("error: --id must be a UUID")
        note_id = args.id
        if note_id in existing_ids:
            raise SystemExit(f"error: ID already exists: {note_id}")
    else:
        note_id = str(uuid.uuid4())
        for _ in range(100):
            if note_id not in existing_ids:
                break
            note_id = str(uuid.uuid4())
        else:
            raise SystemExit("error: could not generate a unique UUID")

    if args.timestamp:
        if not re.match(r"^\d{14}$", args.timestamp):
            raise SystemExit("error: --timestamp must be YYYYMMDDHHMMSS")
        base = datetime.strptime(args.timestamp, "%Y%m%d%H%M%S")
        fixed = True
    else:
        base = datetime.now()
        fixed = False

    path = None
    stamp = base
    for attempt in range(120):
        candidate = notes_dir / f"{stamp:%Y%m%d%H%M%S}-{slug}.org"
        if not candidate.exists():
            path = candidate
            break
        if fixed:
            raise SystemExit(f"error: file already exists: {candidate}")
        stamp = base + timedelta(seconds=attempt + 1)
    if path is None:
        raise SystemExit("error: could not find a free filename")

    tags = parse_tags(args.tags)
    header = [":PROPERTIES:", f":ID:       {note_id}", ":END:", f"#+title: {title}"]
    if tags:
        header.append("#+filetags: " + ":" + ":".join(tags) + ":")
    content = "\n".join(header) + "\n\n" + body

    if args.dry_run:
        print(f"would create: {path}")
        print(f"id: {note_id}")
        print(f"title: {title}")
        print("---")
        print("\n".join(header))
        return 0

    try:
        with path.open("x", encoding="utf-8") as handle:
            handle.write(content)
    except FileExistsError:
        raise SystemExit(f"error: file already exists: {path}")

    if args.json:
        print(
            json.dumps(
                {"path": str(path), "id": note_id, "title": title, "tags": tags},
                ensure_ascii=False,
            )
        )
    else:
        print(f"created: {path}")
        print(f"id: {note_id}")
        print(f"title: {title}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
