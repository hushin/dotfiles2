#!/usr/bin/env python3
"""Validate a zk note before it is committed.

Usage:
  python3 check_note.py ~/ghq/github.com/hushin/notes/zk/20261112193000-foo.org

Checks the things that silently break org-roam or the note's readability:
filename, property drawer, UUID uniqueness, title placement, link targets,
src block balance, heading jumps, and empty sections. Errors exit 1; warnings
are advisory.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

from zk import (
    ID_RE,
    LINK_RE,
    NOTES_DIR,
    TITLE_RE,
    UUID_RE,
    first_heading_pos,
    iter_notes,
    repo_root,
    slugify,
)

FILENAME_RE = re.compile(r"^(\d{14})-(.+)\.org$")
BEGIN_RE = re.compile(r"^#\+begin_(\S+)", re.IGNORECASE | re.MULTILINE)
END_RE = re.compile(r"^#\+end_(\S+)", re.IGNORECASE | re.MULTILINE)
HEADING_RE = re.compile(r"^(\*+) (.+)$", re.MULTILINE)
FILETAGS_RE = re.compile(r"^#\+filetags:\s*(\S+)\s*$", re.MULTILINE)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("note", help="path to the .org file")
    parser.add_argument("--repo", help="notes repository root (default: ZK_NOTES_ROOT or ~/ghq/.../notes)")
    parser.add_argument("--json", action="store_true")
    args = parser.parse_args()

    path = Path(args.note).expanduser().resolve()
    root = repo_root(args.repo)

    errors: list[str] = []
    warnings: list[str] = []

    if not path.is_file():
        errors.append(f"file not found: {path}")
        return report(errors, warnings, args.json)

    text = path.read_text(encoding="utf-8", errors="replace")
    rel = path.relative_to(root).as_posix() if path.is_relative_to(root) else str(path)

    # 1. Location and filename.
    if path.parent != (root / NOTES_DIR):
        warnings.append(f"note is not in {NOTES_DIR}/: {rel}")
    match = FILENAME_RE.match(path.name)
    if not match:
        errors.append("filename must look like YYYYMMDDHHMMSS-slug.org")
    else:
        try:
            from datetime import datetime

            datetime.strptime(match.group(1), "%Y%m%d%H%M%S")
        except ValueError:
            errors.append(f"filename timestamp is not a real datetime: {match.group(1)}")

    # 2. Header: property drawer at the top, then title.
    head = text.lstrip("\ufeff")
    if not head.startswith(":PROPERTIES:"):
        errors.append("file must start with the :PROPERTIES: drawer")
    id_match = ID_RE.search(text)
    if not id_match:
        errors.append("missing :ID: in the property drawer")
        note_id = ""
    else:
        note_id = id_match.group(1)
        if not UUID_RE.match(note_id):
            errors.append(f":ID: is not a UUID: {note_id}")

    title_match = TITLE_RE.search(text, 0, first_heading_pos(text))
    if not title_match or not title_match.group(1).strip():
        errors.append("missing #+title: before the first heading")
        title = ""
    else:
        title = title_match.group(1).strip()

    filetags = FILETAGS_RE.search(text)
    if not filetags:
        warnings.append("no #+filetags: line (capture default is :draft:)")
    elif not re.match(r"^:[^:\s]+(?::[^:\s]+)*:$", filetags.group(1)):
        errors.append(f"malformed #+filetags: {filetags.group(1)}")

    # 3. IDs and links against the rest of the repo.
    all_notes = list(iter_notes(root, include_skipped=True))
    id_counts: dict[str, list[str]] = {}
    title_slugs: dict[str, str] = {}
    for other in all_notes:
        if other["id"]:
            id_counts.setdefault(other["id"], []).append(other["rel"])
        if other["title"]:
            title_slugs.setdefault(slugify(other["title"]), other["rel"])

    if note_id and len(id_counts.get(note_id, [])) > 1:
        errors.append(f"duplicate :ID: {note_id}: {', '.join(id_counts[note_id])}")

    if title:
        other = title_slugs.get(slugify(title))
        if other and other != rel:
            warnings.append(f"same title as {other}; consider updating that note instead")

    all_ids = set(id_counts)
    seen_links = set()
    for link in LINK_RE.finditer(text):
        target = link.group(1)
        if target in seen_links:
            continue
        seen_links.add(target)
        if target not in all_ids:
            errors.append(f"broken id link: {target}")

    # 4. Src blocks.
    begins: dict[str, int] = {}
    ends: dict[str, int] = {}
    for m in BEGIN_RE.finditer(text):
        begins[m.group(1).lower()] = begins.get(m.group(1).lower(), 0) + 1
    for m in END_RE.finditer(text):
        ends[m.group(1).lower()] = ends.get(m.group(1).lower(), 0) + 1
    for kind in sorted(set(begins) | set(ends)):
        if begins.get(kind, 0) != ends.get(kind, 0):
            warnings.append(
                f"unbalanced #+begin_{kind}/#+end_{kind} ({begins.get(kind, 0)} vs {ends.get(kind, 0)})"
            )

    # 5. Heading structure.
    headings = list(HEADING_RE.finditer(text))
    prev_level = 0
    for heading in headings:
        level = len(heading.group(1))
        if prev_level and level > prev_level + 1:
            warnings.append(f"heading level jumps from {prev_level} to {level}: {heading.group(2)}")
        prev_level = level

    for index, heading in enumerate(headings):
        start = heading.end()
        end = headings[index + 1].start() if index + 1 < len(headings) else len(text)
        next_level = len(headings[index + 1].group(1)) if index + 1 < len(headings) else 0
        # Only childless sections can be judged empty here.
        if next_level and next_level > len(heading.group(1)):
            continue
        body = text[start:end].strip()
        if not body:
            warnings.append(f"empty section: {heading.group(2)}")

    if warnings and not errors:
        pass
    return report(errors, warnings, args.json, rel)


def report(errors: list[str], warnings: list[str], as_json: bool, rel: str = "") -> int:
    if as_json:
        print(
            json.dumps(
                {"ok": not errors, "file": rel, "errors": errors, "warnings": warnings},
                ensure_ascii=False,
            )
        )
    else:
        if errors:
            print("errors:")
            for item in errors:
                print(f"  - {item}")
        if warnings:
            print("warnings:")
            for item in warnings:
                print(f"  - {item}")
        if not errors and not warnings:
            print(f"OK {rel}")
        elif not errors:
            print(f"OK {rel} (with warnings)")
    return 1 if errors else 0


if __name__ == "__main__":
    raise SystemExit(main())
