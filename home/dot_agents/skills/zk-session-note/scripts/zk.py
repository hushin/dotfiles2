#!/usr/bin/env python3
"""Shared helpers for the zk-session-note skill.

Notes live in ~/ghq/github.com/hushin/notes/zk. ZK_NOTES_ROOT overrides the
repository root for dry runs and evals; the default must stay the real repo.
"""

from __future__ import annotations

import os
import re
import unicodedata
from pathlib import Path
from typing import Iterable

DEFAULT_REPO = Path.home() / "ghq" / "github.com" / "hushin" / "notes"
NOTES_DIR = "zk"

# Not zettelkasten notes: dailies, logs, archives, tasks, attachments.
SKIP_DIRS = {
    ".git",
    "archives",
    "daily",
    "journal",
    "lifelog",
    "gtd",
    "images",
    "meta",
}

TITLE_RE = re.compile(r"^#\+title:\s*(.+?)\s*$", re.MULTILINE)
ID_RE = re.compile(r"^:ID:\s*(\S+)\s*$", re.MULTILINE)
LINK_RE = re.compile(r"\[\[id:([^\]\s]+)\](?:\[[^\]]*\])?\]")
UUID_RE = re.compile(
    r"^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-"
    r"[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"
)


def repo_root(explicit: str | None = None) -> Path:
    """Resolve the notes repository root.

    ZK_NOTES_ROOT exists so tests can point the scripts at a copy of the repo.
    Refuse a directory without zk/ so a typo cannot create notes in the wrong
    place.
    """
    raw = explicit or os.environ.get("ZK_NOTES_ROOT") or str(DEFAULT_REPO)
    root = Path(raw).expanduser().resolve()
    if not (root / NOTES_DIR).is_dir():
        raise SystemExit(f"error: {root} is not the notes repo (missing {NOTES_DIR}/)")
    return root


def slugify(title: str) -> str:
    """Mirror org-roam-node-slug for the characters that appear in titles."""
    s = unicodedata.normalize("NFD", title)
    # org-roam strips only U+0300..U+0331 combining marks. Stripping every
    # combining mark would turn プ into フ and break Japanese filenames.
    s = "".join(ch for ch in s if not (0x300 <= ord(ch) <= 0x331))
    s = unicodedata.normalize("NFC", s)
    s = "".join(ch if ch.isalnum() else "_" for ch in s)
    return re.sub(r"_+", "_", s).strip("_").lower()


def first_heading_pos(text: str) -> int:
    match = re.search(r"^\*+ ", text, re.MULTILINE)
    return match.start() if match else len(text)


def parse_note(path: Path) -> dict:
    text = path.read_text(encoding="utf-8", errors="replace")
    title_match = TITLE_RE.search(text, 0, first_heading_pos(text))
    id_match = ID_RE.search(text)
    return {
        "path": path,
        "text": text,
        "title": title_match.group(1) if title_match else "",
        "id": id_match.group(1) if id_match else "",
    }


def iter_notes(root: Path, include_skipped: bool = False) -> Iterable[dict]:
    for path in sorted(root.rglob("*.org")):
        rel = path.relative_to(root)
        if not include_skipped and any(part in SKIP_DIRS for part in rel.parts[:-1]):
            continue
        note = parse_note(path)
        note["rel"] = rel.as_posix()
        yield note
