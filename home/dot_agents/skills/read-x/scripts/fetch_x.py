#!/usr/bin/env python3
"""Fetch X (Twitter) posts, threads, conversations, articles, and profiles via FxEmbed API v2."""

from __future__ import annotations

import argparse
import json
import os
import re
import sys
import urllib.error
import urllib.parse
import urllib.request
from typing import Any

DEFAULT_API_BASE = "https://api.fxtwitter.com"
USER_AGENT = "read-x-skill/1.0 (+https://github.com/FxEmbed/FxEmbed)"

SUPPORTED_HOSTS = {
    "x.com",
    "www.x.com",
    "mobile.x.com",
    "twitter.com",
    "www.twitter.com",
    "mobile.twitter.com",
    "fxtwitter.com",
    "www.fxtwitter.com",
    "fixupx.com",
    "www.fixupx.com",
    "vxtwitter.com",
    "www.vxtwitter.com",
    "fixvx.com",
    "www.fixvx.com",
}

RESERVED_PATHS = {
    "home",
    "explore",
    "search",
    "notifications",
    "messages",
    "i",
    "settings",
    "compose",
    "intent",
    "hashtag",
    "login",
    "logout",
    "signup",
    "tos",
    "privacy",
}


class FetchError(Exception):
    """Raised when input parsing or API fetching fails."""


def parse_target(target: str) -> dict[str, str]:
    """Parse an X/Twitter URL, @handle, or status ID into a structured target."""
    raw = target.strip()
    if not raw:
        raise FetchError("Target cannot be empty.")

    if re.fullmatch(r"\d{5,25}", raw):
        return {"kind": "status", "id": raw}

    if raw.startswith("@"):
        handle = raw[1:].strip("/")
        if re.fullmatch(r"[A-Za-z0-9_]{1,20}", handle):
            return {"kind": "profile", "handle": handle}
        raise FetchError(f"Invalid X handle: {raw}")

    if not re.match(r"^https?://", raw, re.IGNORECASE):
        if any(raw.lower().startswith(host + "/") or raw.lower() == host for host in SUPPORTED_HOSTS):
            raw = "https://" + raw
        else:
            raise FetchError(
                f"Unsupported target: {target!r}. Pass an x.com/twitter.com URL, status ID, or @handle."
            )

    parsed = urllib.parse.urlparse(raw)
    host = (parsed.hostname or "").lower()
    if host not in SUPPORTED_HOSTS:
        raise FetchError(f"Unsupported host: {host!r}. Expected x.com / twitter.com / fxtwitter.com / fixupx.com.")

    parts = [p for p in parsed.path.split("/") if p]
    if not parts:
        raise FetchError(f"URL has no path: {target}")

    # Match /<user>/status/<id>, /<user>/statuses/<id>, /<user>/article/<id>, /i/status/<id>, /i/web/status/<id>
    for idx, seg in enumerate(parts):
        if seg.lower() in {"status", "statuses", "article"} and idx + 1 < len(parts):
            status_id = parts[idx + 1]
            if re.fullmatch(r"\d{5,25}", status_id):
                return {"kind": "status", "id": status_id}

    first = parts[0]
    first_lower = first.lower()
    if first_lower == "search":
        raise FetchError(
            "X search URLs are not supported: FxEmbed's /2/search endpoint is currently unavailable (HTTP 503)."
        )
    if first_lower in RESERVED_PATHS:
        raise FetchError(f"Unsupported X path '/{first}': expected a post or user profile URL.")

    handle = first.lstrip("@")
    if not re.fullmatch(r"[A-Za-z0-9_]{1,20}", handle):
        raise FetchError(f"Could not parse X handle or status ID from URL: {target}")

    sub = parts[1].lower() if len(parts) > 1 else ""
    result: dict[str, str] = {"kind": "profile", "handle": handle}
    if sub == "articles":
        result["sub"] = "articles"
    elif sub == "media":
        result["sub"] = "media"
    elif sub == "with_replies":
        result["sub"] = "with_replies"
    return result


def api_get(path: str, params: dict[str, Any] | None = None, timeout: int = 15) -> tuple[int, Any]:
    """Perform a GET request against FxEmbed API v2."""
    base = os.environ.get("FXEMBED_API_BASE", DEFAULT_API_BASE).rstrip("/")
    query = ""
    if params:
        filtered = {k: str(v) for k, v in params.items() if v is not None and v != ""}
        if filtered:
            query = "?" + urllib.parse.urlencode(filtered)
    url = f"{base}{path}{query}"

    req = urllib.request.Request(
        url,
        headers={
            "User-Agent": USER_AGENT,
            "Accept": "application/json",
        },
        method="GET",
    )
    try:
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            status = resp.status
            body = resp.read().decode("utf-8", errors="replace")
            if status == 204 or not body.strip():
                return status, None
            return status, json.loads(body)
    except urllib.error.HTTPError as exc:
        body = exc.read().decode("utf-8", errors="replace")
        detail = ""
        try:
            payload = json.loads(body)
            detail = payload.get("message") or payload.get("error") or ""
            if not detail and isinstance(payload.get("status"), dict):
                detail = payload["status"].get("message") or payload["status"].get("reason") or ""
        except Exception:
            detail = body.strip()[:200]
        msg = f"FxEmbed API returned HTTP {exc.code} for {url}"
        if detail:
            msg += f": {detail}"
        if exc.code == 404:
            msg += " (post/user not found or deleted)"
        elif exc.code == 401:
            msg += " (protected or private account)"
        elif exc.code == 429:
            msg += " (rate limited; wait and retry)"
        elif exc.code == 503:
            msg += " (endpoint temporarily unavailable upstream)"
        raise FetchError(msg) from exc
    except urllib.error.URLError as exc:
        raise FetchError(
            f"Network error contacting {url}: {exc.reason}. "
            "Check internet connectivity or ensure command execution has network access."
        ) from exc
    except json.JSONDecodeError as exc:
        raise FetchError(f"Invalid JSON returned from {url}: {exc}") from exc


def expand_text_urls(text: str, raw_text: dict[str, Any] | None) -> str:
    """Replace remaining shortened t.co URLs with expanded URLs from raw_text.facets."""
    if not text or not isinstance(raw_text, dict):
        return text
    facets = raw_text.get("facets") or []
    result = text
    for facet in facets:
        if not isinstance(facet, dict):
            continue
        if facet.get("type") == "url":
            orig = facet.get("original")
            repl = facet.get("replacement")
            if orig and repl and orig in result:
                result = result.replace(orig, repl)
    return result


def format_count(value: Any) -> str:
    if value is None:
        return "-"
    try:
        return f"{int(value):,}"
    except (TypeError, ValueError):
        return str(value)


def _extract_media_entity_url(entity: dict[str, Any]) -> str | None:
    info = entity.get("media_info")
    if not isinstance(info, dict):
        return None
    if info.get("original_img_url"):
        return str(info["original_img_url"])
    variants = info.get("variants")
    if isinstance(variants, list):
        mp4s = [v for v in variants if isinstance(v, dict) and v.get("content_type") == "video/mp4" and v.get("url")]
        if mp4s:
            mp4s.sort(key=lambda v: v.get("bit_rate") or 0, reverse=True)
            return str(mp4s[0]["url"])
    return None


def render_article(article: dict[str, Any]) -> str:
    """Render an X Article (Draft.js blocks + entityMap) as readable Markdown."""
    lines: list[str] = []
    title = article.get("title") or "(Untitled Article)"
    lines.append(f"### Article: {title}")

    meta_parts = []
    if article.get("created_at"):
        meta_parts.append(f"Created: {article['created_at']}")
    if article.get("modified_at") and article.get("modified_at") != article.get("created_at"):
        meta_parts.append(f"Modified: {article['modified_at']}")
    if meta_parts:
        lines.append(" | ".join(meta_parts))

    cover = article.get("cover_media")
    if isinstance(cover, dict):
        cover_url = _extract_media_entity_url(cover)
        if cover_url:
            lines.append(f"Cover: {cover_url}")

    media_by_id: dict[str, str] = {}
    for ent in article.get("media_entities") or []:
        if not isinstance(ent, dict):
            continue
        mid = str(ent.get("media_id") or ent.get("id") or "")
        url = _extract_media_entity_url(ent)
        if mid and url:
            media_by_id[mid] = url

    content = article.get("content") or {}
    blocks = content.get("blocks") or []
    raw_entity_map = content.get("entityMap") or []
    entity_map: dict[str, dict[str, Any]] = {}
    if isinstance(raw_entity_map, list):
        for item in raw_entity_map:
            if isinstance(item, dict) and "key" in item and isinstance(item.get("value"), dict):
                entity_map[str(item["key"])] = item["value"]
    elif isinstance(raw_entity_map, dict):
        for k, v in raw_entity_map.items():
            if isinstance(v, dict):
                entity_map[str(k)] = v

    if not blocks:
        preview = article.get("preview_text")
        if preview:
            lines.append("")
            lines.append(preview)
        return "\n".join(lines)

    lines.append("")
    in_code_block = False
    for block in blocks:
        if not isinstance(block, dict):
            continue
        btype = block.get("type", "unstyled")
        text = block.get("text", "")
        entity_ranges = block.get("entityRanges") or []

        if btype != "code-block" and in_code_block:
            lines.append("```")
            in_code_block = False

        if btype == "atomic":
            rendered_atomic = False
            for er in entity_ranges:
                ent = entity_map.get(str(er.get("key")))
                if not ent:
                    continue
                etype = ent.get("type", "")
                edata = ent.get("data") or {}
                if etype == "MARKDOWN" and edata.get("markdown"):
                    lines.append(str(edata["markdown"]).rstrip())
                    lines.append("")
                    rendered_atomic = True
                elif etype == "MEDIA":
                    for mi in edata.get("mediaItems") or []:
                        if isinstance(mi, dict):
                            mid = str(mi.get("mediaId") or "")
                            url = media_by_id.get(mid)
                            if url:
                                lines.append(f"[Media: {url}]")
                                rendered_atomic = True
                elif etype == "TWEET" and edata.get("tweetId"):
                    lines.append(f"[Embedded Post: https://x.com/i/status/{edata['tweetId']}]")
                    rendered_atomic = True
                elif etype == "DIVIDER":
                    lines.append("---")
                    lines.append("")
                    rendered_atomic = True
            if not rendered_atomic and text.strip():
                lines.append(text)
                lines.append("")
            continue

        # Collect inline links for this block
        inline_links: list[str] = []
        for er in entity_ranges:
            ent = entity_map.get(str(er.get("key")))
            if ent and ent.get("type") == "LINK":
                url = (ent.get("data") or {}).get("url")
                if url and url not in text and url not in inline_links:
                    inline_links.append(url)

        if btype == "header-one":
            lines.append(f"# {text}")
            lines.append("")
        elif btype == "header-two":
            lines.append(f"## {text}")
            lines.append("")
        elif btype == "header-three":
            lines.append(f"### {text}")
            lines.append("")
        elif btype == "unordered-list-item":
            lines.append(f"- {text}")
        elif btype == "ordered-list-item":
            lines.append(f"1. {text}")
        elif btype == "blockquote":
            lines.append(f"> {text}")
            lines.append("")
        elif btype == "code-block":
            if not in_code_block:
                lines.append("```")
                in_code_block = True
            lines.append(text)
        else:
            if text.strip():
                lines.append(text)
                lines.append("")

        for url in inline_links:
            lines.append(f"  Link: {url}")

    if in_code_block:
        lines.append("```")

    return "\n".join(lines).rstrip()


def render_media(media: dict[str, Any] | None) -> list[str]:
    if not isinstance(media, dict):
        return []
    lines: list[str] = []
    for idx, photo in enumerate(media.get("photos") or [], start=1):
        if not isinstance(photo, dict):
            continue
        url = photo.get("url", "")
        dims = ""
        if photo.get("width") and photo.get("height"):
            dims = f" ({photo['width']}x{photo['height']})"
        alt = photo.get("altText") or ""
        entry = f"- Photo {idx}{dims}: {url}"
        if alt:
            entry += f" | alt: {alt}"
        lines.append(entry)

    for idx, video in enumerate(media.get("videos") or [], start=1):
        if not isinstance(video, dict):
            continue
        vtype = "GIF" if video.get("type") == "gif" else "Video"
        url = video.get("url", "")
        dur = f" ({video['duration']}s)" if video.get("duration") else ""
        entry = f"- {vtype} {idx}{dur}: {url}"
        if video.get("thumbnail_url"):
            entry += f" (thumb: {video['thumbnail_url']})"
        lines.append(entry)

    ext = media.get("external")
    if isinstance(ext, dict) and ext.get("url"):
        lines.append(f"- External video: {ext['url']}")

    return lines


def render_status(
    status: dict[str, Any] | None,
    *,
    heading: str | None = None,
    depth: int = 0,
) -> str:
    """Render a single APITwitterStatus or APIStatusTombstone to Markdown."""
    if not status:
        return "[No status data]"

    if status.get("type") == "tombstone":
        reason = status.get("reason") or "unavailable"
        msg = status.get("message") or "Post is unavailable"
        return f"[Unavailable post ({reason}): {msg}]"

    lines: list[str] = []
    if heading:
        lines.append(heading)

    author = status.get("author") or {}
    name = author.get("name") or ""
    screen_name = author.get("screen_name") or "unknown"
    verified = ""
    ver = author.get("verification")
    if isinstance(ver, dict) and ver.get("verified"):
        vtype = ver.get("type")
        verified = f" [verified:{vtype}]" if vtype else " [verified]"

    header = f"**{name}** (@{screen_name}){verified}" if name else f"@{screen_name}{verified}"
    created = status.get("created_at") or ""
    url = status.get("url") or ""
    meta_bits = [bit for bit in [created, url] if bit]
    lines.append(header + (f" — {' | '.join(meta_bits)}" if meta_bits else ""))

    reposted_by = status.get("reposted_by")
    if isinstance(reposted_by, dict) and reposted_by.get("screen_name"):
        lines.append(f"_Reposted by @{reposted_by['screen_name']}_")

    replying_to = status.get("replying_to")
    if isinstance(replying_to, dict) and (replying_to.get("screen_name") or replying_to.get("post")):
        rt_user = replying_to.get("screen_name") or "?"
        rt_post = replying_to.get("post") or ""
        rt_info = f"@{rt_user}" + (f" (status {rt_post})" if rt_post else "")
        lines.append(f"_Replying to {rt_info}_")

    text = expand_text_urls(status.get("text") or "", status.get("raw_text"))
    if text.strip():
        lines.append("")
        lines.append(text.strip())

    translation = status.get("translation")
    if isinstance(translation, dict) and translation.get("text"):
        src = translation.get("source_lang") or "?"
        tgt = translation.get("target_lang") or "?"
        lines.append("")
        lines.append(f"**Translation ({src} -> {tgt}):**")
        lines.append(translation["text"].strip())

    article = status.get("article")
    if isinstance(article, dict):
        lines.append("")
        lines.append(render_article(article))

    media_lines = render_media(status.get("media"))
    if media_lines:
        lines.append("")
        lines.append("**Media:**")
        lines.extend(media_lines)

    card = status.get("card")
    if isinstance(card, dict) and card.get("url"):
        lines.append("")
        title = card.get("title") or card.get("domain") or card["url"]
        desc = f" — {card['description']}" if card.get("description") else ""
        lines.append(f"**Link Card:** [{title}]({card['url']}){desc}")

    poll = status.get("poll")
    if isinstance(poll, dict) and poll.get("choices"):
        lines.append("")
        lines.append(f"**Poll** (total votes: {format_count(poll.get('total_votes'))}):")
        for choice in poll["choices"]:
            if isinstance(choice, dict):
                lines.append(
                    f"- {choice.get('label', '')}: {choice.get('percentage', 0)}% ({format_count(choice.get('count'))})"
                )

    note = status.get("community_note")
    if isinstance(note, dict) and note.get("text"):
        lines.append("")
        lines.append(f"**Community Note:** {note['text'].strip()}")

    quote = status.get("quote")
    if isinstance(quote, dict) and depth < 2:
        lines.append("")
        lines.append("**Quoted Post:**")
        quoted_md = render_status(quote, depth=depth + 1)
        for qline in quoted_md.splitlines():
            lines.append(f"> {qline}" if qline else ">")

    metrics = [
        f"Likes: {format_count(status.get('likes'))}",
        f"Reposts: {format_count(status.get('reposts'))}",
        f"Replies: {format_count(status.get('replies'))}",
    ]
    if status.get("quotes") is not None:
        metrics.append(f"Quotes: {format_count(status.get('quotes'))}")
    if status.get("bookmarks") is not None:
        metrics.append(f"Bookmarks: {format_count(status.get('bookmarks'))}")
    if status.get("views") is not None:
        metrics.append(f"Views: {format_count(status.get('views'))}")
    if status.get("lang"):
        metrics.append(f"Lang: {status['lang']}")

    lines.append("")
    lines.append(f"_{' | '.join(metrics)}_")

    about = author.get("about_account")
    if isinstance(about, dict) and depth == 0:
        lines.append("")
        lines.append("**About Account:** " + json.dumps(about, ensure_ascii=False))

    return "\n".join(lines)


def render_profile(data: dict[str, Any]) -> str:
    user = data.get("user") or data
    if not isinstance(user, dict) or not user.get("screen_name"):
        return json.dumps(data, indent=2, ensure_ascii=False)

    lines: list[str] = []
    name = user.get("name") or ""
    screen_name = user["screen_name"]
    lines.append(f"# {name} (@{screen_name})" if name else f"# @{screen_name}")
    if user.get("url"):
        lines.append(user["url"])

    desc = expand_text_urls(user.get("description") or "", user.get("raw_description"))
    if desc.strip():
        lines.append("")
        lines.append(desc.strip())

    lines.append("")
    stats = [
        f"Followers: {format_count(user.get('followers'))}",
        f"Following: {format_count(user.get('following'))}",
        f"Posts: {format_count(user.get('statuses'))}",
        f"Likes: {format_count(user.get('likes'))}",
        f"Media: {format_count(user.get('media_count'))}",
    ]
    lines.append(" | ".join(stats))

    details: list[str] = []
    if user.get("location"):
        details.append(f"Location: {user['location']}")
    website = user.get("website")
    if isinstance(website, dict) and website.get("url"):
        details.append(f"Website: {website['url']}")
    if user.get("joined"):
        details.append(f"Joined: {user['joined']}")
    if user.get("protected"):
        details.append("Protected: true")
    ver = user.get("verification")
    if isinstance(ver, dict) and ver.get("verified"):
        details.append(f"Verified: {ver.get('type') or True}")
    if details:
        lines.append(" | ".join(details))

    if isinstance(user.get("about_account"), dict):
        lines.append("")
        lines.append("**About Account:** " + json.dumps(user["about_account"], ensure_ascii=False))

    return "\n".join(lines)


def render_thread_or_conversation(data: dict[str, Any], limit: int) -> str:
    sections: list[str] = []
    thread = data.get("thread") or []
    focal = data.get("status")
    focal_id = focal.get("id") if isinstance(focal, dict) else None

    if thread and len(thread) > 1:
        sections.append(f"# Thread ({len(thread)} posts)")
        for idx, item in enumerate(thread, start=1):
            marker = " (focal)" if isinstance(item, dict) and item.get("id") == focal_id else ""
            sections.append(render_status(item, heading=f"## [{idx}/{len(thread)}]{marker}"))
    else:
        sections.append(render_status(focal))

    replies = data.get("replies")
    if isinstance(replies, list):
        shown = replies[:limit]
        sections.append(f"## Replies (showing {len(shown)} of {len(replies)})")
        for idx, rep in enumerate(shown, start=1):
            sections.append(render_status(rep, heading=f"### Reply {idx}"))

    cursor = (data.get("cursor") or {}).get("bottom")
    if cursor:
        sections.append(f"Next cursor: `{cursor}`")

    return "\n\n---\n\n".join(sections)


def render_timeline(data: dict[str, Any], handle: str, kind: str, limit: int) -> str:
    results = data.get("results") or []
    shown = results[:limit]
    sections: list[str] = [f"# @{handle} — {kind} (showing {len(shown)} of {len(results)})"]
    for idx, item in enumerate(shown, start=1):
        if isinstance(item, dict) and item.get("type") == "thread" and isinstance(item.get("statuses"), list):
            parts = [render_status(st, heading=f"## [{idx}.{j}]") for j, st in enumerate(item["statuses"], start=1)]
            sections.append("\n\n".join(parts))
        else:
            sections.append(render_status(item, heading=f"## [{idx}]"))

    cursor = (data.get("cursor") or {}).get("bottom")
    if cursor:
        sections.append(f"Next cursor: `{cursor}`")
    return "\n\n---\n\n".join(sections)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Read X (Twitter) posts, threads, replies, articles, and profiles via FxEmbed API v2."
    )
    parser.add_argument("target", help="X/Twitter URL, numeric status ID, or @handle")
    parser.add_argument("--thread", action="store_true", help="Unroll conversation thread (/2/thread/{id})")
    parser.add_argument(
        "--replies",
        "--conversation",
        dest="replies",
        action="store_true",
        help="Fetch thread and replies (/2/conversation/{id})",
    )
    parser.add_argument(
        "--ranking",
        choices=["likes", "recency"],
        default="likes",
        help="Reply ranking mode for --replies (default: likes)",
    )
    parser.add_argument(
        "--statuses",
        "--timeline",
        dest="statuses",
        action="store_true",
        help="List user's recent posts (/2/profile/{handle}/statuses)",
    )
    parser.add_argument("--articles", action="store_true", help="List user's X Articles (/2/profile/{handle}/articles)")
    parser.add_argument("--media", action="store_true", help="List user's media posts (/2/profile/{handle}/media)")
    parser.add_argument("--with-replies", action="store_true", help="Include replies in --statuses")
    parser.add_argument("--about", action="store_true", help="Include about_account metadata")
    parser.add_argument("--lang", help="Translate post text to 2-letter ISO language code (e.g. ja, en)")
    parser.add_argument("--limit", "--count", dest="limit", type=int, default=20, help="Max items to show/request")
    parser.add_argument("--cursor", help="Pagination cursor (cursor.bottom from a previous call)")
    parser.add_argument("--json", dest="as_json", action="store_true", help="Output raw JSON from FxEmbed API")
    parser.add_argument("--timeout", type=int, default=15, help="HTTP timeout in seconds (default: 15)")
    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)

    try:
        target = parse_target(args.target)
        params: dict[str, Any] = {}
        if args.lang:
            params["lang"] = args.lang
        if args.about:
            params["about_account"] = "1"

        if target["kind"] == "status":
            status_id = target["id"]
            if args.replies:
                path = f"/2/conversation/{status_id}"
                params["ranking_mode"] = args.ranking
                if args.cursor:
                    params["cursor"] = args.cursor
            elif args.thread:
                path = f"/2/thread/{status_id}"
            else:
                path = f"/2/status/{status_id}"

            _, data = api_get(path, params=params, timeout=args.timeout)
            if args.as_json:
                print(json.dumps(data, indent=2, ensure_ascii=False))
            else:
                print(render_thread_or_conversation(data, limit=args.limit))
            return 0

        # Profile / timeline / articles / media
        handle = target["handle"]
        sub = target.get("sub", "")
        mode = "profile"
        if args.articles or sub == "articles":
            mode = "articles"
        elif args.media or sub == "media":
            mode = "media"
        elif args.statuses or args.with_replies or sub == "with_replies":
            mode = "statuses"

        if mode == "profile":
            path = f"/2/profile/{handle}"
            # /2/profile/{handle} only accepts about_account
            profile_params = {"about_account": "1"} if args.about else None
            _, data = api_get(path, params=profile_params, timeout=args.timeout)
            if args.as_json:
                print(json.dumps(data, indent=2, ensure_ascii=False))
            else:
                print(render_profile(data))
            return 0

        path = f"/2/profile/{handle}/{mode}"
        list_params: dict[str, Any] = {"count": max(1, min(args.limit, 100))}
        if args.cursor:
            list_params["cursor"] = args.cursor
        if args.lang:
            list_params["lang"] = args.lang
        if mode == "statuses" and (args.with_replies or sub == "with_replies"):
            list_params["with_replies"] = "true"

        _, data = api_get(path, params=list_params, timeout=args.timeout)
        if args.as_json:
            print(json.dumps(data, indent=2, ensure_ascii=False))
        else:
            print(render_timeline(data, handle=handle, kind=mode, limit=args.limit))
        return 0

    except FetchError as exc:
        print(f"Error: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
