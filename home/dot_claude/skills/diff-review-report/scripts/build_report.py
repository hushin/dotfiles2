#!/usr/bin/env python3
"""hunks.json + review.json から自己完結の HTML レビューレポートを生成する。

レポートの見た目は assets/report_template.html が持っている。ここがやるのは
「LLM が書いた review.json が hunks.json と整合しているか」の検証と、
テンプレートへのデータ埋め込みだけ。

検証を通すことが重要なのは、レビューの価値が網羅性にあるから。どの hunk も
必ずどれか 1 つのグループに属していないと、読み手は「この差分は見落とされたのか、
意図的に触れられなかったのか」を判別できない。

使い方:
    python build_report.py --hunks hunks.json --review review.json -o report.html
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

TEMPLATE = Path(__file__).resolve().parent.parent / "assets" / "report_template.html"
PLACEHOLDER = "__REPORT_DATA__"

RISK_ORDER = {"high": 0, "medium": 1, "low": 2}
SEVERITY = {"error", "warning", "info"}


def fail(msgs: list[str]) -> None:
    print("review.json の検証に失敗しました:", file=sys.stderr)
    for m in msgs:
        print(f"  - {m}", file=sys.stderr)
    raise SystemExit(1)


def validate(hunks_doc: dict, review: dict, allow_unassigned: bool) -> list[str]:
    errors: list[str] = []
    known = {h["id"] for h in hunks_doc["hunks"]}
    seen: dict[str, str] = {}

    groups = review.get("groups")
    if not groups:
        errors.append("groups が空です。最低 1 つの変更グループが必要です。")
        return errors

    for gi, g in enumerate(groups):
        where = f"groups[{gi}]"
        for field in ("title", "intent", "risk"):
            if not g.get(field):
                errors.append(f"{where}: {field} が未設定です。")
        if g.get("risk") not in RISK_ORDER:
            errors.append(f"{where}: risk は high / medium / low のいずれかです (現在: {g.get('risk')!r})。")
        if not g.get("hunks"):
            errors.append(f"{where}: hunks が空です。グループには必ず hunk を割り当ててください。")
        for hid in g.get("hunks", []):
            if hid not in known:
                errors.append(f"{where}: 未知の hunk id {hid!r}。")
            elif hid in seen:
                errors.append(f"{where}: hunk {hid} は {seen[hid]!r} と重複しています。hunk は 1 グループにのみ属します。")
            else:
                seen[hid] = g.get("title", where)
        for fi, f in enumerate(g.get("findings", [])):
            fwhere = f"{where}.findings[{fi}]"
            if f.get("severity") not in SEVERITY:
                errors.append(f"{fwhere}: severity は error / warning / info のいずれかです (現在: {f.get('severity')!r})。")
            if not f.get("title") or not f.get("detail"):
                errors.append(f"{fwhere}: title と detail は必須です。")
            if f.get("stage") not in ("blind", "plan"):
                errors.append(f"{fwhere}: stage は blind / plan のいずれかです (現在: {f.get('stage')!r})。")
            if f.get("verdict") not in (None, "kept", "demoted"):
                errors.append(f"{fwhere}: verdict は kept / demoted のいずれかです。")
            if f.get("verdict") == "demoted" and not f.get("verdict_note"):
                errors.append(f"{fwhere}: verdict=demoted には verdict_note (plan と照合して格下げした理由) が必要です。")

    missing = [h["id"] for h in hunks_doc["hunks"] if h["id"] not in seen]
    if missing and not allow_unassigned:
        head = ", ".join(missing[:20]) + (" …" if len(missing) > 20 else "")
        errors.append(
            f"{len(missing)} 件の hunk がどのグループにも属していません: {head}\n"
            "    すべての差分を必ずどこかのグループへ入れてください。"
            "レビュー対象外と判断したものも「機械的変更」等のグループにまとめます。"
            "どうしても後回しにする場合のみ --allow-unassigned。"
        )
    return errors


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--hunks", required=True)
    ap.add_argument("--review", required=True)
    ap.add_argument("-o", "--out", required=True)
    ap.add_argument("--allow-unassigned", action="store_true", help="未分類 hunk を「未分類」グループへ回収して続行する")
    ns = ap.parse_args()

    hunks_doc = json.loads(Path(ns.hunks).read_text(encoding="utf-8"))
    review = json.loads(Path(ns.review).read_text(encoding="utf-8"))

    errors = validate(hunks_doc, review, ns.allow_unassigned)
    if errors:
        fail(errors)

    assigned = {hid for g in review["groups"] for hid in g["hunks"]}
    leftovers = [h["id"] for h in hunks_doc["hunks"] if h["id"] not in assigned]
    if leftovers:
        review["groups"].append({
            "title": "未分類",
            "intent": "グループ分けされなかった差分。レビュー時に自分で目を通してください。",
            "risk": "medium",
            "kind": "chore",
            "hunks": leftovers,
            "findings": [],
            "needs_improvement": True,
        })

    review["groups"].sort(key=lambda g: RISK_ORDER.get(g.get("risk"), 9))

    data = {
        "title": review.get("title") or "差分レビュー",
        "subtitle": review.get("subtitle", ""),
        "base": review.get("base") or " ".join(hunks_doc.get("diff_args", [])),
        "repo": review.get("repo", ""),
        "plan": review.get("plan", ""),
        "overview": review.get("overview", ""),
        "stats": hunks_doc["stats"],
        "groups": review["groups"],
        "hunks": {h["id"]: h for h in hunks_doc["hunks"]},
    }

    html = TEMPLATE.read_text(encoding="utf-8")
    if PLACEHOLDER not in html:
        raise SystemExit(f"テンプレートに {PLACEHOLDER} がありません: {TEMPLATE}")
    payload = json.dumps(data, ensure_ascii=False).replace("</", "<\\/")
    out = Path(ns.out)
    out.write_text(html.replace(PLACEHOLDER, payload), encoding="utf-8")

    n_find = sum(len(g.get("findings", [])) for g in data["groups"])
    print(f"生成しました: {out.resolve()}")
    print(f"  グループ {len(data['groups'])} / hunk {data['stats']['hunks']} / 指摘 {n_find}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
