#!/usr/bin/env python3
"""Check that every registered prose statistic appears in the prose of results_draft.tex.

Reads cache/manuscript/prose_numbers.csv (written by Scripts/manuscript/build_prose_numbers.R)
and the prose of results_draft.tex, meaning the running text and footnotes, with tables
(longtable, tabular*), figures (tikzpicture, center), captions, Note paragraphs, and
\\Needspace lines removed. Table and figure references ("Table 5", "Tables 6 to 8") and
exponents ("\\chi^2") are removed before numbers are extracted.

For each registry row the value must appear as a numeric token (or a number word) in the
prose of the section named in the row. Leading zeros, thousands separators, and LaTeX minus
signs are normalized on both sides, so "-0.12" matches "$-.12$" and "1,243" matches "1243".

Numeric tokens in the prose that match no registry row of their section are listed as
UNCATALOGUED for human review; they do not affect the exit code.

Exit code 0 only if no registry row is MISSING.

Run from the project root: python3 Scripts/manuscript/check_prose_numbers.py
"""

import argparse
import csv
import re
import sys
from collections import OrderedDict

EXCLUDED_ENVS = ("longtable", "tabular*", "tabular", "tikzpicture", "center", "figure", "table")
CAPTION_RE = re.compile(r"^(Table|Figure)\s+A?\d+\.")
SECTION_RE = re.compile(r"\\(?:sub)*section\*?\{([^}]*)\}")
TOKEN_RE = re.compile(r"(?<![\w.,])-?(?:\d{1,3}(?:,\d{3})+(?:\.\d+)?|\d*\.\d+|\d+)%?(?![\w])")
REF_RE = re.compile(r"\b(?:Tables?|Figures?)\s+A?\d+(?:\s*(?:,|and|to|--|–)\s*A?\d+)*")


def extract_prose(tex_path):
    """Return OrderedDict section -> prose text."""
    with open(tex_path, encoding="utf-8") as fh:
        lines = fh.read().splitlines()
    try:
        start = next(i for i, l in enumerate(lines) if l.strip().startswith(r"\begin{document}"))
    except StopIteration:
        start = -1
    prose = OrderedDict()
    section = "(preamble)"
    env_depth = 0
    in_note = False
    for line in lines[start + 1:]:
        s = line.strip()
        if s.startswith(r"\end{document}"):
            break
        for env in EXCLUDED_ENVS:
            if s.startswith(r"\begin{" + env + "}"):
                env_depth += 1
                break
        if env_depth > 0:
            for env in EXCLUDED_ENVS:
                if r"\end{" + env + "}" in s:
                    env_depth -= 1
                    break
            continue
        m = SECTION_RE.search(s)
        if m:
            section = m.group(1).strip()
            prose.setdefault(section, [])
            continue
        if not s:
            in_note = False
            continue
        if in_note:
            continue
        if s.startswith("Note.") or s.startswith(r"{\small Note."):
            in_note = True
            continue
        if CAPTION_RE.match(s) or s.startswith(r"\Needspace") or s.startswith(r"\hypertarget") \
                or s.startswith(r"{\renewcommand") or s == "}":
            continue
        prose.setdefault(section, []).append(s)
    return OrderedDict((k, normalize_text(" ".join(v))) for k, v in prose.items())


def normalize_text(t):
    t = re.sub(r"\\(?:label|ref|hypertarget)\{[^}]*\}", " ", t)
    t = t.replace("$-$", "-").replace("\u2212", "-")
    t = t.replace("$", " ")
    t = re.sub(r"\^\{?\d+\}?", "", t)
    t = REF_RE.sub(" ", t)
    t = re.sub(r"\\[A-Za-z]+\*?", " ", t)
    t = t.replace("{", " ").replace("}", " ").replace("\\%", "%")
    return re.sub(r"\s+", " ", t)


def norm_num(tok):
    x = tok.replace(",", "").rstrip("%").strip()
    neg = x.startswith("-")
    x = x.lstrip("-")
    if re.fullmatch(r"0\.\d+", x):
        x = x[1:]
    elif re.fullmatch(r"0+\d+", x):
        x = x.lstrip("0")
    return ("-" if neg else "") + x


def is_word(v):
    return re.fullmatch(r"[A-Za-z]+", v) is not None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--tex", default="results_draft.tex")
    ap.add_argument("--csv", default="cache/manuscript/prose_numbers.csv")
    ap.add_argument("--quiet", action="store_true", help="print only MISSING rows and the summary")
    args = ap.parse_args()

    prose = extract_prose(args.tex)
    tokens = {sec: [(m.group(0), norm_num(m.group(0)), m.start()) for m in TOKEN_RE.finditer(txt)]
              for sec, txt in prose.items()}
    token_sets = {sec: {t[1] for t in toks} for sec, toks in tokens.items()}

    with open(args.csv, encoding="utf-8", newline="") as fh:
        rows = list(csv.DictReader(fh))

    n_ok = n_missing = 0
    catalogued = {}
    print("== Registry rows ==")
    for r in rows:
        sec, val = r["section"], r["value"].strip()
        status, why = "MISSING", ""
        if sec not in prose:
            why = f"section '{sec}' not found in prose"
        elif is_word(val):
            if re.search(r"\b" + re.escape(val) + r"\b", prose[sec], flags=re.IGNORECASE):
                status = "OK"
            catalogued.setdefault(sec, set()).add(val.lower())
        else:
            nv = norm_num(val)
            catalogued.setdefault(sec, set()).add(nv)
            if re.fullmatch(r"-?[\d.]+", nv) and nv in token_sets[sec]:
                status = "OK"
            elif not re.fullmatch(r"-?[\d.]+", nv):
                why = f"value '{val}' is not numeric (failed claim in the R registry?)"
        if status == "OK":
            n_ok += 1
            if not args.quiet:
                print(f"OK       {r['id']:<36} {val:>10}  [{sec}]")
        else:
            n_missing += 1
            print(f"MISSING  {r['id']:<36} {val:>10}  [{sec}] {why or r['description']}")

    print("\n== Uncatalogued prose numbers (for review; not failures) ==")
    n_unc = 0
    for sec, toks in tokens.items():
        seen = OrderedDict()
        for raw, nv, pos in toks:
            if nv in catalogued.get(sec, set()):
                continue
            if nv not in seen:
                txt = prose[sec]
                ctx = txt[max(0, pos - 45):pos + len(raw) + 25]
                seen[nv] = [raw, 0, ctx]
            seen[nv][1] += 1
        for nv, (raw, cnt, ctx) in seen.items():
            n_unc += 1
            print(f"UNCATALOGUED  {raw:>8}  x{cnt}  [{sec}]  ...{ctx}...")

    print(f"\nSummary: {len(rows)} registry rows; OK {n_ok}; MISSING {n_missing}; "
          f"UNCATALOGUED {n_unc} distinct prose tokens")
    return 0 if n_missing == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
