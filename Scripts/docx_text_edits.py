#!/usr/bin/env python3
"""
Scripts/docx_text_edits.py

Reusable library for surgical, run-preserving text revisions to a live
Google Docs manuscript (downloaded as .docx), factored out of the
one-off Scripts/apply_oblimin_text_revisions.py script written for the
September 2026 varimax -> oblimin rotation revision round.

Two edit modes are supported, matching the two situations that keep
recurring across revision rounds in this project:

  1. FULL-PARAGRAPH edits: for a paragraph that is a self-contained
     methods/results statement, match its full concatenated text exactly
     and replace it wholesale. The entire new text is placed in the
     paragraph's first <w:t>; every other <w:t> in the paragraph is
     blanked. Safe for paragraphs with uniform run formatting (the
     overwhelming majority of body prose in this document).

  2. SUBSTRING splices: for a paragraph that mixes the target sentence
     with unrelated surrounding content (most notably this document's
     single very long Discussion paragraph), only the run(s) spanning
     the matched substring are touched; every other run -- and any
     inline formatting it carries -- is left byte-for-byte untouched.

Both edit functions collect every <w:t> descendant of a paragraph
directly (`p.findall(".//w:t", NS)`), NOT one-per-run via `r.find(...)`.
A single <w:r> run can legally contain more than one <w:t> (e.g. split
around a <w:tab/> or <w:br/>), and grabbing only the first <w:t> per run
silently leaves the rest of a multi-<w:t> run's original text in place
-- this produced a real stale-duplicate-sentence bug during the oblimin
revision round (see git history) and is the reason for this comment.

--------------------------------------------------------------------------
Usage as a library (preferred for a new revision round):

    from docx_text_edits import revise_docx

    FULL_PARAGRAPH_EDITS = [
        ("<exact old paragraph text>", "<new paragraph text>"),
        ...
    ]
    SUBSTRING_EDITS = [
        ("<old substring, long enough to be unique>", "<new substring>"),
        ...
    ]

    if __name__ == "__main__":
        import sys
        revise_docx(sys.argv[1], sys.argv[2],
                    full_paragraph_edits=FULL_PARAGRAPH_EDITS,
                    substring_edits=SUBSTRING_EDITS,
                    require_all=True)

Usage as a CLI against a JSON edit-set file (no Python authoring needed):

    python3 Scripts/docx_text_edits.py <in.docx> <out.docx> <edits.json>

    where edits.json is:
        {
          "full_paragraph_edits": [["old text", "new text"], ...],
          "substring_edits": [["old substring", "new substring"], ...]
        }

Both paths validate the result by round-tripping it through `pandoc -t
plain` (if pandoc is on PATH) and abort before writing if anything
failed to apply or the output doesn't parse -- never leave a
partially-edited or corrupted .docx sitting where a sync driver might
upload it.
"""

import json
import subprocess
import sys
import tempfile
import zipfile
import xml.etree.ElementTree as ET

NS = {"w": "http://schemas.openxmlformats.org/wordprocessingml/2006/main"}

# Re-registering the document's namespace prefixes before serialization
# keeps ET.tostring() from renaming them to auto-generated ns0:/ns1:/...
# prefixes. That's technically valid XML, but pandoc's docx reader (used
# below for validation, and by some downstream tooling) fails to parse
# it, so we always register these before writing anything back out.
_NAMESPACES = [
    ("w", "http://schemas.openxmlformats.org/wordprocessingml/2006/main"),
    ("w14", "http://schemas.microsoft.com/office/word/2010/wordml"),
    ("r", "http://schemas.openxmlformats.org/officeDocument/2006/relationships"),
    ("wp", "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"),
    ("a", "http://schemas.openxmlformats.org/drawingml/2006/main"),
    ("pic", "http://schemas.openxmlformats.org/drawingml/2006/picture"),
]
for _prefix, _uri in _NAMESPACES:
    ET.register_namespace(_prefix, _uri)


class RevisionError(RuntimeError):
    """Raised when a requested edit can't be applied or the result fails
    validation. Callers should treat this as "nothing was written" --
    revise_docx() only writes out_docx after every check has passed."""


def para_text(p):
    return "".join(t.text or "" for t in p.findall(".//w:t", NS))


def apply_full_paragraph_edits(body, edits, verbose=True):
    """Exact whole-paragraph replacement. Returns the number of edits
    (out of len(edits)) that matched some paragraph and were applied.
    If more than one paragraph matches the same old text, ALL of them
    are replaced (duplicated boilerplate paragraphs are not unheard of
    in Google Docs exports and should generally all be fixed together)."""
    applied = 0
    for old, new in edits:
        matched_any = False
        for p in body.findall(".//w:p", NS):
            if para_text(p) != old:
                continue
            # Collect every <w:t> descendant directly -- see module
            # docstring for why r.find("w:t") per run is NOT equivalent.
            t_elems = p.findall(".//w:t", NS)
            if not t_elems:
                continue
            t_elems[0].text = new
            for extra in t_elems[1:]:
                extra.text = ""
            matched_any = True
            if verbose:
                print(f"  [full-paragraph] matched & replaced ({len(old)} -> {len(new)} chars)")
        if matched_any:
            applied += 1
        elif verbose:
            print(f"  [WARN] full-paragraph text not found: '{old[:70]}...'")
    return applied


def apply_substring_edits(body, edits, verbose=True):
    """Run-preserving substring splice: only the run(s) whose text
    overlaps the match are touched. Returns the number of edits (out of
    len(edits)) applied. Only the FIRST matching paragraph for each
    substring is edited (unlike full-paragraph edits, repeating the same
    substring search after a paragraph has already been edited could
    otherwise re-match the replacement text itself in pathological
    cases, so we deliberately stop at one match per edit)."""
    applied = 0
    for old_substr, new_substr in edits:
        found = False
        for p in body.findall(".//w:p", NS):
            text = para_text(p)
            idx = text.find(old_substr)
            if idx == -1:
                continue
            end = idx + len(old_substr)

            t_elems = p.findall(".//w:t", NS)
            cursor = 0
            spans = []
            for t in t_elems:
                s = t.text or ""
                spans.append((t, cursor, cursor + len(s)))
                cursor += len(s)

            touched = [(t, s, e) for (t, s, e) in spans if e > idx and s < end]
            if not touched:
                continue

            first_t, first_s, _ = touched[0]
            last_t, last_s, _ = touched[-1]
            prefix = (first_t.text or "")[: idx - first_s]
            suffix = (last_t.text or "")[end - last_s:]

            if first_t is last_t:
                first_t.text = prefix + new_substr + suffix
            else:
                first_t.text = prefix + new_substr
                last_t.text = suffix
                for t, _, _ in touched[1:-1]:
                    t.text = ""

            if verbose:
                print(f"  [substring splice] matched & replaced "
                      f"({len(touched)} run(s) touched): "
                      f"'{old_substr[:40]}...' -> '{new_substr[:40]}...'")
            found = True
            break
        if found:
            applied += 1
        elif verbose:
            print(f"  [WARN] substring not found: '{old_substr[:70]}...'")
    return applied


def validate_docx_with_pandoc(docx_path, verbose=True):
    """Best-effort round-trip check via `pandoc -t plain`. Returns True
    if pandoc is unavailable (skip, don't block) or if it parses the
    file successfully; False only if pandoc is present AND fails."""
    try:
        with tempfile.NamedTemporaryFile(suffix=".txt") as tmp:
            result = subprocess.run(
                ["pandoc", docx_path, "-t", "plain", "-o", tmp.name],
                capture_output=True, text=True, timeout=60,
            )
    except FileNotFoundError:
        if verbose:
            print("  [validate] pandoc not on PATH -- skipping validation.")
        return True
    if result.returncode != 0:
        if verbose:
            print(f"  [validate] pandoc FAILED to parse output: {result.stderr.strip()}")
        return False
    if verbose:
        print("  [validate] pandoc parsed the output docx successfully.")
    return True


def revise_docx(
    in_docx,
    out_docx,
    full_paragraph_edits=None,
    substring_edits=None,
    require_all=True,
    validate=True,
    verbose=True,
):
    """Apply full_paragraph_edits and substring_edits to in_docx and
    write the result to out_docx.

    require_all: if True (default), raise RevisionError and write
        NOTHING if any single edit failed to match, or if validation
        fails -- this is the safe default for a revision batch that's
        supposed to apply completely or not at all.
    validate: if True (default), round-trip the result through pandoc
        before writing out_docx.

    Returns (n_full_applied, n_substring_applied).
    """
    full_paragraph_edits = full_paragraph_edits or []
    substring_edits = substring_edits or []

    with zipfile.ZipFile(in_docx, "r") as zin:
        all_files = {item.filename: zin.read(item.filename) for item in zin.infolist()}

    doc_tree = ET.fromstring(all_files["word/document.xml"])
    body = doc_tree.find("w:body", NS)

    n_full = apply_full_paragraph_edits(body, full_paragraph_edits, verbose=verbose)
    n_sub = apply_substring_edits(body, substring_edits, verbose=verbose)

    expected = len(full_paragraph_edits) + len(substring_edits)
    got = n_full + n_sub
    if verbose:
        print(f"Applied {n_full}/{len(full_paragraph_edits)} full-paragraph edits, "
              f"{n_sub}/{len(substring_edits)} substring splices "
              f"({got}/{expected} total).")
    if require_all and got != expected:
        raise RevisionError(
            f"Expected {expected} edits to apply, got {got}. Nothing was written "
            f"to {out_docx}; re-check the affected old-text strings against a "
            f"fresh download before retrying."
        )

    all_files["word/document.xml"] = ET.tostring(doc_tree, encoding="utf-8", xml_declaration=True)

    with tempfile.NamedTemporaryFile(suffix=".docx", delete=False) as tmp:
        tmp_path = tmp.name
    with zipfile.ZipFile(tmp_path, "w", compression=zipfile.ZIP_DEFLATED) as zout:
        for fname, data in all_files.items():
            zout.writestr(fname, data)

    if validate:
        ok = validate_docx_with_pandoc(tmp_path, verbose=verbose)
        if require_all and not ok:
            raise RevisionError(
                f"Output failed pandoc validation. Nothing was written to {out_docx}; "
                f"inspect {tmp_path} to debug before retrying."
            )

    with open(tmp_path, "rb") as f:
        data = f.read()
    with open(out_docx, "wb") as f:
        f.write(data)

    return n_full, n_sub


def _load_edit_set(json_path):
    with open(json_path, "r", encoding="utf-8") as f:
        spec = json.load(f)
    full = [tuple(pair) for pair in spec.get("full_paragraph_edits", [])]
    sub = [tuple(pair) for pair in spec.get("substring_edits", [])]
    return full, sub


if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: docx_text_edits.py <in.docx> <out.docx> <edits.json>")
        sys.exit(1)
    in_path, out_path, edits_path = sys.argv[1:4]
    full_edits, sub_edits = _load_edit_set(edits_path)
    try:
        revise_docx(in_path, out_path, full_edits, sub_edits)
    except RevisionError as e:
        print(f"[ERROR] {e}")
        sys.exit(1)
    print(f"Wrote {out_path}.")
