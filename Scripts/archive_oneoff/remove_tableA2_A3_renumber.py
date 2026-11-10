#!/usr/bin/env python3
"""
Scripts/remove_tableA2_A3_renumber.py

One-time structural removal of Table A2 (activity item loadings) and
Table A3 (weak-/strong-tie item loadings) from the live "Practice and
Networks" Google Doc -- both are dropped because they duplicate the
loading matrices already shown as heat maps in Figure 1 and Figure 2,
respectively. The surviving appendix table (formerly Table A5, the
network-variety-count validation table) is renumbered to Table A2 so
the appendix table sequence has no gap (A1, A2), per the author's
explicit choice.

Structural paragraph deletion is genuine DOM surgery that
Scripts/docx_text_edits.py's edit modes don't cover (those only ever
touch <w:t> text, never remove/insert paragraphs), so this script does
that part directly. The caption rename and in-text reference cleanup
that go with it are then applied via
docx_text_edits.apply_substring_edits() / validate_docx_with_pandoc()
so they get the same run-preserving, validated treatment as any other
text revision in this project.

Locating the two images to remove: because Google Docs anchors
floating images by absolute position independent of which paragraph
the <w:drawing> XML element happens to sit in (see AGENTS.md Section
4), the paragraph that visually appears adjacent to a caption is NOT a
reliable way to find that caption's own image -- e.g. Table A2's image
(rId15) actually sits, in document.xml's sibling order, in the
paragraph right after Table A1's caption, not after Table A2's own.
This script instead locates each image strictly by relationship ID
(rId15 = Table A2's activity-loadings PNG, rId16 = Table A3's
tie-loadings PNG; both content-verified by opening the actual media
files before writing this script -- see conversation record), wherever
in the document that <a:blip> physically sits, and removes exactly
those two image paragraphs, the two caption paragraphs, and the blank
spacer/manual-page-break paragraphs strictly between/around them. No
other image (Table A1's rId14, Figure A4's rId17, Table A5/new-Table
A2's rId18, Table 3's rId19) is touched, and the now-orphaned rId15/
rId16 relationships and media parts are dropped from the package.

Run once against a fresh drive_download() of the live document; NOT
idempotent (a second run will fail the sanity guard and abort rather
than silently no-op or double-delete).

Usage: remove_tableA2_A3_renumber.py <in.docx> <out.docx>
"""

import re
import sys
import tempfile
import xml.etree.ElementTree as ET
import zipfile

sys.path.insert(0, "Scripts")
from docx_text_edits import apply_substring_edits, validate_docx_with_pandoc  # noqa: E402

NS = {
    "w": "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
    "a": "http://schemas.openxmlformats.org/drawingml/2006/main",
    "r": "http://schemas.openxmlformats.org/officeDocument/2006/relationships",
}
R_EMBED = "{%s}embed" % NS["r"]

RID_TABLE_A2_IMAGE = "rId15"
RID_TABLE_A3_IMAGE = "rId16"

TABLE_A2_CAPTION = "Table A2. Factor Loadings for Culture and Leisure Activity Items."
TABLE_A3_CAPTION = "Table A3. Factor Loadings for Strong- and Weak-Tie Network Variety and Composition Items."

SUBSTRING_EDITS = [
    # Results-section mentions of the doomed Table A2.
    (
        "As seen in Figure 1 and in Table A2 in the Appendix, Factor 1 is characterized by",
        "As seen in Figure 1, Factor 1 is characterized by",
    ),
    (
        " We include the full loadings on all three factors in Appendix Table A2.",
        "",
    ),
    # Renumber the surviving validation table, Table A5 -> Table A2 (caption + in-text reference).
    (
        "Table A5. Correlation Matrix of Factor Scores for Strong and Weak Tie Network "
        "Variety and Composition with the Count of Positions Respondents are Connected to.",
        "Table A2. Correlation Matrix of Factor Scores for Strong and Weak Tie Network "
        "Variety and Composition with the Count of Positions Respondents are Connected to.",
    ),
    (
        "and in Table A5, associations are reported as Pearson correlations",
        "and in Table A2, associations are reported as Pearson correlations",
    ),
]


def para_text(p):
    return "".join(t.text or "" for t in p.findall(".//w:t", NS))


def has_blip(p, rid):
    return any(b.attrib.get(R_EMBED) == rid for b in p.findall(".//a:blip", NS))


def is_blank(p):
    """Blank text and no image -- a pure spacer/manual-break paragraph,
    safe to absorb into the removal set. (Deliberately permissive about
    <w:br/> here, unlike the general-purpose blank-spacer trimmer in
    fix_figure_table_page_fit.py, because within this tightly bounded
    span we've already manually verified the two <w:br w:type="page"/>
    paragraphs present are page breaks that existed only to separate
    Table A2/A3 from their neighbors and should go with them.)"""
    return not para_text(p).strip() and not p.findall(".//a:blip", NS)


def remove_tables(in_docx, out_docx, verbose=True):
    with zipfile.ZipFile(in_docx, "r") as zin:
        all_files = {item.filename: zin.read(item.filename) for item in zin.infolist()}

    rels_root = ET.fromstring(all_files["word/_rels/document.xml.rels"])
    doc_tree = ET.fromstring(all_files["word/document.xml"])
    body = doc_tree.find("w:body", NS)
    paras = list(body.findall("w:p", NS))

    a2_caption_idx = next((i for i, p in enumerate(paras) if para_text(p).strip() == TABLE_A2_CAPTION), None)
    a3_caption_idx = next((i for i, p in enumerate(paras) if para_text(p).strip() == TABLE_A3_CAPTION), None)
    if a2_caption_idx is None or a3_caption_idx is None:
        raise RuntimeError(
            "Could not find the Table A2 and/or Table A3 caption paragraphs by exact text. "
            "Re-check against a fresh download -- the captions may already be gone/reworded, "
            "or this script has already been run once."
        )
    a2_image_idx = next((i for i, p in enumerate(paras) if has_blip(p, RID_TABLE_A2_IMAGE)), None)
    a3_image_idx = next((i for i, p in enumerate(paras) if has_blip(p, RID_TABLE_A3_IMAGE)), None)
    if a2_image_idx is None or a3_image_idx is None:
        raise RuntimeError(
            f"Could not find the image paragraph for {RID_TABLE_A2_IMAGE} and/or "
            f"{RID_TABLE_A3_IMAGE}. The rId -> content mapping may have drifted since this "
            "script was written -- re-verify by extracting and opening the media files before "
            "re-running (see this script's module docstring)."
        )

    core = {a2_caption_idx, a3_caption_idx, a2_image_idx, a3_image_idx}
    lo, hi = min(core), max(core)

    # Absorb every paragraph strictly between lo and hi that isn't already
    # a known target: they must be blank spacers/page-breaks, or we abort
    # rather than risk eating real prose.
    to_remove = set(core)
    for i in range(lo, hi + 1):
        if i in to_remove:
            continue
        if is_blank(paras[i]):
            to_remove.add(i)
        else:
            raise RuntimeError(
                f"Paragraph {i} inside the Table A2/A3 span is neither a known removal "
                f"target nor blank ({para_text(paras[i])[:80]!r}); aborting."
            )

    # Absorb one blank spacer immediately outside each end too, so we don't
    # leave two blank paragraphs stacked where Table A2/A3 used to be
    # (walking outward from the caption at index 167 for Table A1, and up to
    # -- but not touching -- Figure A4's own image paragraph on the other
    # side; both are guaranteed non-blank and stop the walk).
    i = lo - 1
    while i >= 0 and is_blank(paras[i]):
        to_remove.add(i)
        i -= 1
    j = hi + 1
    while j < len(paras) and is_blank(paras[j]):
        to_remove.add(j)
        j += 1

    if verbose:
        print(f"  Removing {len(to_remove)} paragraphs: {sorted(to_remove)}")
        for i in sorted(to_remove):
            print(f"    [{i}] {para_text(paras[i])[:70]!r}")

    for i in sorted(to_remove, reverse=True):
        body.remove(paras[i])

    n_sub = apply_substring_edits(body, SUBSTRING_EDITS, verbose=verbose)
    if n_sub != len(SUBSTRING_EDITS):
        raise RuntimeError(
            f"Expected {len(SUBSTRING_EDITS)} substring edits to apply, got {n_sub}. "
            "Nothing was written; re-check the affected strings against a fresh download."
        )

    # Drop the now-orphaned rId15/rId16 relationships and their media parts.
    removed_rels = 0
    for rel in list(rels_root):
        if rel.get("Id") in (RID_TABLE_A2_IMAGE, RID_TABLE_A3_IMAGE):
            target = rel.get("Target")
            rels_root.remove(rel)
            removed_rels += 1
            media_path = "word/" + target
            all_files.pop(media_path, None)
            if verbose:
                print(f"  Dropped relationship {rel.get('Id')} and {media_path}")
    if removed_rels != 2:
        raise RuntimeError(f"Expected to drop exactly 2 relationships, dropped {removed_rels}.")

    all_files["word/document.xml"] = ET.tostring(doc_tree, encoding="utf-8", xml_declaration=True)
    all_files["word/_rels/document.xml.rels"] = ET.tostring(rels_root, encoding="utf-8", xml_declaration=True)

    with tempfile.NamedTemporaryFile(suffix=".docx", delete=False) as tmp:
        tmp_path = tmp.name
    with zipfile.ZipFile(tmp_path, "w", compression=zipfile.ZIP_DEFLATED) as zout:
        for fname, data in all_files.items():
            zout.writestr(fname, data)

    if not validate_docx_with_pandoc(tmp_path, verbose=verbose):
        raise RuntimeError(f"Output failed pandoc validation; inspect {tmp_path} before retrying.")

    with open(tmp_path, "rb") as f:
        data = f.read()
    with open(out_docx, "wb") as f:
        f.write(data)


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: remove_tableA2_A3_renumber.py <in.docx> <out.docx>")
        sys.exit(1)
    remove_tables(sys.argv[1], sys.argv[2])
