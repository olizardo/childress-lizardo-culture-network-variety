#!/usr/bin/env python3
"""
Scripts/move_tableA3_to_appendix.py

One-time structural edit (September 2026): the residual/DIY-practical
leisure discriminant-validity table was relabeled "Table A3" (see
Scripts/revise_table3_to_A3_references.py) but its caption+image
paragraphs had NOT actually been relocated -- they still physically sat
in the main-text "Tables" section, directly before the "APPENDIX"
heading. This script physically moves that block into the Appendix,
placing it immediately after Table A2 (the last item in the appendix),
so the document order now matches the caption numbering.

Block identification is done by text/structure search, not hardcoded
indices, since Google Docs renumbers body-element order on every
re-save (see AGENTS.md "Live Document rId -> Media -> Replacement Asset
Mapping" section for the general lesson):

  - The Table A3 block = [optional preceding manual-page-break
    paragraph] + [caption paragraph] + [any blank spacer paragraphs] +
    [the paragraph holding the <w:drawing>]. In the live document as of
    this edit that is 4 paragraphs: a paragraph containing a manual
    <w:br w:type="page"/> run, the "Table A3. ..." caption paragraph, a
    blank spacer paragraph, and the image paragraph.
  - The insertion anchor is the paragraph holding Table A2's
    <w:drawing> (found by locating the "Table A2. ..." caption
    paragraph and then scanning forward for the next paragraph that
    contains a drawing). The extracted block is spliced in immediately
    after that anchor paragraph, so Table A3 becomes the new final item
    in the Appendix, retaining its own page break.

This also fixes one in-text cross-reference that becomes inconsistent
once Table A3 is no longer contiguous with Tables 1-2: the Appendix
"Network Variety" validation paragraph said "...we interpret the
weak-tie composition estimates in Tables 1-3 accordingly," which reads
as if Tables 1, 2, and 3 were a contiguous run in the main Tables
section. Corrected to "Tables 1-2 and A3".

Usage: move_tableA3_to_appendix.py <in.docx> <out.docx>
"""

import sys
import zipfile
import xml.etree.ElementTree as ET

sys.path.insert(0, "Scripts")
from docx_text_edits import (
    NS,
    apply_substring_edits,
    para_text,
    validate_docx_with_pandoc,
)

CAPTION_A3 = "Table A3. Coefficient Estimates"
CAPTION_A2 = "Table A2. Correlation Matrix"

SUBSTRING_EDITS = [
    (
        "we interpret the weak-tie composition estimates in Tables 1\u20133 accordingly.",
        "we interpret the weak-tie composition estimates in Tables 1\u20132 and A3 accordingly.",
    ),
]


def has_drawing(p):
    return p.find(".//w:drawing", NS) is not None


def has_page_break_run(p):
    return p.find(".//w:br[@w:type='page']", NS) is not None


def find_caption_index(body_list, prefix):
    for i, p in enumerate(body_list):
        if not p.tag.endswith("}p"):
            continue
        if para_text(p).strip().startswith(prefix):
            return i
    raise ValueError(f"caption not found: {prefix!r}")


def find_block_bounds(body_list, cap_idx):
    """Returns (start, end) inclusive indices for the caption's full
    caption+spacer+image block."""
    start = cap_idx
    if cap_idx > 0 and has_page_break_run(body_list[cap_idx - 1]):
        start = cap_idx - 1
    end = cap_idx
    i = cap_idx + 1
    while i < len(body_list):
        p = body_list[i]
        if not p.tag.endswith("}p"):
            break
        end = i
        if has_drawing(p):
            break
        if para_text(p).strip():
            # Hit the next real content paragraph without finding a
            # drawing -- something unexpected; stop before consuming it.
            end = i - 1
            break
        i += 1
    return start, end


def move_block(in_docx, out_docx):
    with zipfile.ZipFile(in_docx, "r") as zin:
        all_files = {item.filename: zin.read(item.filename) for item in zin.infolist()}

    doc_tree = ET.fromstring(all_files["word/document.xml"])
    body = doc_tree.find("w:body", NS)

    # --- 1. Locate and extract the Table A3 block ---
    body_list = list(body)
    a3_cap_idx = find_caption_index(body_list, CAPTION_A3)
    a3_start, a3_end = find_block_bounds(body_list, a3_cap_idx)
    if not has_drawing(body_list[a3_end]):
        raise ValueError(
            f"Table A3 block scan ended at index {a3_end} without a drawing "
            f"paragraph -- re-inspect document structure before retrying."
        )
    block_elems = body_list[a3_start:a3_end + 1]
    print(f"[move] Table A3 block spans body indices {a3_start}-{a3_end} "
          f"({len(block_elems)} paragraph(s)).")

    for elem in block_elems:
        body.remove(elem)

    # --- 2. Locate the Table A2 drawing paragraph as insertion anchor ---
    body_list = list(body)  # refresh after removal
    a2_cap_idx = find_caption_index(body_list, CAPTION_A2)
    a2_draw_idx = None
    for i in range(a2_cap_idx, min(a2_cap_idx + 5, len(body_list))):
        if has_drawing(body_list[i]):
            a2_draw_idx = i
            break
    if a2_draw_idx is None:
        raise ValueError("Could not find Table A2's drawing paragraph.")
    print(f"[move] Inserting after Table A2's drawing paragraph "
          f"(body index {a2_draw_idx}).")

    insert_at = a2_draw_idx + 1
    for offset, elem in enumerate(block_elems):
        body.insert(insert_at + offset, elem)

    # --- 3. Fix the now-inconsistent "Tables 1-3" cross-reference ---
    n_sub = apply_substring_edits(body, SUBSTRING_EDITS, verbose=True)
    if n_sub != len(SUBSTRING_EDITS):
        raise ValueError(
            f"Expected {len(SUBSTRING_EDITS)} substring edit(s) to apply, got {n_sub}."
        )

    all_files["word/document.xml"] = ET.tostring(
        doc_tree, encoding="utf-8", xml_declaration=True
    )

    with zipfile.ZipFile(out_docx, "w", compression=zipfile.ZIP_DEFLATED) as zout:
        for fname, data in all_files.items():
            zout.writestr(fname, data)

    if not validate_docx_with_pandoc(out_docx, verbose=True):
        raise ValueError(
            f"Output failed pandoc validation; inspect {out_docx} before uploading."
        )


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: move_tableA3_to_appendix.py <in.docx> <out.docx>")
        sys.exit(1)
    move_block(sys.argv[1], sys.argv[2])
    print(f"Wrote {sys.argv[2]}.")
