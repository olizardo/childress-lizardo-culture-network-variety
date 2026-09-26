#!/usr/bin/env python3
"""
Scripts/insert_table3.py

One-time structural insertion of Table 3 (the residual/DIY-practical
leisure discriminant-validity check) into the "Practice and Networks"
Google Doc. Unlike Scripts/sync_manuscript.py (which only ever swaps the
bytes of an *existing* image behind an *existing* relationship ID) and
Scripts/fix_figure_table_page_fit.py (pure layout, zero prose), this
script performs genuine content insertion:

1. Registers a brand-new image relationship (next free rId) + media part
   pointing at Plots/table3_residual_leisure.png, sized/narrowed with the
   same height-capped `target_extent()` logic used for Tables 1-2.
2. Clones the Table 2 caption+image paragraph (which already carries
   `pageBreakBefore`, centered positioning, and bold caption run
   formatting) to build the new Table 3 caption+image paragraph, and
   inserts it immediately after Table 2's in the "Tables" section (no
   renumbering needed elsewhere since Appendix tables use an independent
   "A" prefix).
3. Inserts two new body paragraphs -- cloning the `pPr` of the existing
   Table 2 discussion paragraph (index 82) for plain body text, and of
   the "[Table 2 About Here]" placeholder (index 84) for the centered
   placeholder style -- introducing the discriminant-validity check and
   reporting the corrected nested Wald statistics, positioned right after
   the "[Table 2 About Here]" placeholder and before the "Discussion and
   Concluding Remarks" heading.

Run once against a fresh drive_download() of the live document; this is
NOT idempotent (rerunning would insert a second copy), so re-verify the
document doesn't already contain "Table 3." before running again.

Usage: insert_table3.py <in.docx> <out.docx>
"""

import copy
import re
import struct
import sys
import xml.etree.ElementTree as ET
import zipfile

NS = {
    "w": "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
    "a": "http://schemas.openxmlformats.org/drawingml/2006/main",
    "wp": "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing",
    "pic": "http://schemas.openxmlformats.org/drawingml/2006/picture",
    "r": "http://schemas.openxmlformats.org/officeDocument/2006/relationships",
}
W = NS["w"]
R_EMBED = "{%s}embed" % NS["r"]

EMU_PER_IN = 914400
W_MAX = int(6.5 * EMU_PER_IN)
H_MAX = int(7.2 * EMU_PER_IN)

TABLE3_PNG = "Plots/table3_residual_leisure.png"
TABLE3_CAPTION = (
    "Table 3. Coefficient Estimates of the Effects of Strong and Weak Tie "
    "Network Variety and Composition on the Residual (DIY-Practical) "
    "Leisure Factor."
)

RESULTS_PARA_1 = (
    "As a discriminant-validity check on our network composition measures, "
    "we re-estimate this same four-model sequence using the third, residual "
    "factor from our activities battery (Table 3). This factor loads on "
    "gardening, home or auto repair, and hiking or camping, and negatively "
    "on fast food consumption, and we interpret it as capturing everyday "
    "DIY-practical leisure rather than engagement with the arts or with "
    "solitary cultural consumption. If ideological network composition "
    "captures something specific about cultural and leisure participation, "
    "rather than a generic tendency toward social engagement of any kind, "
    "its effects should not extend to this residual domain."
)
RESULTS_PARA_2 = (
    "Consistent with this expectation, none of the four composition "
    "coefficients approaches statistical significance in Models 3 or 4 of "
    "Table 3, and formal Wald tests confirm that the composition block adds "
    "no explanatory power over models with variety alone (\u03c7\u00b2(4) = "
    "5.99, p = .200) or over models that already include the full set of "
    "socio-demographic controls (\u03c7\u00b2(4) = 5.61, p = .231). This null "
    "pattern, set alongside the associations we observe for public arts "
    "participation and solitary leisure, supports treating our composition "
    "measures as tapping domain-specific cultural and leisure engagement "
    "rather than an artifact correlated with any activity measure in the "
    "battery."
)
PLACEHOLDER_TEXT = "[Table 3 About Here]"


def get_png_dimensions(path):
    with open(path, "rb") as f:
        data = f.read(24)
        if len(data) >= 24 and data.startswith(b"\x89PNG\r\n\x1a\n"):
            return struct.unpack(">II", data[16:24])
    return None


def target_extent(pw, ph):
    cy_full = round(W_MAX * ph / pw)
    if cy_full <= H_MAX:
        return W_MAX, cy_full
    cx = round(H_MAX * pw / ph)
    return cx, H_MAX


def paragraph_text(p):
    return "".join(t.text or "" for t in p.findall(".//w:t", NS))


def next_free_rid(rels_root):
    ids = [int(m.group(1)) for el in rels_root
           if (m := re.match(r"rId(\d+)$", el.get("Id") or ""))]
    return f"rId{max(ids) + 1}"


def next_free_media_name(all_files):
    nums = [int(m.group(1)) for fn in all_files
            if (m := re.match(r"word/media/image(\d+)\.png$", fn))]
    return f"image{max(nums) + 1}.png"


def make_body_paragraph(template_ppr, text_runs):
    """Build a <w:p> with the given <w:pPr> (deep-copied) and one <w:r> per
    (text, italic) pair in text_runs."""
    p = ET.Element("{%s}p" % W)
    if template_ppr is not None:
        p.append(copy.deepcopy(template_ppr))
    for text, italic in text_runs:
        r = ET.SubElement(p, "{%s}r" % W)
        rPr = ET.SubElement(r, "{%s}rPr" % W)
        if italic:
            ET.SubElement(rPr, "{%s}i" % W).set("{%s}val" % W, "1")
            ET.SubElement(rPr, "{%s}iCs" % W).set("{%s}val" % W, "1")
        t = ET.SubElement(r, "{%s}t" % W)
        t.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
        t.text = text
    return p


def insert_table3(in_docx, out_docx, verbose=True):
    with zipfile.ZipFile(in_docx, "r") as zin:
        all_files = {item.filename: zin.read(item.filename) for item in zin.infolist()}

    rels_root = ET.fromstring(all_files["word/_rels/document.xml.rels"])
    doc_tree = ET.fromstring(all_files["word/document.xml"])
    body = doc_tree.find("w:body", NS)
    paras = list(body.findall("w:p", NS))

    # Sanity guard: bail if Table 3 already exists.
    for p in paras:
        if paragraph_text(p).startswith("Table 3."):
            raise RuntimeError("Table 3 caption already present; aborting to avoid a duplicate insert.")

    # ------------------------------------------------------------------
    # 1. New relationship + media part for the Table 3 PNG.
    # ------------------------------------------------------------------
    new_rid = next_free_rid(rels_root)
    new_media = next_free_media_name(all_files)
    rel = ET.SubElement(rels_root, "{http://schemas.openxmlformats.org/package/2006/relationships}Relationship")
    rel.set("Id", new_rid)
    rel.set("Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image")
    rel.set("Target", f"media/{new_media}")
    with open(TABLE3_PNG, "rb") as f:
        all_files[f"word/media/{new_media}"] = f.read()

    pw, ph = get_png_dimensions(TABLE3_PNG)
    cx, cy = target_extent(pw, ph)
    if verbose:
        print(f"  New image: {new_rid} -> word/media/{new_media} "
              f"({pw}x{ph}px -> {cx/EMU_PER_IN:.2f}in x {cy/EMU_PER_IN:.2f}in)")

    # ------------------------------------------------------------------
    # 2. Clone the Table 2 caption+image paragraph as a template, retarget
    #    the blip to the new rId/extent, and swap in the Table 3 caption.
    # ------------------------------------------------------------------
    table2_idx = next(i for i, p in enumerate(paras) if paragraph_text(p).startswith("Table 2."))
    table3_para = copy.deepcopy(paras[table2_idx])
    for blip in table3_para.findall(".//a:blip", NS):
        blip.set(R_EMBED, new_rid)
    for ext in table3_para.findall(".//wp:extent", NS):
        ext.set("cx", str(cx))
        ext.set("cy", str(cy))
    for ext in table3_para.findall(".//a:ext", NS):
        if ext.get("cx") is not None:
            ext.set("cx", str(cx))
            ext.set("cy", str(cy))
    docPr = table3_para.find(".//wp:docPr", NS)
    if docPr is not None:
        docPr.set("name", new_media)
    cNvPr = table3_para.find(".//pic:cNvPr", NS)
    if cNvPr is not None:
        cNvPr.set("name", new_media)
    t_elems = [t for t in table3_para.findall(".//w:t", NS)]
    caption_t_elems = [t for t in t_elems if (t.text or "").startswith("Table 2.")]
    if caption_t_elems:
        caption_t_elems[0].text = TABLE3_CAPTION
    else:
        raise RuntimeError("Could not locate the Table 2 caption text run to relabel.")

    # Insert the new caption+image paragraph immediately after Table 2's.
    body.insert(list(body).index(paras[table2_idx]) + 1, table3_para)

    # ------------------------------------------------------------------
    # 3. Insert the two Results-section discussion paragraphs plus the
    #    "[Table 3 About Here]" placeholder, right after the existing
    #    "[Table 2 About Here]" placeholder and before "Discussion and
    #    Concluding Remarks".
    # ------------------------------------------------------------------
    placeholder2_idx = next(
        i for i, p in enumerate(paras) if paragraph_text(p).strip() == "[Table 2 About Here]"
    )
    body_ppr_template = paras[82].find("w:pPr", NS) if paragraph_text(paras[82]).startswith("As before, adding ideological") else None
    if body_ppr_template is None:
        # Fall back to any plain body paragraph's pPr near the Table 2 discussion.
        body_ppr_template = paras[80].find("w:pPr", NS)
    placeholder_ppr_template = paras[placeholder2_idx].find("w:pPr", NS)

    new_paras = [
        make_body_paragraph(None, []),  # blank spacer
        make_body_paragraph(body_ppr_template, [(RESULTS_PARA_1, False)]),
        make_body_paragraph(body_ppr_template, [(RESULTS_PARA_2, False)]),
        make_body_paragraph(None, []),  # blank spacer
        make_body_paragraph(placeholder_ppr_template, [(PLACEHOLDER_TEXT, False)]),
    ]
    insert_at = list(body).index(paras[placeholder2_idx]) + 1
    for offset, np in enumerate(new_paras):
        body.insert(insert_at + offset, np)

    if verbose:
        print(f"  Inserted Table 3 caption+image after paragraph {table2_idx} (Table 2).")
        print(f"  Inserted 2 Results paragraphs + placeholder after paragraph {placeholder2_idx} ([Table 2 About Here]).")

    all_files["word/document.xml"] = ET.tostring(doc_tree, encoding="utf-8", xml_declaration=True)
    all_files["word/_rels/document.xml.rels"] = ET.tostring(rels_root, encoding="utf-8", xml_declaration=True)

    with zipfile.ZipFile(out_docx, "w", compression=zipfile.ZIP_DEFLATED) as zout:
        for fname, data in all_files.items():
            zout.writestr(fname, data)


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: insert_table3.py <in.docx> <out.docx>")
        sys.exit(1)
    insert_table3(sys.argv[1], sys.argv[2])
