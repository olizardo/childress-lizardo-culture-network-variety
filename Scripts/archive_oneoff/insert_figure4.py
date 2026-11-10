#!/usr/bin/env python3
"""
Scripts/insert_figure4.py

One-time structural insertion of Figure 4 (predicted solitary leisure by
weak-/strong-tie network variety -- the countervailing-effects companion
to Figure 3) into the "Practice and Networks" Google Doc.

Two coordinated insertions, both genuine content insertion (not covered
by Scripts/docx_text_edits.py's text-only edit modes, nor by
Scripts/sync_manuscript.py's swap-existing-image-bytes-only design):

1. Registers a brand-new image relationship (next free rId) + media part
   pointing at Plots/fig4_leisure_variety_margins_plot.png, sized with
   the same height-capped target_extent() logic used elsewhere in this
   project's OpenXML tooling. Clones Figure 3's caption+image paragraph
   (which already combines both in one <w:p>, exactly like Table 2/3's
   template used by insert_table3.py) to build the new Figure 4
   caption+image paragraph, and inserts it immediately after Figure 3's
   in the "Figures" section.
2. Inserts a new Results-section discussion paragraph -- cloning the
   <w:pPr> of the existing Table 2 network-variety paragraph (index 81,
   "Table 2 examines the network predictors...") for plain body text,
   and the <w:pPr> of the existing "[Figure 3 About Here]" placeholder
   (index 76) for the centered placeholder style -- introducing Figure 4
   and describing the crossing pattern, plus a "[Figure 4 About Here]"
   placeholder, positioned right after that Table 2 variety-effects
   paragraph and before the following socio-demographic-effects
   paragraph (index 82, "Looking at Model 2...").

Run once against a fresh drive_download() of the live document; this is
NOT idempotent (rerunning would insert a second copy), so re-verify the
document doesn't already contain "Figure 4." before running again.

Usage: insert_figure4.py <in.docx> <out.docx>
"""

import copy
import re
import struct
import sys
import tempfile
import xml.etree.ElementTree as ET
import zipfile

sys.path.insert(0, "Scripts")
from docx_text_edits import validate_docx_with_pandoc  # noqa: E402

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

FIGURE4_PNG = "Plots/fig4_leisure_variety_margins_plot.png"
FIGURE4_CAPTION = "Figure 4. Weak and Strong Tie Network Variety on Solitary Leisure"

RESULTS_PARA = (
    "This countervailing pattern is illustrated in Figure 4, which plots the "
    "predicted score in the solitary leisure factor on the y-axis as a "
    "function of the respondent\u2019s score in the strong-tie (solid line) "
    "and weak-tie (dashed line) variety factors on the x-axis, following "
    "the same predicted-margins specification used for Figure 3. The two "
    "predicted lines cross near the sample mean of the variety factors, "
    "with weak-tie variety associated with higher predicted solitary "
    "leisure and strong-tie variety associated with lower predicted "
    "solitary leisure across the observed range of network variety, "
    "after adjusting for other socio-demographic factors."
)
PLACEHOLDER_TEXT = "[Figure 4 About Here]"


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


def insert_figure4(in_docx, out_docx, verbose=True):
    with zipfile.ZipFile(in_docx, "r") as zin:
        all_files = {item.filename: zin.read(item.filename) for item in zin.infolist()}

    rels_root = ET.fromstring(all_files["word/_rels/document.xml.rels"])
    doc_tree = ET.fromstring(all_files["word/document.xml"])
    body = doc_tree.find("w:body", NS)
    paras = list(body.findall("w:p", NS))

    for p in paras:
        if paragraph_text(p).startswith("Figure 4."):
            raise RuntimeError("Figure 4 caption already present; aborting to avoid a duplicate insert.")

    # ------------------------------------------------------------------
    # 1. New relationship + media part for the Figure 4 PNG.
    # ------------------------------------------------------------------
    new_rid = next_free_rid(rels_root)
    new_media = next_free_media_name(all_files)
    rel = ET.SubElement(rels_root, "{http://schemas.openxmlformats.org/package/2006/relationships}Relationship")
    rel.set("Id", new_rid)
    rel.set("Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image")
    rel.set("Target", f"media/{new_media}")
    with open(FIGURE4_PNG, "rb") as f:
        all_files[f"word/media/{new_media}"] = f.read()

    pw, ph = get_png_dimensions(FIGURE4_PNG)
    cx, cy = target_extent(pw, ph)
    if verbose:
        print(f"  New image: {new_rid} -> word/media/{new_media} "
              f"({pw}x{ph}px -> {cx/EMU_PER_IN:.2f}in x {cy/EMU_PER_IN:.2f}in)")

    # ------------------------------------------------------------------
    # 2. Clone Figure 3's caption+image paragraph, retarget the blip and
    #    extent, swap in the Figure 4 caption, and insert right after it.
    # ------------------------------------------------------------------
    fig3_idx = next(i for i, p in enumerate(paras) if paragraph_text(p).startswith("Figure 3."))
    fig4_para = copy.deepcopy(paras[fig3_idx])
    for blip in fig4_para.findall(".//a:blip", NS):
        blip.set(R_EMBED, new_rid)
    for ext in fig4_para.findall(".//wp:extent", NS):
        ext.set("cx", str(cx))
        ext.set("cy", str(cy))
    for ext in fig4_para.findall(".//a:ext", NS):
        if ext.get("cx") is not None:
            ext.set("cx", str(cx))
            ext.set("cy", str(cy))
    docPr = fig4_para.find(".//wp:docPr", NS)
    if docPr is not None:
        docPr.set("name", new_media)
    cNvPr = fig4_para.find(".//pic:cNvPr", NS)
    if cNvPr is not None:
        cNvPr.set("name", new_media)
    t_elems = fig4_para.findall(".//w:t", NS)
    caption_t_elems = [t for t in t_elems if (t.text or "").startswith("Figure 3.")]
    if caption_t_elems:
        caption_t_elems[0].text = FIGURE4_CAPTION
        for extra in caption_t_elems[1:]:
            extra.text = ""
    else:
        raise RuntimeError("Could not locate the Figure 3 caption text run to relabel.")

    body.insert(list(body).index(paras[fig3_idx]) + 1, fig4_para)

    if verbose:
        print(f"  Inserted Figure 4 caption+image after paragraph {fig3_idx} (Figure 3).")

    # ------------------------------------------------------------------
    # 3. Insert the Results-section discussion paragraph + "[Figure 4
    #    About Here]" placeholder, right after the Table 2 variety-
    #    effects paragraph and before the socio-demographic paragraph.
    # ------------------------------------------------------------------
    variety_idx = next(
        i for i, p in enumerate(paras)
        if paragraph_text(p).startswith("Table 2 examines the network predictors of everyday solitary leisure")
    )
    sociodem_idx = next(
        i for i, p in enumerate(paras)
        if paragraph_text(p).startswith("Looking at Model 2, we see that the effects of other socio-demographic")
    )
    if sociodem_idx != variety_idx + 1:
        raise RuntimeError(
            f"Expected the socio-demographic paragraph immediately after the variety paragraph "
            f"(got indices {variety_idx} and {sociodem_idx}); re-check anchors against a fresh download."
        )
    fig3_placeholder_idx = next(
        i for i, p in enumerate(paras) if paragraph_text(p).strip() == "[Figure 3 About Here]"
    )
    body_ppr_template = paras[variety_idx].find("w:pPr", NS)
    placeholder_ppr_template = paras[fig3_placeholder_idx].find("w:pPr", NS)

    new_paras = [
        make_body_paragraph(body_ppr_template, [(RESULTS_PARA, False)]),
        make_body_paragraph(None, []),  # blank spacer
        make_body_paragraph(placeholder_ppr_template, [(PLACEHOLDER_TEXT, False)]),
        make_body_paragraph(None, []),  # blank spacer
    ]
    insert_at = list(body).index(paras[variety_idx]) + 1
    for offset, np in enumerate(new_paras):
        body.insert(insert_at + offset, np)

    if verbose:
        print(f"  Inserted discussion paragraph + placeholder after paragraph {variety_idx} "
              f"(Table 2 variety-effects paragraph), before paragraph {sociodem_idx}.")

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
        print("Usage: insert_figure4.py <in.docx> <out.docx>")
        sys.exit(1)
    insert_figure4(sys.argv[1], sys.argv[2])
