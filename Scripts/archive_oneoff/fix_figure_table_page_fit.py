#!/usr/bin/env python3
"""
Scripts/fix_figure_table_page_fit.py

One-time (but re-runnable/idempotent-ish) layout fix so that every embedded
figure/table PNG in the "Practice and Networks" Google Doc fits, together
with its caption, on a single portrait page (8.5x11in, 1in margins -> 9.0in
usable height).

Three coordinated changes, all DOM-surgical (zero prose edits):

1. Height-capped resizing: every image is resized to fit within a max
   height budget (leaving headroom for its caption, and for a section
   heading when one shares the page), keeping the full 6.5in printable
   width whenever that already fits under the cap, and narrowing the width
   (preserving aspect ratio) only for images that would otherwise be too
   tall (Table 1, Table 2, Table A2). Horizontal positioning is normalized
   to centered-on-margin for every image so narrowed images aren't left
   flush against the left margin.
2. Explicit page breaks (`<w:pageBreakBefore/>`) inserted on the caption
   paragraphs that were not already isolated onto their own page: Table 2,
   the APPENDIX heading (which precedes Table A1), Figure A4, and Table A5.
3. Trimming redundant blank spacer paragraphs (empty paragraphs with no
   text, drawing, or break) down to at most one per run, recovering
   vertical space that was padding around images at their old, taller
   sizes.

Usage: fix_figure_table_page_fit.py <in.docx> <out.docx>
"""

import struct
import sys
import xml.etree.ElementTree as ET
import zipfile

NS = {
    "w": "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
    "a": "http://schemas.openxmlformats.org/drawingml/2006/main",
    "wp": "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing",
    "r": "http://schemas.openxmlformats.org/officeDocument/2006/relationships",
}
W = NS["w"]
WP = NS["wp"]
R_EMBED = "{%s}embed" % NS["r"]

# Re-registering the document's namespace prefixes before serialization
# keeps ET.tostring() from renaming them to auto-generated ns0:/ns1:/...
# prefixes. That's technically valid XML, but pandoc's docx reader (used
# for validation elsewhere in this project's tooling) fails to parse it --
# see Scripts/docx_text_edits.py's module docstring for the full story.
for _prefix, _uri in [
    ("w", NS["w"]),
    ("w14", "http://schemas.microsoft.com/office/word/2010/wordml"),
    ("r", NS["r"]),
    ("wp", NS["wp"]),
    ("a", NS["a"]),
    ("pic", "http://schemas.openxmlformats.org/drawingml/2006/picture"),
]:
    ET.register_namespace(_prefix, _uri)

EMU_PER_IN = 914400
W_MAX = int(6.5 * EMU_PER_IN)   # full printable width, Letter/1in margins
H_MAX = int(7.2 * EMU_PER_IN)   # leaves ~1.8in headroom per page for a
                                 # heading, caption, and blank spacer lines

# caption text prefix (log only) -> (relationship ID, source PNG)
#
# NOTE (September 2026): re-derived from scratch by content inspection
# (not adjacency -- see AGENTS.md Section 4) after a fresh download
# showed Google Docs had reassigned every single rId since the last
# edit, independent of the Table A2/A3 removal or Figure 4 insertion
# content changes. Table A2 (activity item loadings) and Table A3
# (weak-/strong-tie item loadings) were removed from the live document
# -- see Scripts/remove_tableA2_A3_renumber.py -- because they
# duplicated the loading matrices already shown as heat maps in Figure 1
# and Figure 2. The surviving appendix table (formerly Table A5) was
# renumbered to Table A2; its asset was renamed to
# tableA2_variety_validation.png. Figure 4 was added via
# Scripts/insert_figure4.py.
#
# Re-verify this map against a fresh download before every future run --
# do not assume it is still current even if nothing was edited since.
IMAGE_MAP = {
    "Figure 1":  ("rId9",  "Plots/fig1_activity_loadings_heatmap.png"),
    "Figure 2":  ("rId10", "Plots/fig2_network_loadings_heatmap.png"),
    "Figure 3":  ("rId11", "Plots/fig3_variety_margins_plot.png"),
    "Figure 4":  ("rId12", "Plots/fig4_leisure_variety_margins_plot.png"),
    "Table 1":   ("rId13", "Plots/table1_arts_participation.png"),
    "Table 2":   ("rId14", "Plots/table2_solitary_leisure.png"),
    "Table A1":  ("rId15", "Plots/tableA1_descriptives.png"),
    "Table A4":  ("rId16", "Plots/figA1_predictor_correlation_heatmap.png"),
    "Table A2 (fka A5)": ("rId17", "Plots/tableA2_variety_validation.png"),
    "Table 3":   ("rId18", "Plots/table3_residual_leisure.png"),
}

# Exact caption-paragraph text (or unique prefix) that needs an explicit
# pageBreakBefore so it starts a fresh page.
PAGE_BREAK_BEFORE_PREFIXES = [
    "Figure 4. Weak and Strong Tie Network Variety on Solitary Leisure",
    "Table 2. Coefficient Estimates",
    "Table 3. Coefficient Estimates",
    "APPENDIX",
    "Figure A4. Correlation Heat Map",
    "Table A2. Correlation Matrix of Factor Scores",
]


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


def set_centered_position_h(anchor):
    ph = anchor.find("wp:positionH", NS)
    if ph is None:
        return
    for child in list(ph):
        ph.remove(child)
    ph.set("relativeFrom", "margin")
    align = ET.SubElement(ph, "{%s}align" % WP)
    align.text = "center"


def paragraph_text(p):
    return "".join(t.text or "" for t in p.findall(".//w:t", NS)).strip()


def is_blank_spacer(p):
    if paragraph_text(p):
        return False
    if p.findall(".//w:drawing", NS):
        return False
    if p.findall(".//w:br", NS):
        return False
    return True


def add_page_break_before(p):
    pPr = p.find("w:pPr", NS)
    if pPr is None:
        pPr = ET.Element("{%s}pPr" % W)
        p.insert(0, pPr)
    if pPr.find("w:pageBreakBefore", NS) is not None:
        return False
    pbb = ET.Element("{%s}pageBreakBefore" % W)
    # Schema order: pStyle, keepNext, keepLines, pageBreakBefore, ...
    insert_at = 0
    for i, child in enumerate(pPr):
        tag = child.tag.split("}")[-1]
        if tag in ("pStyle", "keepNext", "keepLines"):
            insert_at = i + 1
        else:
            break
    pPr.insert(insert_at, pbb)
    return True


def fix_layout(in_docx, out_docx, verbose=True):
    with zipfile.ZipFile(in_docx, "r") as zin:
        all_files = {item.filename: zin.read(item.filename) for item in zin.infolist()}

    rels_root = ET.fromstring(all_files["word/_rels/document.xml.rels"])
    rid_to_target = {el.get("Id"): el.get("Target") for el in rels_root if el.get("Id")}

    doc_tree = ET.fromstring(all_files["word/document.xml"])
    body = doc_tree.find("w:body", NS)

    # 1. Resize + recenter every mapped image.
    for caption, (rid, png_path) in IMAGE_MAP.items():
        target = rid_to_target.get(rid)
        if target is None:
            print(f"  [WARN] {caption}: relationship {rid} not found; skipped.")
            continue
        media_path = "word/" + target
        with open(png_path, "rb") as f:
            all_files[media_path] = f.read()

        dims = get_png_dimensions(png_path)
        if dims is None:
            print(f"  [WARN] {caption}: could not read PNG dimensions.")
            continue
        pw, ph = dims
        cx, cy = target_extent(pw, ph)

        n_updated = 0
        for drawing in doc_tree.findall(".//w:drawing", NS):
            blips = drawing.findall(".//a:blip", NS)
            if not any(b.attrib.get(R_EMBED) == rid for b in blips):
                continue
            for ext in drawing.findall(".//wp:extent", NS):
                ext.set("cx", str(cx))
                ext.set("cy", str(cy))
            for ext in drawing.findall(".//a:ext", NS):
                if ext.get("cx") is not None:
                    ext.set("cx", str(cx))
                    ext.set("cy", str(cy))
            anchor = drawing.find("wp:anchor", NS)
            if anchor is not None:
                set_centered_position_h(anchor)
            # Clear any leftover rotation transform (e.g. a stale 270deg
            # rot="16200000" on Figure A4 left over from before it was a
            # landscape-shaped heat map) so images render right-side up.
            for xfrm in drawing.findall(".//a:xfrm", NS):
                if xfrm.get("rot") is not None:
                    del xfrm.attrib["rot"]
            n_updated += 1

        if verbose:
            print(f"  {caption}: {pw}x{ph}px -> {cx/EMU_PER_IN:.2f}in x "
                  f"{cy/EMU_PER_IN:.2f}in ({n_updated} drawing(s))")

    # 2. Explicit page breaks so every item starts its own page.
    paras = list(body.findall("w:p", NS))
    for p in paras:
        text = paragraph_text(p)
        for prefix in PAGE_BREAK_BEFORE_PREFIXES:
            if text.startswith(prefix):
                added = add_page_break_before(p)
                if verbose:
                    print(f"  pageBreakBefore {'added' if added else 'already present'}: {text[:60]!r}")
                break

    # 3. Trim redundant blank spacer paragraphs (keep at most one per run),
    # restricted to the Figures/Tables/Appendix block (from the "Figures"
    # section heading to the end of the document) so nothing in the
    # Introduction/Methods/Results/References prose is touched.
    paras = list(body.findall("w:p", NS))  # re-fetch (unchanged by step 2)
    start = next((i for i, p in enumerate(paras) if paragraph_text(p) == "Figures"), None)
    if start is None:
        print("  [WARN] Could not locate 'Figures' heading; skipping blank-spacer trim.")
        scope = []
    else:
        scope = paras[start:]

    to_remove = []
    run = []
    for p in scope:
        if is_blank_spacer(p):
            run.append(p)
        else:
            if len(run) > 1:
                to_remove.extend(run[1:])
            run = []
    if len(run) > 1:
        to_remove.extend(run[1:])
    for p in to_remove:
        body.remove(p)
    if verbose:
        print(f"  Removed {len(to_remove)} redundant blank spacer paragraph(s).")

    all_files["word/document.xml"] = ET.tostring(doc_tree, encoding="utf-8", xml_declaration=True)

    with zipfile.ZipFile(out_docx, "w", compression=zipfile.ZIP_DEFLATED) as zout:
        for fname, data in all_files.items():
            zout.writestr(fname, data)


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: fix_figure_table_page_fit.py <in.docx> <out.docx>")
        sys.exit(1)
    fix_layout(sys.argv[1], sys.argv[2])
