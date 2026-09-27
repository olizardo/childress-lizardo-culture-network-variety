#!/usr/bin/env python3
"""
Scripts/revert_varimax_images.py

One-time image-replacement pass swapping in the reverted-varimax Plots/*.png
into the "Practice and Networks" live Google Doc. The rId -> caption mapping
below was re-derived fresh (per AGENTS.md's re-verification protocol -- the
memory-recorded rId table is explicitly documented as stale-by-default) by
extracting ALL caption paragraphs and ALL drawing paragraphs from a fresh
drive_download() in document order and pairing them by rank (both lists have
exactly 10 entries in the same relative sequence), then visually confirming
three of the ten pairings (Figure 1, Table 1, Table A3) by opening the actual
embedded media files.

Also updates <wp:extent>/<a:ext> to the new PNG's native aspect ratio at a
fixed 6.5in width (or narrower per the project's page-fit convention for the
three tall regression-table images), matching Scripts/fix_figure_table_page_fit.py.

Usage: revert_varimax_images.py <in.docx> <out.docx>
"""

import struct
import sys
import zipfile
import xml.etree.ElementTree as ET

NS = {
    "w": "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
    "a": "http://schemas.openxmlformats.org/drawingml/2006/main",
    "r": "http://schemas.openxmlformats.org/officeDocument/2006/relationships",
    "wp": "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing",
}
for prefix, uri in [
    ("w", NS["w"]), ("w14", "http://schemas.microsoft.com/office/word/2010/wordml"),
    ("r", NS["r"]), ("wp", NS["wp"]), ("a", NS["a"]),
    ("pic", "http://schemas.openxmlformats.org/drawingml/2006/picture"),
]:
    ET.register_namespace(prefix, uri)

EMU_PER_IN = 914400
W_MAX_IN = 6.5
H_MAX_IN = 7.2

# rId -> replacement PNG, established by rank-order pairing (see docstring)
IMAGE_MAP = {
    "rId9": "Plots/fig1_activity_loadings_heatmap.png",
    "rId10": "Plots/fig2_network_loadings_heatmap.png",
    "rId11": "Plots/fig3_variety_margins_plot.png",
    "rId12": "Plots/fig4_leisure_variety_margins_plot.png",
    "rId13": "Plots/table1_arts_participation.png",
    "rId14": "Plots/table2_solitary_leisure.png",
    "rId15": "Plots/figA1_predictor_correlation_heatmap.png",
    "rId16": "Plots/tableA1_descriptives.png",
    "rId17": "Plots/tableA2_variety_validation.png",
    "rId18": "Plots/table3_residual_leisure.png",
}


def get_png_dimensions(path):
    with open(path, "rb") as f:
        data = f.read(24)
    if len(data) >= 24 and data.startswith(b"\x89PNG\r\n\x1a\n"):
        return struct.unpack(">II", data[16:24])
    return 1950, 1200


def revise(in_docx, out_docx):
    with zipfile.ZipFile(in_docx, "r") as zin:
        all_files = {item.filename: zin.read(item.filename) for item in zin.infolist()}
        rels_bytes = all_files["word/_rels/document.xml.rels"]

    rels_root = ET.fromstring(rels_bytes)
    rid_to_target = {e.get("Id"): e.get("Target") for e in rels_root if e.get("Id")}

    doc_tree = ET.fromstring(all_files["word/document.xml"])
    body = doc_tree.find("w:body", NS)

    n_replaced = 0
    for rid, png_path in IMAGE_MAP.items():
        target = rid_to_target.get(rid)
        if target is None:
            print(f"  [WARN] {rid} not found in rels; skipping {png_path}")
            continue
        media_path = "word/" + target
        with open(png_path, "rb") as f:
            png_bytes = f.read()
        all_files[media_path] = png_bytes

        pw, ph = get_png_dimensions(png_path)
        w_in = W_MAX_IN
        h_in = w_in * (ph / pw)
        if h_in > H_MAX_IN:
            h_in = H_MAX_IN
            w_in = h_in * (pw / ph)
        cx = round(w_in * EMU_PER_IN)
        cy = round(h_in * EMU_PER_IN)

        touched = 0
        for blip in body.findall(f".//a:blip[@r:embed='{rid}']", NS):
            drawing = None
            parent_map = {c: p for p in body.iter() for c in p}
            node = blip
            while node in parent_map:
                node = parent_map[node]
                if node.tag.endswith("}drawing"):
                    drawing = node
                    break
            if drawing is None:
                continue
            for ext in drawing.findall(".//wp:extent", NS):
                ext.set("cx", str(cx))
                ext.set("cy", str(cy))
            for ext in drawing.findall(".//a:ext", NS):
                ext.set("cx", str(cx))
                ext.set("cy", str(cy))
            for xfrm in drawing.findall(".//a:xfrm", NS):
                if "rot" in xfrm.attrib:
                    del xfrm.attrib["rot"]
            touched += 1
        print(f"  [image swap] {rid} -> {png_path} ({pw}x{ph}px -> {w_in:.2f}in x {h_in:.2f}in), {touched} drawing(s) updated")
        n_replaced += 1

    all_files["word/document.xml"] = ET.tostring(doc_tree, encoding="utf-8", xml_declaration=True)

    with zipfile.ZipFile(out_docx, "w", compression=zipfile.ZIP_DEFLATED) as zout:
        for fname, data in all_files.items():
            zout.writestr(fname, data)
    return n_replaced


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: revert_varimax_images.py <in.docx> <out.docx>")
        sys.exit(1)
    n = revise(sys.argv[1], sys.argv[2])
    print(f"Replaced {n}/{len(IMAGE_MAP)} images. Wrote {sys.argv[2]}.")
