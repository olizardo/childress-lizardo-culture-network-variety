#!/usr/bin/env python3
"""
Scripts/sync_manuscript.py

Surgical, in-place OpenXML image replacement for the "Practice and
Networks" Google Doc. Every table and figure in this manuscript is
embedded as a plain PNG screenshot (there are no native Word <w:tbl>
tables to inject into), so the entire update consists of replacing the
bytes of the target media file(s) referenced by specific relationship
IDs, and synchronizing the <wp:extent>/<a:ext> dimensions so Google Docs
does not stretch the new image into the old image's box.

This performs ZERO text edits except one deliberate, explicitly-approved
caption change: "Table A4. Correlation Matrix..." -> "Figure A4.
Correlation Heat Map..." (the underlying asset changed from a table to a
heat map at the user's request). All other captions, paragraph
properties, and surrounding prose are left completely untouched.

Mapping of caption -> relationship ID was established by downloading the
live document and (a) walking word/document.xml to find the <w:drawing>
nearest each caption paragraph, then (b) visually inspecting each
word/media/imageN.png to confirm content, since floating (anchored)
images do not always sit in natural XML sibling order relative to their
caption text. This mapping is specific to the current live document and
should be re-verified with inspect_docx_structure() if the author adds,
removes, or reorders any tables/figures directly in Google Docs.
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
R_EMBED = "{http://schemas.openxmlformats.org/officeDocument/2006/relationships}embed"

# caption text (for the human-readable log only) -> (relationship ID, new PNG path)
#
# NOTE (September 2026): re-derived from scratch by content inspection
# (not adjacency -- see the module docstring) after a fresh download
# showed Google Docs had reassigned every single rId since the last
# edit, independent of the Table A2/A3 removal or Figure 4 insertion
# content changes. The original Table A2 (activity item loadings) and
# Table A3 (weak-/strong-tie item loadings) were removed from the live
# document -- see Scripts/remove_tableA2_A3_renumber.py -- because they
# duplicated the loading matrices already shown as heat maps in Figure 1
# and Figure 2. The surviving appendix table (formerly Table A5) was
# renumbered to Table A2 to close the gap; its asset was renamed from
# tableA5_variety_validation.png to tableA2_variety_validation.png.
# Figure 4 was added via Scripts/insert_figure4.py.
#
# Re-verify this map against a fresh download before every future sync --
# do not assume it is still current even if nothing was edited since.
IMAGE_MAP = {
    "Figure 1":  ("rId9",  "Plots/fig1_activity_loadings_heatmap.png"),
    "Figure 2":  ("rId10", "Plots/fig2_network_loadings_heatmap.png"),
    "Figure 3":  ("rId11", "Plots/fig3_variety_margins_plot.png"),
    "Figure 4":  ("rId12", "Plots/fig4_leisure_variety_margins_plot.png"),
    "Table 1":   ("rId13", "Plots/table1_arts_participation.png"),
    "Table 2":   ("rId14", "Plots/table2_solitary_leisure.png"),
    "Table A1":  ("rId15", "Plots/tableA1_descriptives.png"),
    "Table A4":  ("rId16", "Plots/figA1_predictor_correlation_heatmap.png"),  # table -> heat map
    "Table A2 (fka A5)": ("rId17", "Plots/tableA2_variety_validation.png"),
    "Table 3":   ("rId18", "Plots/table3_residual_leisure.png"),
}

CAPTION_EDIT = {
    "old": "Table A4. Correlation Matrix of Continuous and Ordinal Predictor Variables.",
    "new": "Figure A4. Correlation Heat Map of Continuous and Ordinal Predictor Variables.",
}


def get_png_dimensions(path):
    with open(path, "rb") as f:
        data = f.read(24)
        if len(data) >= 24 and data.startswith(b"\x89PNG\r\n\x1a\n"):
            return struct.unpack(">II", data[16:24])
    return None


def find_relationship_targets(rels_root):
    return {
        el.get("Id"): el.get("Target")
        for el in rels_root
        if el.get("Id")
    }


def replace_images(in_docx, out_docx, image_map, edit_caption=True, verbose=True):
    with zipfile.ZipFile(in_docx, "r") as zin:
        all_files = {item.filename: zin.read(item.filename) for item in zin.infolist()}

    rels_root = ET.fromstring(all_files["word/_rels/document.xml.rels"])
    rid_to_target = find_relationship_targets(rels_root)

    doc_tree = ET.fromstring(all_files["word/document.xml"])
    body = doc_tree.find("w:body", NS)

    # 1. Swap image bytes + sync <wp:extent>/<a:ext> for each mapped rId.
    for caption, (rid, new_png) in image_map.items():
        target = rid_to_target.get(rid)
        if target is None:
            print(f"  [WARN] {caption}: relationship {rid} not found in document; skipped.")
            continue
        media_path = "word/" + target
        with open(new_png, "rb") as f:
            all_files[media_path] = f.read()

        dims = get_png_dimensions(new_png)
        if dims is None:
            print(f"  [WARN] {caption}: could not read PNG dimensions for {new_png}.")
            continue
        pw, ph = dims
        cx = 5943600  # 6.5 in in EMUs (full printable width)
        cy = int(round(cx * (ph / pw)))

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
            n_updated += 1

        if verbose:
            print(f"  {caption}: {target} <- {new_png} ({pw}x{ph}; {n_updated} drawing(s) resized)")

    # 2. Single deliberate caption text edit: Table A4 -> Figure A4.
    if edit_caption:
        edited = False
        for p in body.findall(".//w:p", NS):
            text = "".join(t.text or "" for t in p.findall(".//w:t", NS))
            if text.strip() == CAPTION_EDIT["old"]:
                runs = p.findall(".//w:r", NS)
                # Put the full new caption in the first run, blank the rest,
                # preserving each run's original rPr (formatting) untouched.
                t_elems = [r.find("w:t", NS) for r in runs if r.find("w:t", NS) is not None]
                if t_elems:
                    t_elems[0].text = CAPTION_EDIT["new"]
                    for extra in t_elems[1:]:
                        extra.text = ""
                    edited = True
        if verbose:
            print(f"  Caption edit applied: {edited}")

    all_files["word/document.xml"] = ET.tostring(doc_tree, encoding="utf-8", xml_declaration=True)

    with zipfile.ZipFile(out_docx, "w", compression=zipfile.ZIP_DEFLATED) as zout:
        for fname, data in all_files.items():
            zout.writestr(fname, data)


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: sync_manuscript.py <in.docx> <out.docx>")
        sys.exit(1)

    replace_images(sys.argv[1], sys.argv[2], IMAGE_MAP)
