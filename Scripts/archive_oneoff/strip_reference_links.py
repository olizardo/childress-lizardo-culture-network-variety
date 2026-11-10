#!/usr/bin/env python3
"""Scripts/strip_reference_links.py

One-off surgical edit: removes DOI strings and bare URLs from the tail of
each entry in the manuscript's References list (the section starting at the
paragraph whose text is exactly "References"). Every DOI/URL in this
document appears as plain trailing text in the final <w:t> run of its
reference paragraph (no <w:hyperlink> elements are present), so this script
locates that run and strips the trailing " doi:...", " doi: https://...",
or bare " https://..." substring, leaving the rest of the citation
(including the sentence-final period that already precedes the doi/url)
untouched. No other paragraphs, runs, or styling are modified.

Usage: python3 Scripts/strip_reference_links.py <in.docx> <out.docx>
"""
import re
import sys
import zipfile
import xml.etree.ElementTree as ET

W_NS = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
ns = {"w": W_NS}

DOI_OR_URL_RE = re.compile(
    r"\s*doi:\s*(?:https?://)?\S+\.?\s*$"  # "doi:10.xxx." or "doi: https://doi.org/...."
    r"|\s*https?://\S+\.?\s*$",             # bare "https://...." with no doi: prefix
    flags=re.IGNORECASE,
)


def strip_links(in_docx, out_docx):
    with zipfile.ZipFile(in_docx, "r") as zin:
        all_files = {item.filename: zin.read(item.filename) for item in zin.infolist()}

    ET.register_namespace("w", W_NS)
    doc_xml = all_files["word/document.xml"]
    tree = ET.fromstring(doc_xml)
    body = tree.find("w:body", ns)
    paras = list(body)

    ref_start = None
    for i, p in enumerate(paras):
        text = "".join(p.itertext()).strip()
        if text.lower() == "references":
            ref_start = i
            break
    if ref_start is None:
        print("ERROR: could not locate the 'References' heading paragraph.", file=sys.stderr)
        return 1

    # References run to the end of the document body (no Appendix/Table/Figure
    # section follows References in this manuscript).
    n_edited = 0
    for p in paras[ref_start + 1:]:
        runs = p.findall("w:r", ns)
        if not runs:
            continue
        # DOI/URL text is always in the final run's <w:t>.
        last_run = runs[-1]
        t_el = last_run.find("w:t", ns)
        if t_el is None or not t_el.text:
            continue
        new_text, n_sub = DOI_OR_URL_RE.subn("", t_el.text)
        if n_sub:
            t_el.text = new_text
            n_edited += 1

    all_files["word/document.xml"] = ET.tostring(tree, encoding="utf-8", xml_declaration=True)

    with zipfile.ZipFile(out_docx, "w", compression=zipfile.ZIP_DEFLATED) as zout:
        for fname, data in all_files.items():
            zout.writestr(fname, data)

    print(f"Stripped DOI/URL text from {n_edited} reference entries.")
    return 0


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python3 strip_reference_links.py <in.docx> <out.docx>", file=sys.stderr)
        sys.exit(1)
    sys.exit(strip_links(sys.argv[1], sys.argv[2]))
