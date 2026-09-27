#!/usr/bin/env python3
"""
Scripts/build_ssr_submission_docs.py

One-off script producing three local .docx deliverables for a
double-blind journal submission (Social Science Research), built by
surgical OOXML slicing of a fresh Google Drive download of the
"Practice and Networks" manuscript -- never by Pandoc re-compilation,
so every retained paragraph keeps its live document formatting.

Outputs (written to SSR-submission/):
  1. manuscript_anonymized.docx -- full manuscript with the title-page
     author/affiliation block (and its "Coauthorship is equal."
     footnote) removed. Body, abstract, keywords, tables, figures, and
     references are otherwise untouched.
  2. title_page.docx -- title, author names, affiliations, and the
     coauthorship note, standalone.
  3. abstract_page.docx -- title, abstract heading, abstract text, and
     keywords, standalone (no author identification).

Usage:
    python3 Scripts/build_ssr_submission_docs.py <fresh_live.docx>
"""

import copy
import sys
import zipfile
import xml.etree.ElementTree as ET

NS_W = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
NS = {"w": NS_W}

_NAMESPACES = [
    ("w", NS_W),
    ("w14", "http://schemas.microsoft.com/office/word/2010/wordml"),
    ("r", "http://schemas.openxmlformats.org/officeDocument/2006/relationships"),
    ("wp", "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"),
    ("a", "http://schemas.openxmlformats.org/drawingml/2006/main"),
    ("pic", "http://schemas.openxmlformats.org/drawingml/2006/picture"),
    ("mc", "http://schemas.openxmlformats.org/markup-compatibility/2006"),
]
for _prefix, _uri in _NAMESPACES:
    ET.register_namespace(_prefix, _uri)


def para_text(p):
    return "".join(t.text or "" for t in p.findall(".//w:t", NS))


def load_files(path):
    with zipfile.ZipFile(path, "r") as z:
        return {item.filename: z.read(item.filename) for item in z.infolist()}


def write_docx(all_files, document_xml_bytes, out_path):
    all_files = dict(all_files)
    all_files["word/document.xml"] = document_xml_bytes
    with zipfile.ZipFile(out_path, "w", compression=zipfile.ZIP_DEFLATED) as z:
        for fname, data in all_files.items():
            z.writestr(fname, data)


def get_body(xml_bytes):
    tree = ET.fromstring(xml_bytes)
    body = tree.find("w:body", NS)
    return tree, body


def build_manuscript_anonymized(all_files):
    tree, body = get_body(all_files["word/document.xml"])
    children = list(body)
    assert para_text(children[0]).startswith("Beyond Network Variety"), \
        "title paragraph (index 0) not found where expected"
    assert para_text(children[2]) == "Omar Lizardo", \
        "author paragraph (index 2) not found where expected"
    assert para_text(children[13]) == "Abstract", \
        "Abstract heading (index 13) not found where expected"
    # Remove the author/affiliation block (indices 1-9 inclusive):
    # blank, "Omar Lizardo" (+ Coauthorship-equal footnote ref),
    # affiliation, blank, "Clayton Childress", affiliation, blank x3.
    for el in children[1:10]:
        body.remove(el)
    return tree


def build_title_page(all_files):
    tree, body = get_body(all_files["word/document.xml"])
    children = list(body)
    assert para_text(children[0]).startswith("Beyond Network Variety"), \
        "title paragraph (index 0) not found where expected"
    assert para_text(children[6]) == "University of British Columbia", \
        "second affiliation paragraph (index 6) not found where expected"
    final_sectpr = children[-1]
    assert final_sectpr.tag == f"{{{NS_W}}}sectPr", "expected trailing sectPr"
    # Indices 0-8 already include the "Omar Lizardo" paragraph's own
    # footnoteReference (id 0, "Coauthorship is equal."), which will
    # render as a normal bottom-of-page footnote -- no need to add a
    # second, redundant explicit note paragraph.
    keep = children[0:9]  # title .. blank line after 2nd affiliation
    for el in list(body):
        body.remove(el)
    for el in keep:
        body.append(el)
    body.append(copy.deepcopy(final_sectpr))
    return tree


def build_abstract_page(all_files):
    tree, body = get_body(all_files["word/document.xml"])
    children = list(body)
    assert para_text(children[0]).startswith("Beyond Network Variety"), \
        "title paragraph (index 0) not found where expected"
    assert para_text(children[13]) == "Abstract", \
        "Abstract heading (index 13) not found where expected"
    assert para_text(children[16]).startswith("Keywords:"), \
        "Keywords paragraph (index 16) not found where expected"
    final_sectpr = children[-1]
    assert final_sectpr.tag == f"{{{NS_W}}}sectPr", "expected trailing sectPr"
    keep = [children[0], children[13], children[14], children[15], children[16]]
    for el in list(body):
        body.remove(el)
    for el in keep:
        body.append(el)
    body.append(copy.deepcopy(final_sectpr))
    return tree


def main():
    if len(sys.argv) != 2:
        print("Usage: build_ssr_submission_docs.py <fresh_live.docx>")
        sys.exit(1)
    src = sys.argv[1]
    all_files = load_files(src)

    import os
    outdir = "SSR-submission"
    os.makedirs(outdir, exist_ok=True)

    tree = build_manuscript_anonymized(all_files)
    write_docx(all_files, ET.tostring(tree, encoding="utf-8", xml_declaration=True),
               os.path.join(outdir, "manuscript_anonymized.docx"))
    print(f"Wrote {outdir}/manuscript_anonymized.docx")

    tree = build_title_page(all_files)
    write_docx(all_files, ET.tostring(tree, encoding="utf-8", xml_declaration=True),
               os.path.join(outdir, "title_page.docx"))
    print(f"Wrote {outdir}/title_page.docx")

    tree = build_abstract_page(all_files)
    write_docx(all_files, ET.tostring(tree, encoding="utf-8", xml_declaration=True),
               os.path.join(outdir, "abstract_page.docx"))
    print(f"Wrote {outdir}/abstract_page.docx")


if __name__ == "__main__":
    main()
