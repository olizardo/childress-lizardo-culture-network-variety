#!/usr/bin/env python3
"""
Scripts/revise_table3_to_A3_references.py

One-time text correction (September 2026): the author moved the
residual/DIY-practical leisure discriminant-validity table into the
Appendix and relabeled its caption "Table A3. Coefficient Estimates of
the Effects of Strong and Weak Tie Network Variety and Composition on
the Residual Factor." (it previously sat in the main-text "Tables"
section as "Table 3"). Three in-text references in the Results section
still said "Table 3" / "[Table 3 About Here]" and needed to be updated
to match the new appendix numbering:

  1. "...residual factor from our activities battery (Table 3)."
     -> "...(Table A3)."
  2. "...Models 3 or 4 of Table 3, and formal Wald tests..."
     -> "...Table A3, and formal Wald tests..."
  3. "[Table 3 About Here]" placeholder -> "[Table A3 About Here]"

No other in-text cross-references needed updating: the appendix's other
relabeling (the correlation heat map moving from "Figure A4" to
"Figure A1") has no narrative in-text mention anywhere in the document
(it is only ever referenced via its own caption), and "Table A1" /
"Table A2" narrative mentions already matched their captions before this
edit.

Uses Scripts/docx_text_edits.py's substring-splice mode so only the
run(s) containing "Table 3" are touched; all surrounding text and
formatting is left untouched.

Usage: revise_table3_to_A3_references.py <in.docx> <out.docx>
"""

import sys
sys.path.insert(0, "Scripts")
from docx_text_edits import revise_docx, RevisionError

SUBSTRING_EDITS = [
    (
        "factor from our activities battery (Table 3). This factor loads",
        "factor from our activities battery (Table A3). This factor loads",
    ),
    (
        "approaches statistical significance in Models 3 or 4 of Table 3, and formal Wald tests",
        "approaches statistical significance in Models 3 or 4 of Table A3, and formal Wald tests",
    ),
    (
        "[Table 3 About Here]",
        "[Table A3 About Here]",
    ),
]

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: revise_table3_to_A3_references.py <in.docx> <out.docx>")
        sys.exit(1)
    try:
        n_full, n_sub = revise_docx(
            sys.argv[1], sys.argv[2],
            full_paragraph_edits=[],
            substring_edits=SUBSTRING_EDITS,
        )
    except RevisionError as e:
        print(f"[ERROR] {e}")
        sys.exit(1)
    print(f"Applied {n_sub} substring edit(s). Wrote {sys.argv[2]}.")
