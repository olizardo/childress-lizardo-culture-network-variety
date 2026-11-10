#!/usr/bin/env python3
"""
Scripts/revise_kmo_correction.py

One-time text correction (September 2026): the manuscript reported
KMO = .926 (weak ties) and .922 (strong ties), but the actual,
reproducible pipeline (Scripts/01_prepare_data.R, via
psych::KMO(cor(item_df))) computes .938 and .935 respectively. This was
investigated and is NOT an artifact of the varimax->oblimin rotation
switch (KMO is computed on the raw correlation matrix before any
rotation is applied and is mathematically identical under either
rotation), NOT a missing-data handling issue (both weak-tie and
strong-tie item batteries have zero missing values across all N=1258
respondents, so complete.obs/pairwise.complete.obs are identical), and
NOT a Pearson-vs-polychoric correlation choice (polychoric-based KMO on
the same items is .9385, i.e. the same value to 3 decimals). No
alternative item set or correlation method reproduces .926/.922 from
any data file present in this repository (including the "full" 179-
column raw export, which -- like the committed minimal extract -- lacks
the v8/duration columns that would be needed to reproduce the paper's
duplicate/speeder-drop step). The most likely explanation is that these
two figures are simply stale, carried over from an earlier draft of the
data or analysis and never recomputed. Since the current pipeline is
the reproducible source of truth, this script corrects the manuscript
text to match it.

Uses Scripts/docx_text_edits.py's substring-splice mode so only the
run(s) containing the two numbers are touched.

Usage: revise_kmo_correction.py <in.docx> <out.docx>
"""

import sys
sys.path.insert(0, "Scripts")
from docx_text_edits import revise_docx, RevisionError

SUBSTRING_EDITS = [
    (
        "KMOs = .926 for weak ties and .922 for strong ties",
        "KMOs = .938 for weak ties and .935 for strong ties",
    ),
]

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: revise_kmo_correction.py <in.docx> <out.docx>")
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
