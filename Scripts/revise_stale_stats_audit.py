#!/usr/bin/env python3
"""
Scripts/revise_stale_stats_audit.py

One-time correction (September 2026): a systematic audit of every
quantitative claim in the manuscript against the current, reproducible
oblimin-based pipeline turned up three more stale/inaccurate statistics
that were not caught during the varimax -> oblimin revision pass
(because that pass focused on the composition-term coefficients
specifically, not every numeric claim in the surrounding prose):

  1. Table 2 discussion (Results): the claimed standard-deviation ranges
     for the weak-/strong-tie variety effects on solitary leisure across
     Models 1-4 ("0.15 to 0.20" and ".16 to .20") no longer match the
     actual coefficients under oblimin (weak_variety ranges 0.126-0.213;
     strong_variety ranges -0.081 to -0.170 in magnitude).
  2. Table 2 discussion: childhood arts exposure was claimed to become
     "non-significant" in Model 4, but it remains significant at
     conventional levels (b = .042 -> .042, p = .010 -> .024 from Model
     2 to Model 4) -- it attenuates but does not lose significance.
  3. Table 1 discussion: the race/ethnicity sentence lumps Hispanic/
     Latine respondents in with AAPI and multiracial respondents as if
     all three contrasts with White respondents were significant, but
     the Hispanic/Latine contrast is only marginal (p = .072) while
     AAPI (p < .001) and multiracial (p = .005) are clearly significant.

Uses Scripts/docx_text_edits.py's substring-splice mode so only the
run(s) containing the affected clauses are touched.

Usage: revise_stale_stats_audit.py <in.docx> <out.docx>
"""

import sys
sys.path.insert(0, "Scripts")
from docx_text_edits import revise_docx, RevisionError

SUBSTRING_EDITS = [
    (
        "and is higher among self-identified white respondents than "
        "among Hispanic, AAPI, or multiracial respondents.",
        "and is higher among self-identified white respondents than "
        "among AAPI (p < .001) or multiracial respondents (p = .005); "
        "the corresponding contrast with Hispanic/Latine respondents is "
        "in the same direction but only marginally significant (p = .07).",
    ),
    (
        "as does exposure to childhood arts (although this effect "
        "becomes non-significant once network composition variables "
        "are introduced in Model 4).",
        "as does exposure to childhood arts, whose effect attenuates "
        "somewhat but remains statistically significant once network "
        "composition variables are introduced in Model 4 (b = .04, "
        "p = .02).",
    ),
    (
        "corresponds to an increase of approximately 0.15 to 0.20 "
        "standard deviations in solitary leisure. In contrast, greater "
        "strong-tie variety is associated with a roughly .16 to .20 "
        "standard deviation decrease in the frequency of engagement in "
        "everyday leisure activities.",
        "corresponds to an increase of approximately 0.13 to 0.21 "
        "standard deviations in solitary leisure across the four model "
        "specifications. In contrast, greater strong-tie variety is "
        "associated with a roughly 0.08 to 0.17 standard deviation "
        "decrease in the frequency of engagement in everyday leisure "
        "activities.",
    ),
]

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: revise_stale_stats_audit.py <in.docx> <out.docx>")
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
