"""Replaces the Tables A4 and A5 block in results_draft.tex with cache/manuscript/ols_appendix.tex.

Run from the project root after Scripts/manuscript/build_ols_appendix.R.
"""
path = "results_draft.tex"
src = open(path).read().split("\n")
new = open("cache/manuscript/ols_appendix.tex").read().rstrip("\n").split("\n")
start = next(i for i, l in enumerate(src) if l.startswith("Table A4"))
if start > 0 and src[start - 1].startswith("\\Needspace"):
    start -= 1
end = next(i for i, l in enumerate(src) if l.startswith("\\end{document}"))
out = src[:start] + new + [""] + src[end:]
open(path, "w").write("\n".join(out))
print("replaced lines", start + 1, "to", end, "with", len(new), "lines")
