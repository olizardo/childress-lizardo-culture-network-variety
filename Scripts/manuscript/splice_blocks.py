"""Replaces generated blocks in results_draft.tex.

Inputs (produced by Scripts/manuscript/build_measurement_blocks.R and build_structural_blocks.R):
  cache/manuscript/measurement_blocks.tex   full blocks for Tables 1, 2, 3, 4, A1, A2
                                            (Needspace, caption, table skeleton, rows, note)
  cache/manuscript/structural_blocks.tex    Tables 5 to 10, Figures 5 to 10, Table A3

Replacement is block-level for the measurement tables (from the \\Needspace line through the
table's Note paragraph) and for the structural region and Table A3. The script stops without
writing if any caption or note cannot be located. Re-runnable.
"""
PATH = "results_draft.tex"
import re

L = open(PATH).read().split("\n")


def sections(path):
    """Split a generated-fragments file on its '% === Key' markers."""
    out, cur = {}, None
    for line in open(path).read().split("\n"):
        if line.startswith("% === "):
            cur = line[6:].strip()
            out[cur] = []
        elif cur is not None:
            out[cur].append(line)
    return out


def trim_trailing_blank(lines):
    while lines and lines[-1] == "":
        lines.pop()
    return lines


meas = {k: trim_trailing_blank(v) for k, v in sections("cache/manuscript/measurement_blocks.tex").items()}
struct = open("cache/manuscript/structural_blocks.tex").read().split("\n")
struct = [l for l in struct if not l.startswith("% ")]

# Captions are matched on their cross-reference macro, not on a literal table number, so the
# splicer keeps working when a table is added or reordered. The literal forms are accepted so a
# draft written before the cross-reference macros can still be spliced once.
def pref(macro):
    return (macro, re.sub(r"\\\\[a-z]+\{([a-z-]+)\}", lambda m: m.group(1), macro).split(".")[0])


CAP = {"Table 1": "Table \\tabnum{activities-varimax}.",
       "Table 2": "Table \\tabnum{activities-cfa}.",
       "Table 3": "Table \\tabnum{activities-esem}.",
       "Table 4": "Table \\tabnum{ties-esem}.",
       "Table A1": "Table \\apptabnum{activities-oblimin}.",
       "Table A2": "Table \\apptabnum{ties-cfa}."}
LEGACY = {"Table \\tabnum{activities-varimax}.": "Table 1. Varimax Loadings",
          "Table \\tabnum{activities-cfa}.": "Table 2. Confirmatory and Exploratory",
          "Table \\tabnum{activities-esem}.": "Table 3. ESEM Loadings",
          "Table \\tabnum{ties-esem}.": "Table 4. Standardized Loadings",
          "Table \\apptabnum{activities-oblimin}.": "Table A1. Oblimin",
          "Table \\apptabnum{ties-cfa}.": "Table A2. Standardized Loadings and Antonym"}


def find_line(lines, macro, legacy):
    return next((i for i, l in enumerate(lines)
                 if l.startswith(macro) or l.startswith(legacy)), None)

# ---------------------------------------------------------------- measurement blocks (block-level)
for key, cap in CAP.items():
    gen = meas[key]
    c = find_line(L, cap, LEGACY[cap])
    if c is None:
        raise SystemExit(f"{key}: caption not found in draft; nothing written")
    start = c - 1 if c > 0 and L[c - 1].startswith("\\Needspace") else c
    end = next((i for i in range(c, len(L)) if L[i].startswith("Note.")), None)
    if end is None:
        raise SystemExit(f"{key}: note paragraph not found in draft; nothing written")
    L[start:end + 1] = gen

# ---------------------------------------------------------------- structural region
s = find_line(L, "Table \\tabnum{network-activity}.", "Table 5. Standardized Network-to-Activity")
while s > 0 and L[s - 1].startswith("\\Needspace"):
    s -= 1
e = next(i for i, l in enumerate(L) if l.startswith("\\subsection*{Appendix"))
# keep structural block only up to Table A3 (A3 handled separately)
a3_start = next(i for i, l in enumerate(struct) if l.startswith("Table \\apptabnum{rotation-halves}."))
if a3_start > 0 and struct[a3_start - 1].startswith("\\Needspace"):
    a3_start -= 1
# drop the "% BEGIN" marker line and leading blanks so repeated splices are idempotent
struct_main = [l for l in struct[:a3_start] if not l.startswith("% ")]
while struct_main and struct_main[0] == "":
    struct_main.pop(0)
while struct_main and struct_main[-1] == "":
    struct_main.pop()
struct_a3 = [l for l in struct[a3_start:] if not l.startswith("% ")]
while struct_a3 and struct_a3[0] == "":
    struct_a3.pop(0)
L = L[:s] + struct_main + L[e:]

# ---------------------------------------------------------------- Table A3 block
a3s = find_line(L, "Table \\apptabnum{rotation-halves}.", "Table A3.")
while a3s > 0 and L[a3s - 1].startswith("\\Needspace"):
    a3s -= 1
a4 = next((i for i, l in enumerate(L) if l.startswith("Table A4")), None)
if a4 is None:
    # Tables A4/A5 were dropped (November 11, 2026); the A3 block now runs to the end of the document
    a4 = next(i for i, l in enumerate(L) if l.startswith("\\end{document}"))
elif a4 > 0 and L[a4 - 1].startswith("\\Needspace"):
    a4 -= 1
L = L[:a3s] + struct_a3 + [""] + L[a4:]

open(PATH, "w").write("\n".join(L))
print("spliced: measurement blocks, structural region, Table A3")