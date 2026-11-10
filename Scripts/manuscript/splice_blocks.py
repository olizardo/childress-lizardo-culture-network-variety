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

CAP = {"Table 1": "Table 1. Varimax Loadings",
       "Table 2": "Table 2. Confirmatory and Exploratory",
       "Table 3": "Table 3. ESEM Loadings",
       "Table 4": "Table 4. Standardized Loadings",
       "Table A1": "Table A1. Oblimin",
       "Table A2": "Table A2. Standardized Loadings and Antonym"}

# ---------------------------------------------------------------- measurement blocks (block-level)
for key, cap in CAP.items():
    gen = meas[key]
    c = next((i for i, l in enumerate(L) if l.startswith(cap)), None)
    if c is None:
        raise SystemExit(f"{key}: caption not found in draft; nothing written")
    start = c - 1 if c > 0 and L[c - 1].startswith("\\Needspace") else c
    end = next((i for i in range(c, len(L)) if L[i].startswith("Note.")), None)
    if end is None:
        raise SystemExit(f"{key}: note paragraph not found in draft; nothing written")
    L[start:end + 1] = gen

# ---------------------------------------------------------------- structural region
s = next(i for i, l in enumerate(L) if l.startswith("Table 5. Standardized Network-to-Activity"))
e = next(i for i, l in enumerate(L) if l.startswith("\\subsection*{Appendix"))
# keep structural block only up to Table A3 (A3 handled separately)
a3_start = next(i for i, l in enumerate(struct) if l.startswith("Table A3."))
struct_main = struct[:a3_start]
struct_a3 = struct[a3_start:]
L = L[:s] + struct_main + L[e:]

# ---------------------------------------------------------------- Table A3 block
a3s = next(i for i, l in enumerate(L) if l.startswith("Table A3."))
a4 = next(i for i, l in enumerate(L) if l.startswith("Table A4"))
if a4 > 0 and L[a4 - 1].startswith("\\Needspace"):
    a4 -= 1
L = L[:a3s] + struct_a3 + [""] + L[a4:]

open(PATH, "w").write("\n".join(L))
print("spliced: measurement blocks, structural region, Table A3")