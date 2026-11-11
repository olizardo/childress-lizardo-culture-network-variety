#!/usr/bin/env bash
# Full manuscript build for results_draft.tex.
#
# Run order (from the project root):
#   ./Scripts/manuscript/build_manuscript.sh            everything (fits + generators + splice + compile)
#   ./Scripts/manuscript/build_manuscript.sh --skip-fits  reuse existing fit caches, then generators onward
#   ./Scripts/manuscript/build_manuscript.sh --compile-only  only splice + xelatex
#
# Fit scripts (about 30 to 40 minutes total) run sequentially in the background with logs in
# cache/manuscript/build_logs/. Long generators also log there. Splice refuses to write on any
# row-count or caption mismatch. The compile output goes to cache/manuscript/pdf/.

set -euo pipefail
cd "$(dirname "$0")/../.."   # project root

LOGS=cache/manuscript/build_logs
PDF=cache/manuscript/pdf
mkdir -p "$LOGS" "$PDF"

MODE=all
case "${1:-}" in
  "") MODE=all ;;
  --skip-fits) MODE=skipfits ;;
  --compile-only) MODE=compile ;;
  *) echo "usage: build_manuscript.sh [--skip-fits|--compile-only]"; exit 2 ;;
esac

run_bg () {  # run_bg <logfile> <cmd...>
  local log=$1; shift
  echo "[build] $* (log: $log)"
  "$@" > "$LOGS/$log" 2>&1
}

# ---------------------------------------------------------------- 1. fit scripts
if [ "$MODE" = all ]; then
  run_bg sem_gpa_fixed_sep_covars.log \
    Rscript --no-save --no-restore Scripts/diagnostics/sem_gpa_fixed_sep_covars.R
  run_bg sem_gpa_fixed_sep_decomposition.log \
    Rscript --no-save --no-restore Scripts/diagnostics/sem_gpa_fixed_sep_decomposition.R
  run_bg sem_gpa_fixed_splithalf.log \
    Rscript --no-save --no-restore Scripts/diagnostics/sem_gpa_fixed_splithalf.R
  run_bg esem_ties_target.log \
    Rscript --no-save --no-restore Scripts/diagnostics/esem_ties_target.R
fi

# ---------------------------------------------------------------- 2. generators
if [ "$MODE" != compile ]; then
  run_bg build_measurement_blocks.log \
    Rscript --no-save --no-restore Scripts/manuscript/build_measurement_blocks.R
  run_bg build_structural_blocks.log \
    Rscript --no-save --no-restore Scripts/manuscript/build_structural_blocks.R
  run_bg build_ols_appendix.log \
    Rscript --no-save --no-restore Scripts/manuscript/build_ols_appendix.R

  # ---------------------------------------------------------------- 3. prose-number registry and check
  run_bg build_prose_numbers.log \
    Rscript --no-save --no-restore Scripts/manuscript/build_prose_numbers.R
  python3 Scripts/manuscript/check_prose_numbers.py | tee "$LOGS/check_prose_numbers.log"
fi

# ---------------------------------------------------------------- 4. splice
python3 Scripts/manuscript/splice_blocks.py | tee "$LOGS/splice.log"

# ---------------------------------------------------------------- 5. compile
xelatex -interaction=nonstopmode -output-directory "$PDF" results_draft.tex > "$LOGS/xelatex_1.log" 2>&1 || true
xelatex -interaction=nonstopmode -output-directory "$PDF" results_draft.tex > "$LOGS/xelatex_2.log" 2>&1
if grep -q "^!" "$LOGS/xelatex_2.log"; then
  echo "[build] LaTeX errors:"; grep -A2 "^!" "$LOGS/xelatex_2.log" | head -40; exit 1
fi
echo "[build] PDF: $PDF/results_draft.pdf"
