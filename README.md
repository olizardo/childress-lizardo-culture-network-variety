# Beyond Network Variety in Arts Attendance

Reproducibility materials for:

> Lizardo, Omar, and Clayton Childress. "Beyond Network Variety in Arts Attendance: Network Composition, Ideological Sorting, and Everyday Leisure."

This repository contains the full analytic pipeline, from the raw survey extract through the tables and figures reported in the paper.

## Data

- **Source:** `data/grabbagdata111925.dta` — a Prolific "representative sample" quota panel of US adults (fielded April 2025), $N = 1{,}258$ completed surveys.
- **Minimal reproducibility extract:** The version of `data/grabbagdata111925.dta` committed to this repository is a *minimal-column extract* (63 of the 179 columns in the full raw survey export): the 20 activity/leisure items, the 18 weak-tie and 18 strong-tie group-position-generator items, and the 7 covariates (`educ`, `gender`, `race3`, `child_arts`, `income`, `age2`, `poli`) used anywhere in the pipeline. This is everything `Scripts/01-05` read from the raw data; no other survey columns are needed to reproduce any table or figure in the paper. The full raw export (which may carry additional identifying or unused variables) is kept out of version control — see `Scripts/00_extract_minimal_data.R`, which regenerates the committed extract from a local, gitignored `data/grabbagdata111925_full.dta`.
- **Sample construction:** The paper's reported analytic sample drops duplicate respondents and speeders before listwise deletion on the measurement-model items ($N = 1{,}258 \to 1{,}243$). The extract distributed here does not retain the original `v8` (duplicate-ID) or `duration` (completion-time) columns needed to reproduce that explicit drop step. In practice this has no material consequence: listwise deletion on the 20 activity items alone already recovers exactly $N = 1{,}243$, matching the paper's reported analytic sample for the activities factor model. The regression sample used for all reported models is $N = 1{,}227$ after listwise deletion on every model covariate (see `Scripts/02_fit_models.R`). This is documented in comments at the top of `Scripts/01_prepare_data.R`.
- **Measurement models:**
  - A 20-item activities/leisure battery, reduced via 3-factor PCA (varimax) to *Public Arts Participation*, *Solitary Leisure*, and a residual ("DIY Practical") factor used as a discriminant-validity check.
  - An 18-item weak-tie and an 18-item strong-tie group-position generator (covering race, gender, religiosity, urbanicity, ideology, etc.), each reduced via a separate 3-factor PCA (varimax) to *Variety*, *Liberal Composition*, and *Conservative Composition*.

The PCA scores above are the round-1 operationalization and remain the basis of the OLS appendix tables. In the revision, both batteries are also modeled with polychoric confirmatory and target-rotated exploratory structural equation models (WLSMV), and those ESEM factors are the measurement blocks of the structural models in the manuscript; see the run-order section and `revision_reports/`.

## Software requirements

- R 4.5.3 (the version recorded in `renv.lock`).
- R packages are pinned in `renv.lock`; install them with `renv::restore()` as described in the reproducibility section below. The analysis uses `haven`, `dplyr`, `tidyr`, `psych`, `polycor`, `GPArotation`, `lavaan`, `semTools`, `estimatr`, `car`, `ggplot2`, and `stringr`. `semPlot` is recorded in the lockfile for two diagnostic plotting scripts in `Scripts/diagnostics/`, and `googledrive` is used only by the audit's `--pull` option and the maintainers' sync scripts. The statistical pipeline (`run_all.R` and scripts 01 to 09) needs no external services.

## Reproducibility environment (renv)

The package versions used for the analysis are pinned in `renv.lock` (R 4.5.3, with `psych` 2.6.9, `lavaan` 0.7-3, `estimatr` 2.0.1, `polycor` 0.8-2, and `semTools` 0.5-10, among others). To reproduce the same environment:

1. Open R in the repository root. The included `.Rprofile` activates renv automatically.
2. Run `renv::restore()` to install the pinned packages into the project library.
3. Check with `renv::status()`; the only expected flag is four recommended base packages (`class`, `codetools`, `KernSmooth`, `spatial`), which are installed but not used directly by the scripts.

## Script run order

Run the scripts in this order from the repository root. `run_all.R` runs steps 1 to 4 in one call; the remaining scripts are run individually after it.

1. `Scripts/01_prepare_data.R`: factor models and prepared data (writes `cache/01_prepared_data.rds`).
2. `Scripts/02_fit_models.R`: regressions and Wald tests (`cache/02_models.rds`).
3. `Scripts/03_generate_figures.R`: Figures 1 to 4 and Figure A1 in `Plots/`.
4. `Scripts/04_generate_tables.R`: markdown tables in `cache/`.
5. `Scripts/05_render_table_images.R`: flat table PNGs in `Plots/` (round-1 manuscript embedding only; not used by the current draft).
6. `Scripts/06_rotation_sensitivity.R`: varimax versus oblimin comparison (`cache/rotation_sensitivity/`).
7. `Scripts/07_measurement_reliability_cfa.R`: reliability and ordinal CFA (`cache/07_reliability_cfa.rds`).
8. `Scripts/08_solitary_robustness.R`: solitary-leisure sensitivity (`cache/08_solitary_robustness.rds`).
9. `Scripts/09_no_gender_robustness.R`: refits without gender items (`cache/09_no_gender_robustness.rds`).
10. `Scripts/audit_manuscript_stats.R`: checks reported statistics against the manuscript text. This needs the manuscript text cache (`cache/_manuscript_text_cache.txt`), or the maintainers' Google Drive access to refresh it with `--pull`.

Each script expects the working directory to be the repository root and reads `data/grabbagdata111925.dta` directly. The factor models use polychoric correlations throughout (`Scripts/poly_helpers.R`), so no Pearson correlations enter the measurement models.

### Manuscript build (reviewers' round)

`Scripts/manuscript/build_manuscript.sh` runs the full manuscript chain in one call and reproduces `results_draft.tex` end to end:

1. The four structural fit scripts in `Scripts/diagnostics/` (`sem_gpa_fixed_sep_covars.R`, `sem_gpa_fixed_sep_decomposition.R`, `sem_gpa_fixed_splithalf.R`, `esem_ties_target.R`), about nine minutes total.
2. The three table/figure generators in `Scripts/manuscript/` (`build_measurement_blocks.R`, `build_structural_blocks.R`, `build_ols_appendix.R`), which write full LaTeX blocks, including captions and notes, to `cache/manuscript/`.
3. `build_prose_numbers.R` and `check_prose_numbers.py`, which recompute every statistic cited in the prose from the caches into `cache/manuscript/prose_numbers.csv` (154 rows) and verify each one against the draft text.
4. `splice_blocks.py` and `splice_ols_appendix.py`, which replace the generated blocks in `results_draft.tex` (block-level; they refuse to write on a missing caption or note).
5. Two passes of `xelatex` into `cache/manuscript/pdf/`.

Flags: `--skip-fits` reuses the existing fit caches and starts at the generators; `--compile-only` runs only the splices and the compile. Stage logs go to `cache/manuscript/build_logs/`. A full run from the fits reproduces the committed draft byte-for-byte.

## Reproducing the analysis

From the repository root:

```bash
Rscript run_all.R
```

This runs the full pipeline end to end:

| Step | Script | What it does |
|---|---|---|
| 1 | `Scripts/01_prepare_data.R` | Loads `data/grabbagdata111925.dta` (the committed minimal-column extract), fits the three factor-analytic measurement models, and writes the analytic data frame to `cache/01_prepared_data.rds`. |
| 2 | `Scripts/02_fit_models.R` | Fits the hierarchical four-model OLS sequence (variety only → + demographics → + ideological composition → + both) for each outcome, with HC1 robust standard errors, plus the in-text Wald tests. Writes `cache/02_models.rds`. |
| 3 | `Scripts/03_generate_figures.R` | Renders Figures 1–4 and Figure A1 (the predictor correlation heat map) to `Plots/` at 6.5in width, 300 DPI. |
| 4 | `Scripts/04_generate_tables.R` | Renders all regression and descriptive tables as GitHub-flavored markdown to `cache/`. |

Total runtime is under a minute on a standard laptop; no parallelization or HPC resources are required.

Several additional scripts are present but are **not** part of the statistical pipeline and are not run by `run_all.R`:

- `Scripts/00_extract_minimal_data.R` is a maintainer-only utility that regenerates the committed `data/grabbagdata111925.dta` extract from a full, gitignored raw export (`data/grabbagdata111925_full.dta`). It is not needed to *reproduce* the analysis — the extract it produces is already committed — only to *regenerate* that extract if the underlying raw data changes.
- `Scripts/05_render_table_images.R` re-renders the tables from `cache/02_models.rds` as flat PNG images. This exists purely to support the maintainers' internal Google Docs manuscript-sync workflow and is unnecessary for statistical reproduction.
- The Google Docs manuscript-sync helpers (`sync_manuscript.R`/`.py`, `fix_figure_table_page_fit.py`, `strip_reference_links.py`, `docx_text_edits.py`, and the `revert_*`, `apply_*`, and `move_*`/`remove_*` utilities) are one-off or maintainer-only helpers that require Google Drive credentials. For the manuscript itself they are superseded by the generated-block pipeline in `Scripts/manuscript/`, and they are archived in `Scripts/archive_oneoff/`.
- `Scripts/build_ssr_submission_docs.py` rebuilds the blind-review submission docx files into `submission_round1/SSR-submission/`.
- Other one-off editing scripts from earlier revision rounds (`insert_*`, `revise_*`) are also archived in `Scripts/archive_oneoff/` and are not runnable in the current layout.

## Table and figure correspondence

Every item below is generated; nothing table-related is typed in `results_draft.tex`. The one-command chain is `Scripts/manuscript/build_manuscript.sh` (see the run-order section above).

### Measurement tables (Tables 1 to 4, A1, A2)

Produced by `Scripts/manuscript/build_measurement_blocks.R` (full blocks to `cache/manuscript/measurement_blocks.tex`, numbers cached in `measurement_numbers.rds`):

| Manuscript item | Content | Sources |
|---|---|---|
| Table 1 | Varimax PCA loadings, activity battery | `cache/01_prepared_data.rds` (`act_fa`) |
| Table 2 | Activity CFAs and target ESEM, fit and scaled difference tests | refits in the generator (WLSMV) |
| Table 3 | Activity ESEM loadings, target rotation | refit in the generator |
| Table 4 | Weak- and strong-tie ESEM loadings, target rotation | `cache/diagnostics/esem_ties_target.rds` |
| Table A1 | Oblimin EFA loadings for the activity battery | refit in the generator |
| Table A2 | Tie confirmatory loadings and antonym residual covariances | `cache/07_reliability_cfa.rds`, refits in the generator |

### Structural tables and path diagrams (Tables 5 to 10, Figures 5 to 10, Table A3)

Produced by `Scripts/manuscript/build_structural_blocks.R` (to `cache/manuscript/structural_blocks.tex` and `structural_numbers.rds`):

| Manuscript item | Content | Sources |
|---|---|---|
| Table 5 | Network-to-activity paths, weak and strong ties | `cache/diagnostics/sem_gpa_fixed_sep_covars.rds` |
| Tables 6 and 7 | Demographic predictors of weak- and strong-tie network factors | same |
| Table 8 | Demographic predictors of activity factors, ties side by side | same |
| Figures 5 to 10 | TikZ path diagrams, education / childhood arts / ideology to arts (inline, captions kept with `\Needspace`) | same |
| Tables 9 and 10 | Direct, network-mediated, and total effects with delta-method SEs | `cache/diagnostics/sem_gpa_fixed_sep_decomposition.rds` |
| Table A3 | Paths under target and oblimin rotations and in random halves | `cache/diagnostics/sem_gpa_fixed_splithalf.rds` |

### Appendix OLS tables (Tables A4, A5)

Produced by `Scripts/manuscript/build_ols_appendix.R` (to `cache/manuscript/ols_appendix.tex`) from `cache/02_models.rds` (the four-step HC1 OLS sequence on the polychoric PCA scores, Scripts 02).

### Standalone figures (Figures 1 to 4, Figure A1)

`Scripts/03_generate_figures.R` renders `Plots/fig1_activity_loadings_heatmap.png`, `fig2_network_loadings_heatmap.png`, `fig3_variety_margins_plot.png`, `fig4_leisure_variety_margins_plot.png`, and `figA1_predictor_correlation_heatmap.png` at 6.5in width, 300 DPI. These are standalone reproductions of the round-1 figures; the current draft embeds no raster images, and its Figures 5 to 10 are inline TikZ generated with the structural tables.

### Prose numbers

Every statistic cited in the running prose is exported with its cache source to `cache/manuscript/prose_numbers.csv` by `Scripts/manuscript/build_prose_numbers.R` and checked against the draft text by `Scripts/manuscript/check_prose_numbers.py`. Threshold claims (for example, "p < .001") are tested in R; a failing claim is written as `CLAIM FAILS` and surfaces as MISSING in the checker.

The `cache/table*.md` markdown tables and the `Plots/table*.png` flat images from the round-1 pipeline (Scripts 04 and 05) remain in the cache for the record but no longer correspond to the manuscript's tables.

## Repository structure

```
.
├── run_all.R                    # Master pipeline driver (Rscript run_all.R)
├── results_draft.tex            # Manuscript of record (tables/figures spliced from caches)
├── data/
│   ├── grabbagdata111925.dta      # Minimal-column survey extract (N = 1,258; 63 of 179 raw columns)
│   └── grabbagdata111925_full.dta # Full raw export (gitignored; source of the extract)
├── Scripts/
│   ├── 00_extract_minimal_data.R # Maintainer-only: regenerates the extract above from the full raw export
│   ├── 01_prepare_data.R        # Data ingestion & factor models
│   ├── 02_fit_models.R          # Hierarchical OLS regressions & Wald tests
│   ├── 03_generate_figures.R    # Figures 1-4 and Figure A1
│   ├── 04_generate_tables.R     # Markdown tables
│   ├── 05_render_table_images.R # Flat table PNGs (round-1 embedding)
│   ├── 06-09                    # Rotation sensitivity, reliability/CFA, robustness refits
│   ├── audit_manuscript_stats.R # Doc-text statistic audit (maintainer-only)
│   ├── diagnostics/             # Reviewer-round diagnostic fits (incl. the four structural fit scripts)
│   ├── manuscript/              # Manuscript generators, prose-number registry, build_manuscript.sh
│   ├── archive_oneoff/          # Archived one-off editing scripts
│   └── poly_helpers.R           # Polychoric helpers shared by the pipeline
├── submission_round1/           # Round-1 SSR submission package and reviews
├── revision_reports/            # Narrative reports from the revision round
├── cache/                       # Cached model objects (.rds) and generated LaTeX fragments
└── Plots/                       # PNG figures (and archived round-1 table images)
```

## Contact

Omar Lizardo (UCLA) and Clayton Childress (University of British Columbia).
