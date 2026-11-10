# Beyond Network Variety in Arts Attendance

Reproducibility materials for:

> Lizardo, Omar, and Clayton Childress. "Beyond Network Variety in Arts Attendance: Network Composition, Ideological Sorting, and Everyday Leisure."

This repository contains the full analytic pipeline, from the raw survey extract through the tables and figures reported in the paper.

## Data

- **Source:** `data/grabbagdata111925.dta` — a Prolific "representative sample" quota panel of US adults (fielded April 2025), $N = 1{,}258$ completed surveys.
- **Minimal reproducibility extract:** The version of `data/grabbagdata111925.dta` committed to this repository is a *minimal-column extract* (63 of the 179 columns in the full raw survey export): the 20 activity/leisure items, the 18 weak-tie and 18 strong-tie group-position-generator items, and the 7 covariates (`educ`, `gender`, `race3`, `child_arts`, `income`, `age2`, `poli`) used anywhere in the pipeline. This is everything `Scripts/01-05` read from the raw data; no other survey columns are needed to reproduce any table or figure in the paper. The full raw export (which may carry additional identifying or unused variables) is kept out of version control — see `Scripts/00_extract_minimal_data.R`, which regenerates the committed extract from a local, gitignored `data/data/grabbagdata111925_full.dta`.
- **Sample construction:** The paper's reported analytic sample drops duplicate respondents and speeders before listwise deletion on the measurement-model items ($N = 1{,}258 \to 1{,}243$). The extract distributed here does not retain the original `v8` (duplicate-ID) or `duration` (completion-time) columns needed to reproduce that explicit drop step. In practice this has no material consequence: listwise deletion on the 20 activity items alone already recovers exactly $N = 1{,}243$, matching the paper's reported analytic sample for the activities factor model. The regression sample used for all reported models is $N = 1{,}227$ after listwise deletion on every model covariate (see `Scripts/02_fit_models.R`). This is documented in comments at the top of `Scripts/01_prepare_data.R`.
- **Measurement models:**
  - A 20-item activities/leisure battery, reduced via 3-factor PCA (varimax) to *Public Arts Participation*, *Solitary Leisure*, and a residual ("DIY Practical") factor used as a discriminant-validity check.
  - An 18-item weak-tie and an 18-item strong-tie group-position generator (covering race, gender, religiosity, urbanicity, ideology, etc.), each reduced via a separate 3-factor PCA (varimax) to *Variety*, *Liberal Composition*, and *Conservative Composition*.

## Software requirements

- R 4.5.3 (the version recorded in `renv.lock`).
- R packages are pinned in `renv.lock`; install them with `renv::restore()` as described in the reproducibility section above. The analysis uses `haven`, `dplyr`, `tidyr`, `psych`, `polycor`, `GPArotation`, `lavaan`, `semTools`, `estimatr`, `car`, `ggplot2`, and `stringr`. `semPlot` is recorded in the lockfile for two diagnostic plotting scripts in `Scripts/diagnostics/`, and `googledrive` is used only by the audit's `--pull` option and the maintainers' sync scripts. The statistical pipeline (`run_all.R` and scripts 01 to 09) needs no external services.

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
5. `Scripts/05_render_table_images.R`: flat table PNGs in `Plots/` (manuscript embedding only).
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
| 3 | `Scripts/03_generate_figures.R` | Renders Figures 1–3 (and the Appendix correlation heat map) to `Plots/` at 6.5in width, 300 DPI. |
| 4 | `Scripts/04_generate_tables.R` | Renders all regression and descriptive tables as GitHub-flavored markdown to `cache/`. |

Total runtime is under a minute on a standard laptop; no parallelization or HPC resources are required.

Several additional scripts are present but are **not** part of the statistical pipeline and are not run by `run_all.R`:

- `Scripts/00_extract_minimal_data.R` is a maintainer-only utility that regenerates the committed `data/grabbagdata111925.dta` extract from a full, gitignored raw export (`data/grabbagdata111925_full.dta`). It is not needed to *reproduce* the analysis — the extract it produces is already committed — only to *regenerate* that extract if the underlying raw data changes.
- `Scripts/05_render_table_images.R` re-renders the tables from `cache/02_models.rds` as flat PNG images (matching the plain screenshot style already embedded in the manuscript). This exists purely to support the maintainers' internal Google Docs manuscript-sync workflow and is unnecessary for statistical reproduction.
- `Scripts/sync_manuscript.R`, `Scripts/sync_manuscript.py`, `Scripts/fix_figure_table_page_fit.py`, `Scripts/insert_table3.py`, and `Scripts/strip_reference_links.py` are one-off or maintainer-only utilities for synchronizing the live Google Docs manuscript draft with this repository's figures, tables, and reference list. They require Google Drive credentials the maintainers hold and are irrelevant to reproducing the reported results.

## Table and figure correspondence

| Manuscript item | Output file |
|---|---|
| Table 1 (Public Arts Participation regression) | `cache/table1_arts_participation.md` |
| Table 2 (Solitary Leisure regression) | `cache/table2_solitary_leisure.md` |
| Table 3 (Residual/DIY-practical leisure regression, discriminant-validity check) | fitted as `residual_models` in `cache/02_models.rds` |
| Table A1 (Descriptive statistics) | `cache/tableA1_descriptives_continuous.md`, `cache/tableA1_descriptives_categorical.md` |
| Table A2 (Activity/leisure item factor loadings) | `cache/tableA2_activity_loadings.md` |
| Table A3 (Weak- and strong-tie item factor loadings) | `cache/tableA3_weak_tie_loadings.md`, `cache/tableA3_strong_tie_loadings.md` |
| Table A5 (Network variety factor validation) | `cache/tableA5_variety_validation.md` |
| Figure 1 (Activity/leisure factor loadings heat map) | `Plots/fig1_activity_loadings_heatmap.png` |
| Figure 2 (Weak- and strong-tie factor loadings heat map) | `Plots/fig2_network_loadings_heatmap.png` |
| Figure 3 (Predicted margins: network variety on arts participation) | `Plots/fig3_variety_margins_plot.png` |
| Figure A4 (Correlation heat map of continuous/ordinal predictors) | `Plots/figA1_predictor_correlation_heatmap.png` |

`Plots/table*.png` and `Plots/tableA*.png` are flat-image renders of the same tables, produced by the non-pipeline script noted above for embedding in the manuscript; the authoritative numeric source for every table is the corresponding `.md` file in `cache/` (or, for Table 3, `cache/02_models.rds`).

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
│   ├── 03_generate_figures.R    # Figures 1-3 and Figure A4
│   ├── 04_generate_tables.R     # Markdown tables
│   └── ...                      # Maintainer-only manuscript-sync utilities (see above)
├── cache/                       # Cached model objects (.rds) and markdown tables
└── Plots/                       # Publication-grade PNG figures and table images
```

## Contact

Omar Lizardo (UCLA) and Clayton Childress (University of British Columbia).
