# Beyond Network Variety in Arts Attendance

Reproducibility materials for:

> Lizardo, Omar, and Clayton Childress. "Beyond Network Variety in Arts Attendance: Network Composition, Ideological Sorting, and Everyday Leisure."

This repository contains the full analytic pipeline, from the raw survey extract through the tables and figures reported in the paper.

## Data

- **Source:** `grabbagdata111925.dta` — a Prolific "representative sample" quota panel of US adults (fielded April 2025), $N = 1{,}258$ completed surveys.
- **Minimal reproducibility extract:** The version of `grabbagdata111925.dta` committed to this repository is a *minimal-column extract* (63 of the 179 columns in the full raw survey export): the 20 activity/leisure items, the 18 weak-tie and 18 strong-tie group-position-generator items, and the 7 covariates (`educ`, `gender`, `race3`, `child_arts`, `income`, `age2`, `poli`) used anywhere in the pipeline. This is everything `Scripts/01-05` read from the raw data; no other survey columns are needed to reproduce any table or figure in the paper. The full raw export (which may carry additional identifying or unused variables) is kept out of version control — see `Scripts/00_extract_minimal_data.R`, which regenerates the committed extract from a local, gitignored `grabbagdata111925_full.dta`.
- **Sample construction:** The paper's reported analytic sample drops duplicate respondents and speeders before listwise deletion on the measurement-model items ($N = 1{,}258 \to 1{,}243$). The extract distributed here does not retain the original `v8` (duplicate-ID) or `duration` (completion-time) columns needed to reproduce that explicit drop step. In practice this has no material consequence: listwise deletion on the 20 activity items alone already recovers exactly $N = 1{,}243$, matching the paper's reported analytic sample for the activities factor model. The regression sample used for all reported models is $N = 1{,}227$ after listwise deletion on every model covariate (see `Scripts/02_fit_models.R`). This is documented in comments at the top of `Scripts/01_prepare_data.R`.
- **Measurement models:**
  - A 20-item activities/leisure battery, reduced via 3-factor PCA (varimax) to *Public Arts Participation*, *Solitary Leisure*, and a residual ("DIY Practical") factor used as a discriminant-validity check.
  - An 18-item weak-tie and an 18-item strong-tie group-position generator (covering race, gender, religiosity, urbanicity, ideology, etc.), each reduced via a separate 3-factor PCA (varimax) to *Variety*, *Liberal Composition*, and *Conservative Composition*.

## Software requirements

- R (developed and tested under R 4.5.3).
- R packages: `haven`, `dplyr`, `tidyr`, `psych`, `estimatr`, `car`, `ggplot2`.

Install everything with:

```r
install.packages(c("haven", "dplyr", "tidyr", "psych", "estimatr", "car", "ggplot2"))
```

No compiled dependencies, external services, or non-CRAN packages are required to reproduce the statistical results. (`googledrive` is used only by the maintainers' internal manuscript-sync scripts, described below, and is not needed to reproduce the analysis.)

## Reproducing the analysis

From the repository root:

```bash
Rscript run_all.R
```

This runs the full pipeline end to end:

| Step | Script | What it does |
|---|---|---|
| 1 | `Scripts/01_prepare_data.R` | Loads `grabbagdata111925.dta` (the committed minimal-column extract), fits the three factor-analytic measurement models, and writes the analytic data frame to `cache/01_prepared_data.rds`. |
| 2 | `Scripts/02_fit_models.R` | Fits the hierarchical four-model OLS sequence (variety only → + demographics → + ideological composition → + both) for each outcome, with HC1 robust standard errors, plus the in-text Wald tests. Writes `cache/02_models.rds`. |
| 3 | `Scripts/03_generate_figures.R` | Renders Figures 1–3 (and the Appendix correlation heat map) to `Plots/` at 6.5in width, 300 DPI. |
| 4 | `Scripts/04_generate_tables.R` | Renders all regression and descriptive tables as GitHub-flavored markdown to `cache/`. |

Total runtime is under a minute on a standard laptop; no parallelization or HPC resources are required.

Several additional scripts are present but are **not** part of the statistical pipeline and are not run by `run_all.R`:

- `Scripts/00_extract_minimal_data.R` is a maintainer-only utility that regenerates the committed `grabbagdata111925.dta` extract from a full, gitignored raw export (`grabbagdata111925_full.dta`). It is not needed to *reproduce* the analysis — the extract it produces is already committed — only to *regenerate* that extract if the underlying raw data changes.
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
├── grabbagdata111925.dta        # Minimal-column survey extract (N = 1,258; 63 of 179 raw columns)
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
