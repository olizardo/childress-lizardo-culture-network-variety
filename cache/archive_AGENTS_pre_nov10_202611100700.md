# Project Memory: Beyond Network Variety in Arts Attendance (Practice and Networks)

**Trimmed November 9, 2026.** Removed: the Google Doc sync workflow and rId/media mapping, the Doc image pipeline, superseded confirmatory and joint-SEM results, the September and October rotation history, and the diagnostics that the November 9 structural models replace. Full earlier versions are in `cache/archive_AGENTS_pre_*.md` and in git history. Shared cross-project standards are in `~/.config/agents/AGENTS.md`.

## 1. Overview

- **Manuscript:** *Beyond Network Variety in Arts Attendance: Network Composition, Ideological Sorting, and Everyday Leisure*. Authors: Omar Lizardo (UCLA) and Clayton Childress (UBC), equal coauthors.
- **Manuscript of record:** `results_draft.tex` in the project root. The Google Doc is not to be used or updated unless the user asks for a Drive sync, which needs approval for Drive writes.
- **Data:** Prolific representative quota panel (April 2025), N = 1,258 (raw extract `grabbagdata111925.dta`). Complete-case analytic N = 1,243 for activities and N = 1,227 for models with covariates.
- **Stata original:** `activitiesnetworks1212025.do` (partial; Windows path).
- **Measurement:** 20 activity items (varimax, polychoric). Two 16-item position-generator batteries (weak and strong ties; gender items excluded), each with three factors: variety, liberal composition, conservative composition. The item sorting comes from script 07 (`cache/07_reliability_cfa.rds`, `structures`).

## 2. Polychoric Policy

- Use polychoric correlations for all ordinal items: EFA, PCA, reliability, CFA, ESEM, and any matrix feeding a measurement model. Use Pearson only for variables that are continuous by design, and say so in the text.
- Implementation: `psych::polychoric()` for correlation matrices passed to `fa()`, `principal()`, `omega()`, or `alpha()`. For `lavaan`, use `ordered = <items>` with `estimator = "WLSMV"`.
- `Scripts/poly_helpers.R` provides `poly_pca()` and `poly_cor_count()`.
- Sparse top categories: some activity items have very few responses in category 7 (full sample: symphony 2, dance 2, carnival 5, museum 8). Check category counts before fitting. In half-samples, empty top categories give NaN in `psych::polychoric()`; use dense category codes. The collapsed-top-category sensitivity (`Scripts/diagnostics/activities_reporting_gaps.R`, 4b) leaves every factor assignment unchanged and moves the CFA CFI from .956 to .955.

## 3. Reporting Models (current)

**Structural models.** The main results are separate weak-tie and strong-tie structural models with covariates. Each model uses fixed-loading ESEM blocks for both the network and the activity factors. Loadings are fixed from a minres solution rotated outside `sem()` with `GPArotation::targetT` to the script 07 sorting. Cross-loadings are present but fixed.

- Scripts: `Scripts/diagnostics/sem_gpa_fixed_sep_covars.R` (fits), `sem_gpa_fixed_sep_decomposition.R` (labeled direct, network-mediated, and total effects with delta-method SEs), `sem_gpa_fixed_splithalf.R` (random halves with seed 4217, target and oblimin rotations). Outputs are in `cache/diagnostics/` as `.rds` with the same base names.
- Fit (N = 1,227): weak CFI .891, RMSEA .058, SRMR .075; strong CFI .862, RMSEA .066, SRMR .086. Both converge with positive definite latent covariances.
- Split-half check: variety paths on arts are positive in every half under both rotations. The weak liberal path on arts is near zero and changes sign, so treat it as unstable.
- Decomposition: total effects on the activity factors are nearly identical across tie models (differences of .004 or less). Only the split between direct and network-mediated routes differs.
- The joint weak-plus-strong fixed-loading model converges only with the 16 matched residuals. Its leisure variety paths cancel (+.46 and −.47) with disturbance correlation .84. Do not report it as the main result.

**OLS models.** `cache/02_models.rds` (Scripts 02 and 03) holds the four-step OLS sequence on the polychoric PCA scores (HC1 SEs, `estimatr`). These are Tables A4 (arts) and A5 (solitary leisure) in the draft.

**Suppression is measurement-dependent.** Under the fixed-loading SEM, weak liberal composition on arts is +.05 (p < .001). Under the OLS on PCA scores, it is −.12 (p < .001, Model 4). The draft says the sign depends on the measurement model and does not treat suppression as settled. Keep that wording unless new evidence changes it.

**Confirmatory and ESEM measurement (comparison only).** Confirmatory tie models (script 07 sorting, gender excluded) have baseline CFI .909 and RMSEA .111 for weak ties, and CFI .919 and RMSEA .106 for strong ties. Antonym-freed versions have CFI .915 and .928. Target-rotated tie ESEMs have CFI .979 and RMSEA .061 (weak) and CFI .975 and RMSEA .069 (strong). The ESEMs are the measurement models for the draft's Table 4. The CFA-based SEMs with covariates (CFI .906 and .903) are superseded by the fixed-loading models.

## 4. Measurement Facts Still in Force

- **Activities:** KMO .955, variance explained 63.2% (polychoric varimax). Three-factor CFA CFI .956, RMSEA .095. Target ESEM CFI .979, RMSEA .074, SRMR .035 (draft Table 2). Residual factor omega .55, polychoric alpha .31. Leisure factor is weak: the arts factor is reliable and solitary leisure rests on walking, exercise, and reading.
- **Tie batteries:** KMO .939 (weak) and .935 (strong). Variance explained about 68% for each. Pooled and joint six-factor measurement models are inadmissible; report separate tie models.
- **Tie ESEM fit (verified November 9, 2026, `Scripts/diagnostics/esem_ties_target.R`, `cache/diagnostics/esem_ties_target.rds`):** weak CFI .979, RMSEA .061, SRMR .029; strong CFI .975, RMSEA .069, SRMR .033. Both specifications (`target` and `target.strict`) give identical fit. Confirmatory baselines: weak CFI .909, RMSEA .111, SRMR .072; strong CFI .919, RMSEA .106, SRMR .071.
- **Antonym residuals (Table A2, verified November 9, 2026):** Weak −.32, .10, −.40. Strong −.55, −.05, −.44. Unstandardized SEs .019 to .021 (refit on N = 1,258; the table note's "standard error of .02" is accurate for the unstandardized metric).
- **Activity target rotation (draft line 81):** the ESEM target matrix is binary, built from the varimax primary-loading assignment (1 for the assigned factor, 0 for the other two). Target rotation pulls off-target loadings toward zero rather than fixing them. Oblimin EFA (Table A1) is the only check on the activity structure without that assignment; its factor correlations (.19, .15, .01) differ from the ESEM's (.36, .21, .11).
- **Rotation:** Varimax is the standing choice for the PCA scores used in the OLS models. The SEMs use fixed target and oblimin loadings. Factor correlations depend on the rotation, so the paths are reported with that caveat. Oblimin results are reproducible from `Scripts/06_rotation_sensitivity.R`.
- **Politics:** `poli` is the mean of social and economic items (1 very liberal to 7 very conservative), entered linearly.
- **Education:** `educ_cat` (four levels) in the OLS models; `educ_ord` (1 to 4) in the SEMs. The paper must state this.
- **Covariates:** gender, race and ethnicity, childhood arts exposure, income, age in years (`age2` holds age in years), politics, and education.

## 5. Scripts and Pipeline

- **Run order:** `Scripts/01` (PCA and prepared data), `02` (OLS models), `03` (Figures 1–4 and A1), `04` (tables), `05` (table PNGs), `06` (rotation sensitivity), `07` (reliability and CFA), `08` (solitary-leisure robustness), `09` (no-gender refit), `Scripts/audit_manuscript_stats.R` (statistics audit). Scripts 01 to 05 were rerun on November 9, 2026, so the caches and `Plots/` reflect the polychoric pipeline. Archives: `cache/archive_plots_pre_rerun_*`, `cache/archive_tables_pre_rerun_*`.
- **Diagnostics:** `Scripts/diagnostics/`. The current structural scripts are listed in Section 3. Pooled-model SEM scripts are archived in `Scripts/diagnostics/archive/pooled_sem/`.
- **Reproducibility:** `renv.lock` (R 4.5.3). Run scripts with `Rscript --no-save --no-restore` from the project root, or with `sys.source()` in an isolated environment.
- **Audit:** `Scripts/audit_manuscript_stats.R` recomputes reported statistics from the caches and compares them against the live Doc text (`--pull`). It does not check `results_draft.tex`. Latest run: 13 OK, 18 MISMATCH, 9 NOT_FOUND. The mismatches are the expected polychoric shifts and the Doc has not been updated. The mismatched numbers are listed in the session notes (KMO, variance explained, Table A2 correlations, Table 1 and 2 Wald tests and coefficients).

## 6. Warnings

- **lavaan covariate pathology:** with `fixed.x = TRUE`, covariate regressions onto EFA-block factors (estimated inside `sem()`) produce coefficients whose signs contradict the item-level associations. The fixed.x = FALSE route is not identified. Do not report set-ESEM structural paths with covariates.
- **Joint models:** the joint weak-plus-strong CFA and set-ESEM are inadmissible. The draft reports separate tie models.
- **Factor-score choice:** CFA (MAP) scores diverge from PCA scores (arts r = .89, tie factors .70 to .80). Substituting CFA scores into the OLS changes results materially, so keep PCA scores for the OLS and report CFA-based results only as measurement validation.
- **Variety-only constraint test:** a df discrepancy (3 where about 15 is expected) remains unresolved in the constraint comparison. Do not cite it.

## 7. Technical Gotchas

- `psych::polychoric()` returns NaN on zero cells with the default continuity correction. Use `correct = 0` in the rotation step.
- `lavInspect(fit, "cor.all")` and `"cor.ov"` omit exogenous covariates under `fixed.x = TRUE`. Covariate-implied correlations must be read another way.
- `psych::polyserial()` fails on these inputs; use `polycor::polyserial(..., ML = TRUE)`.
- `semTools::compRelSEM(fit)` returns the omega for each factor directly (no `[["omega"]]` subscript).
- estimatr `lm_robust` objects: read `$coefficients`, `$std.error`, and `$p.value` directly. `summary()` fails on cached objects.
- `car::linearHypothesis` on these models returns `Pr(>Chisq)`, not `Pr(>F)`.
- `mods_canon$mdat` is a tibble with rownames 1..N. Align with `which(complete.cases(...))`, never rownames.
- `pgrep -f <script>` matches the calling shell. Use `ps aux | grep "[s]cript"`.
- Long R jobs: `nohup Rscript --no-save --no-restore script.R > log 2>&1 &`, then poll the log with `sleep`. Do not block the foreground shell.
- A shell variable is not visible to `Rscript -e` unless exported.
- The `renv` out-of-sync message in Rscript output is benign.

## 8. Open Items

- **Primary measurement model for the paper.** The reporting choice is the fixed-loading separate models, but the decision is still open (fixed-loading ESEM versus confirmatory versus PCA-based OLS).
- **OLS placement.** Tables A4 and A5 sit in the appendix. Moving them into the main results would renumber Tables 5 to 10.
- **Network predictor tables.** Tables 6 and 7 (network predictors) are not yet combined. They are a candidate for the same side-by-side layout as Table 8, but the strong-tie race coefficients on conservative composition differ enough to show.
- **Reproducibility (partly closed, November 9).** Tables 1 to 4, A1, A2 (row data), Tables 5 to 10, Table A3, Figures 5 to 10, and Tables A4 and A5 are generated from saved caches or refits by `Scripts/manuscript/build_measurement_blocks.R`, `Scripts/manuscript/build_structural_blocks.R`, and `Scripts/manuscript/build_ols_appendix.R`, and spliced by `Scripts/manuscript/splice_blocks.py` and `splice_ols_appendix.py`. Captions and notes remain typed in `results_draft.tex`. Prose numbers are still typed and are not yet checked against a script.
- **Discussion section.** The draft has no discussion yet. Any discussion that relies on the suppression result needs to reflect the measurement dependence.
- **Bootstrap.** Indirect effects are reported as delta-method estimates. Bootstrap them before presenting any as inferential claims.
- **Little-church residual.** Listed in the confirmatory antonym analysis but not interpreted. Decide whether to report it.
- **Figures and tables.** Verify the TikZ Figures 5 to 10 against the final structural text after any edit. Regenerated `Plots/` figures and table PNGs must be checked before they are used in any output.
- **Reviewer response.** The response-to-reviewers template (`response_to_reviewers_template.md`) predates these results and needs updating.
- **Manuscript sync.** The Google Doc does not match `results_draft.tex`. Sync only on request, with a fresh download and in-place edits.

## 9. Manuscript Build Conventions (November 9, 2026)

- **Compile:** `cd cache/diagnostics && xelatex -interaction=nonstopmode -output-directory <scratch> ../../results_draft.tex`. Check the log for `^!` errors and `Overfull` boxes. Render pages with `pdftoppm` to inspect tables. The draft is 19 pages.
- **Table fit:** the preamble sets `\small` for every `tabular*` and `longtable` (etoolbox) and `\setlength{\tabcolsep}{3pt}` for `tabular*`. Column widths should sum to about 0.94 to 0.96 of `\textwidth`. The `\hphantom` padding in coefficient cells widens cells and causes overfull boxes; remove it when a table overflows. Table A4 and A5 use `\arraystretch{0.8}` and a `\small` note.
- **Bold convention:** loading tables (Tables 1, 3, 4, A1) bold each item's assigned-factor loading. Each of those notes says so. Table A2 and the fit tables are not bolded.
- **Table 4 labels:** headers are `Lib.` and `Con.`; the note defines them and MENA and NHPI.
- **Structural table numbering:** Tables 5 (network to activity), 6 and 7 (network predictors, weak and strong), 8 (combined activity predictors, weak and strong), 9 and 10 (direct, mediated, and total effects). Figures 5 to 10 are inline TikZ and the captions are kept with the diagrams with `\Needspace`.
- **Backups:** each edit to `results_draft.tex` was preceded by a copy in `cache/diagnostics/results_draft_pre_*.tex`. The most recent pre-edit copy is `results_draft_pre_combine89.tex`.
- **Footnotes:** the geomin footnote was removed (no supporting analysis). The sparse-category caveat was moved into the main text. The footnote on the oblimin split-half check points to Table A3.

