# Factor Structure Diagnostics: PCA vs. CFA Scoring, Collinearity, and Pooled Models

Date: November 6, 2026
Scope: Diagnostic analysis prompted by SSR-D-26-01726 Reviewer 1 (reliability, CFA, robustness) and Reviewer 4 (PCA vs. latent variable scale construction, oblique rotation, skewed Likert indicators). Scripts are in `Scripts/diagnostics/`. Data: `grabbagdata111925.dta` (N = 1,258; complete-case subsets noted below).

## Summary

1. Replacing PCA scores with CFA scores changes the Table 1 and Table 2 results, and the changes depend on which side of the model is replaced. The outcome side drives the sign flip on strong-tie variety for solitary leisure. The composition block's loss of significance needs both sides replaced.
2. The collinearity among predictors is largely a property of the constructs. Latent correlations are high: .84 (weak liberal with conservative), .79 (weak variety with liberal), and .98 (weak with strong variety, using a proper two-factor model).
3. Within each battery, the liberal and conservative composition factors are statistically distinguishable (scaled chi-square difference tests, p < .001), though they share substantial variance.
4. Weak- and strong-tie variety are statistically distinguishable only marginally (scaled Δχ² = 7.47, df = 1, p = .006), and fit the same with one shared factor (CFI .898 vs. .901). The weak/strong contrast in the regressions is therefore weakly identified.
5. A pooled PCA across all 36 network items recovers variety, liberal, and conservative factors, with 54% variance explained. Its variety factor mixes weak and strong items.
6. A pooled CFA with battery method factors fits better than a pooled three-factor CFA, but the method factors correlate .79 with each other and absorb most of the shared variance. The pooled substantive factors are nearly uncorrelated only because of an orthogonality constraint.

## 1. Sensitivity of Tables 1 and 2 to Scoring Method

The four-arm comparison refits models 2 and 4 with PCA scores throughout, CFA predictors only, CFA outcome only, and CFA on both sides. The CFA scores are lavaan's default regression-type factor scores from ordinal WLSMV CFAs on the item structures from script 07. The sample is the 1,243 respondents complete on the activity and tie items, before covariate listwise deletion.

Solitary leisure, model 4:

| Arm | Weak variety b (p) | Strong variety b (p) | Weak liberal b (p) | Composition block χ² (p) |
|---|---|---|---|---|
| PCA throughout | 0.14 (.006) | -0.15 (.002) | 0.12 (.004) | 44.7 (< .001) |
| CFA predictors only | 0.00 (.99) | -0.23 (.007) | 0.25 (.04) | 37.5 (< .001) |
| CFA outcome only | 0.10 (.003) | +0.15 (< .001) | 0.02 (.59) | 27.7 (< .001) |
| CFA both sides | 0.19 (.001) | +0.22 (< .001) | -0.10 (.28) | 9.0 (.06) |

Arts participation, model 4:

| Arm | Weak variety b (p) | Strong variety b (p) | Weak liberal b (p) | Composition block χ² (p) |
|---|---|---|---|---|
| PCA throughout | 0.09 (.014) | 0.41 (< .001) | -0.12 (< .001) | 34.7 (< .001) |
| CFA predictors only | 0.52 (< .001) | 0.51 (< .001) | -0.68 (< .001) | 121.7 (< .001) |
| CFA outcome only | 0.09 (.008) | 0.27 (< .001) | -0.02 (.67) | 19.1 (< .001) |
| CFA both sides | 0.26 (< .001) | 0.41 (< .001) | -0.23 (.035) | 31.5 (< .001) |

Interpretation. For solitary leisure, changing only the outcome reverses the strong-tie variety sign, because the CFA leisure factor weights items differently from the PCA factor (exercise and reading weigh more). Changing only the predictors leaves the outcome unchanged. Its marginal correlations with the PCA leisure factor stay near zero (strong-tie variety r = .02 in the PCA arm), so the conditional coefficients move through collinearity. For arts, predictor-only CFA scores inflate the network coefficients and the composition chi-square, consistent with factor-score indeterminacy under correlated predictors.

Caveat. Each arm changes a named set of variables, but the both-sides arm is not a simple sum of the one-sided arms, so the source of the composition-block change cannot be assigned to a single variable.

## 2. Latent Correlations from the Ordinal CFAs

Standardized latent correlations from the WLSMV CFAs (script 07 fits):

| Model | Pair | r (SE) |
|---|---|---|
| Weak ties | Liberal with conservative | .84 (.01) |
| Weak ties | Variety with liberal | .79 (.01) |
| Weak ties | Variety with conservative | .62 (.02) |
| Strong ties | Variety with liberal | .76 (.02) |
| Strong ties | Variety with conservative | .71 (.02) |
| Strong ties | Liberal with conservative | .75 (.02) |
| Activities | Arts with leisure | .76 (.04) |
| Activities | Arts with residual | .68 (.03) |
| Activities | Leisure with residual | .59 (.05) |

The activity factors correlate .76 for arts and leisure, which conflicts with the orthogonal varimax solution in the current manuscript and supports Reviewer 1's concern about the leisure factor.

The PCA scores have zero within-battery correlations by construction, but the CFA factors do not. The collinearity among CFA-scored predictors is therefore a property of the measurement model.

## 3. Discriminant Checks on the Composition Factors

Within-battery comparison of the three-factor CFA against a merged two-factor model (liberal and conservative items on one composition factor):

| Battery | Scaled Δχ² (df = 2) | p | CFI, three-factor | CFI, merged | RMSEA, three-factor | RMSEA, merged |
|---|---|---|---|---|---|---|
| Weak | 223.0 | < .001 | .891 | .874 | .119 | .127 |
| Strong | 194.0 | < .001 | .918 | .899 | .105 | .115 |

The liberal and conservative composition factors are statistically distinguishable within each battery, although they share substantial variance (latent correlations .84 and .75). The difference tests use the Satorra-Bentler scaled-and-shifted statistic; the unadjusted chi-square column in `lavTestLRT()` output is not reportable on its own.

## 4. Weak vs. Strong Variety

A two-factor model (weak variety, strong variety), estimated within the same CFA framework, converged to a positive-definite solution with latent correlation .975 (SE .009). Constraining that correlation to 1 gives a scaled Δχ² of 7.47 (df = 1, p = .006), with CFI .898 for the two-factor model and .901 for the one-factor model.

The two variety factors are statistically distinct only at the margin with this sample size. The one-factor model fits as well, so the weak/strong contrast in the regressions rests on roughly 5% unique variance in each battery.

Note on the joint model. A joint weak-and-strong model with all six composition and variety factors did not reach a positive-definite covariance matrix (CFI .75, RMSEA .13). Its latent correlations (weak variety with strong variety .98; weak liberal with strong liberal .99; weak conservative with strong conservative .93) are indicative only.

## 5. Pooled PCA Across All Network Items

Varimax PCA on all 36 network items (N = 1,258 complete):

| Factor | Dominant items | r with weak score | r with strong score |
|---|---|---|---|
| RC1, variety | Hawaiian/Pacific Islander, American Indian, Middle Eastern, second home, born elsewhere (both batteries) | .90 (weak variety) | .96 (strong variety) |
| RC2, liberal | Very liberal, LGBTQ, city, Hispanic, Asian, Black, men and women | .97 (weak liberal) | .78 (strong liberal) |
| RC3, conservative | Very conservative, church (both), rural, white, men and women | .91 (weak conservative) | .95 (strong conservative) |

Variance explained: 54.1% (compared with about 61% for each battery separately). The pooled scores are orthogonal by construction. The variety factor mixes weak and strong items, and the liberal factor mixes ethnic and racial items with ideological items, the same mixing Reviewer 1 flagged in the original network PCA.

## 6. Pooled CFA with Battery Method Factors

Pooled three-factor CFA (variety, liberal, conservative, items from the script 07 structures) against the same model with one method factor per battery loading on all of that battery's items. Method factors are constrained orthogonal to the substantive factors. N = 1,258, WLSMV.

| Model | CFI (scaled) | RMSEA (scaled) |
|---|---|---|
| Three pooled factors | .738 | .130 |
| Three pooled factors plus battery method factors | .813 | .113 |

Adding the method factors improves fit significantly (scaled Δχ² = 2,579, df = 37, p < .001). Both models converged to positive-definite solutions.

With method factors included, the substantive correlations are variety with liberal .04 (SE .12), variety with conservative .26 (SE .03), and liberal with conservative -.03 (SE .09). The two method factors correlate .79 with each other, and their standardized loadings range from .33 to .86.

Interpretation. The method factors are largely one shared factor across the two batteries, which may reflect repeated-respondent response tendencies or a general network-breadth factor. The model cannot distinguish these. Because the method factors are orthogonal to the substantive factors, they absorb the variance the substantive factors share, so the near-zero substantive correlations are partly a constraint artifact. The pooled analysis does not support treating the substantive factors as well defined on their own.

## 7. Implications for the Response to Reviewers

1. PCA scores remain defensible as the regression inputs under the current specification, but the response should disclose the sensitivity in Section 1 rather than presenting the PCA coefficients as stable.
2. The liberal/conservative composition distinction is statistically supported within each battery, and the response can say so, while noting shared variance.
3. The weak/strong variety contrast is not well identified. The discussion and abstract should not lean on it as an established difference without that qualification.
4. Reviewer 4's request for a latent variable scale construction is only partly met by the CFA. Scores from the CFA change conclusions (Section 1), and the pooled models do not yield a cleaner alternative (Sections 5 and 6).
5. The defensible framing is that the network items share a large common component, and the paper's substantive claims about composition and tie strength are less precise than currently written.

## 8. Limitations

- CFA scores are lavaan's default regression-type factor scores, not MAP or EAP scores. Results may differ under other score methods.
- The four-arm comparison uses 1,243 complete cases before covariate deletion, so coefficients differ slightly from the canonical `cache/02_models.rds` values (e.g., strong-tie variety on leisure is -0.15 here versus -0.13 in the canonical pipeline).
- The joint weak-and-strong CFA and the pooled CFA with method factors had convergence or identification concerns; their numbers are indicative.
- The method-factor constraints (orthogonality to substantive factors) were a modeling choice; alternatives (a general factor with orthogonal composition factors) were not tested.
- Reliability and fit statistics here do not replace the omega and CFA results in `cache/07_reliability_cfa.rds`.

## 9. Nested Factor Model Comparison

Seven nested or related specifications were fit with WLSMV on the 36 network items (N = 1,258), and again with the 8 men and women items removed (32 items, same N). Model 2 is the pooled three-factor model (variety, liberal, conservative). Models 4 to 7 use a general factor loading on all items. Model 4 fixes the liberal–conservative covariance at zero, and model 5 frees it. Models 6 and 7 add 16 or 18 weak–strong residual covariances for the same group (the gender pairs are dropped in the no-gender condition, as the model has no gender items to pair). The residual covariances were chosen by inspecting misfit in these data, so their fit gains are optimistic.

| Model | df (all items) | CFI | RMSEA | SRMR | df (no gender) | CFI | RMSEA | SRMR |
|---|---|---|---|---|---|---|---|---|
| 1. One general factor | 594 | .629 | .154 | .147 | 464 | .627 | .163 | .143 |
| 2. Pooled 3-factor, correlated | 591 | .738 | .130 | .117 | 461 | .763 | .130 | .111 |
| 3. Pooled 3-factor + battery method factors | 554 | .813 | .113 | .087 | 428 | .819 | .118 | .086 |
| 4. Bifactor, composition orthogonal | 571 | .743 | .131 | .121 | 445 | .781 | .128 | .106 |
| 5. Bifactor, composition correlated | 570 | .752 | .129 | .108 | 444 | .781 | .127 | .103 |
| 6. Bifactor orthogonal + weak–strong residuals | 553 | .817 | .112 | .107 | 429 | .864 | .102 | .091 |
| 7. Bifactor correlated + weak–strong residuals | 552 | .851 | .101 | .088 | 428 | .872 | .099 | .084 |
| 8. Bifactor correlated + battery method factors | 533 | .824 | .112 | .069 | 411 | .831 | .117 | .075 |
| 9. Bifactor correlated + battery method factors + residuals | 515 | .960 | .054 | .042 | 395 | .958 | .059 | .044 |

Model 8 adds one method factor per battery, loading on that battery's items and constrained orthogonal to the general and composition factors. Model 9 adds the weak–strong residuals to model 8. The method factors in models 8 and 9 correlate .77 (all items) and .82 (no gender) in model 8, and .71 in model 9 under both conditions.

Nested comparisons (scaled Satorra–Bentler differences):

| Comparison | Δχ² (all items, df) | Δχ² (no gender, df) |
|---|---|---|
| 4 → 5, free liberal–conservative covariance | 198.3 (1), p < .001 | 72.5 (1), p < .001 |
| 4 → 6, add residuals to orthogonal model | 3172.1 (18), p < .001 | 2714.6 (16), p < .001 |
| 5 → 7, add residuals to correlated model | 3490.2 (18), p < .001 | 2769.9 (16), p < .001 |
| 6 → 7, free covariance given residuals | 334.5 (1), p < .001 | 102.5 (1), p < .001 |
| 8 → 9, add residuals to method model | 3840.9 (18), p < .001 | 2879.2 (16), p < .001 (not admissible; see below) |

Models 7 and 8 are not nested in each other, so their difference is not tested. Model 7 is nested in model 9, but I haven't reported that test separately.

Caveats on models 8 and 9. Model 9 fits well in both conditions, but its residual covariances and battery method factors absorb overlapping cross-battery covariance, so it has many partly redundant parameters. The no-gender version of model 9 produced a non-positive-definite latent covariance matrix, so its fit values and the 8 → 9 test in that condition should not be interpreted. The fit gains in model 9 have not been checked on held-out data. Verifying them is pending a split-sample analysis.

Interpretation. Freeing the liberal–conservative covariance improves fit in both conditions, so the orthogonal bifactor is rejected. The weak–strong residual covariances are the largest single improvement, and model 7 fits best in both conditions. Removing gender items raises CFI for the best model from .851 to .872 and lowers RMSEA from .101 to .099. No model reaches conventional fit thresholds. All models converged to positive-definite solutions, and the admissibility check on the best models passed. The residual covariances should be checked on a split sample before the fit gains are reported.

Scripts: `Scripts/diagnostics/nested_table.R` (this table), `Scripts/diagnostics/bifactor_resid.R` (residual covariances with gender items), and `Scripts/diagnostics/bifactor_nogender_full.R` (no-gender models).

## 10. Liberal–Conservative Correlation Across Models

Liberal and conservative composition correlate at a level that depends on which battery-level terms are in the model. The table below shows fit and that correlation for the bifactor family in both gender conditions. Models 5 to 9 are as defined in Section 9.

| Model | Condition | CFI | RMSEA | Liberal–conservative r |
|---|---|---|---|---|
| 5. Bifactor, correlated | All items | .752 | .129 | .52 |
| 6. Bifactor, orthogonal + residuals | All items | .817 | .112 | .00 (fixed) |
| 7. Bifactor, correlated + residuals (script 07 sorting) | All items | .851 | .101 | .66 |
| 7. Bifactor, correlated + residuals (pooled-EFA sorting) | All items | .848 | .102 | .62 |
| 8. Bifactor, correlated + method factors | All items | .824 | .112 | −.34 |
| 9. Bifactor, correlated + method factors + residuals | All items | .960 | .054 | −.94 |
| 5. Bifactor, correlated | No gender | .781 | .127 | .25 |
| 6. Bifactor, orthogonal + residuals | No gender | .864 | .102 | .00 (fixed) |
| 7. Bifactor, correlated + residuals (script 07 sorting) | No gender | .872 | .099 | .40 |
| 7. Bifactor, correlated + residuals (pooled-EFA sorting) | No gender | .867 | .101 | .37 |
| 8. Bifactor, correlated + method factors | No gender | .831 | .117 | −.12 |
| 9. Bifactor, correlated + method factors + residuals | No gender | .958 | .059 | −1.30 (not admissible) |

The pooled-EFA rows use a 3-factor pooled EFA (polychoric, oblimin, WLS) on all 36 items, assigning each item to whichever of factors 2 (liberal) or 3 (conservative) it loads on highest. Relative to the script 07 sorting, three items change: weak KnowBornElsewhere (liberal → variety), weak Knowwomen (conservative → liberal), and strong KnowCityppl (liberal → variety). Changing the sorting shifts CFI by at most .005 and RMSEA by .002, and the liberal–conservative correlation by .04 (all items) and .03 (no gender). Script: `Scripts/diagnostics/model7_pooled_sorting.R`.

Interpretation. Model 9 has the best fit but implies a near-perfect negative correlation between the liberal and conservative factors, which is not a substantive pattern and makes the two composition factors nearly one bipolar dimension. Adding the battery method factors moves the correlation from positive to negative, so the method factors absorb the shared composition variance. Model 9 should not be used to interpret the composition structure.

Among models that keep the liberal and conservative correlation positive, model 7 has the best fit. Its no-gender version (CFI .872, RMSEA .099) gives a moderate correlation of .40, which is the more defensible reading. The all-items version gives .66, high enough to question whether the two factors are separable. Model 7's residual covariances were chosen from misfit in these data, so its fit gain should be validated on held-out data before it is reported.

Scripts: `Scripts/diagnostics/moderate_lc.R` (this table), `Scripts/diagnostics/model9_pe.R` (model 9 latent correlations and loading ranges), and `Scripts/diagnostics/plot_model9.R` (semPlot attempt, not used in the report because the diagram was unreadable).

### Why the gender items are problematic

The gender items ask how many men and how many women the respondent knows, and they differ from the other network items in ways that matter for the model. First, they sort inconsistently. In the weak-tie battery, knowing men loads on the liberal factor and knowing women on the conservative factor, even though the two are the same kind of question. The pooled-EFA sorting then moves the weak women item to the liberal side. Second, they fit worst. The gender items show the largest misfit against the rest of the structure, and their answers are not well explained by the other items. Third, dropping them helps. Removing the four gender items raises fit slightly and lowers the liberal–conservative correlation from .66 to .40, which suggests they were pulling the two composition factors together.

A plausible reading is that these items measure something other than the worldview content the rest of the battery captures, such as the gender mix of a respondent's social circle or how the respondent's own gender shapes whom they know. The data show that the gender items behave differently but cannot establish why, so that explanation is a hypothesis. For the revision, the gender items should be reported as a separate question, presented as a sensitivity check, or justified on their own terms, rather than mixed into the ideological composition measures.

## Reproducibility

| Script | Purpose |
|---|---|
| `Scripts/diagnostics/verify_cfa_scores.R` | Four-arm PCA vs. CFA refits of models 2 and 4 (Section 1) |
| `Scripts/diagnostics/diag_signs2.R` | Predictor and bivariate correlations under each scoring method (Section 1) |
| `Scripts/diagnostics/latent_cor.R` | Latent correlations from the script 07 CFA fits (Section 2) |
| `Scripts/diagnostics/discrim.R` | Within-battery three-factor vs. merged comparison, joint weak-and-strong CFA (Sections 3 and 4) |
| `Scripts/diagnostics/lrt.R` | Scaled chi-square difference tests for the within-battery comparison (Section 3) |
| `Scripts/diagnostics/variety_test.R` | Weak vs. strong variety, one factor vs. two (Section 4) |
| `Scripts/diagnostics/pooled_pca.R` | Pooled PCA over all network items (Section 5) |
| `Scripts/diagnostics/pooled_cfa.R` | Pooled CFA with battery method factors (Section 6) |

All scripts are run from the project root with `Rscript`. They read `cache/01_prepared_data.rds` and `cache/07_reliability_cfa.rds` and do not write to the canonical cache.

## 11. Model 7 Network Factors Predicting Activity Factors: Scoring Comparison

Outcomes are the two activity CFA factors (arts participation, solitary leisure; 17 items, two-factor model). Predictors are the three model 7 factors (no-gender condition, script 07 sorting): the general factor G and the specific liberal (L) and conservative (C) factors. Covariates match Tables 1 and 2. Three estimators are compared. Regression and Bartlett are score-based OLS with HC1 errors. The SEM is a latent-variable WLSMV model in which the activity factors are regressed directly on the network factors, with the measurement model estimated jointly. Coefficients are b (SE) with p in brackets. Regression and Bartlett are on score scales; the SEM is on latent scales with unit variance, so compare direction and significance rather than magnitude.

| Outcome | Method | G | L | C | N |
|---|---|---|---|---|---|
| Arts | Regression | 0.41 (0.03) [< .001] | −0.12 (0.03) [< .001] | −0.01 (0.03) [.77] | 1,227 |
| Arts | Bartlett | 0.09 (0.03) [.007] | −0.12 (0.11) [.28] | 0.05 (0.05) [.34] | 1,200 |
| Arts | SEM | 0.60 (0.04) [< .001] | −0.29 (0.05) [< .001] | 0.03 (0.05) [.56] | 1,227 |
| Leisure | Regression | 0.29 (0.02) [< .001] | −0.04 (0.03) [.11] | 0.00 (0.02) [.94] | 1,227 |
| Leisure | Bartlett | 0.02 (0.02) [.24] | 0.07 (0.06) [.26] | −0.01 (0.06) [.90] | 1,200 |
| Leisure | SEM | 0.26 (0.05) [< .001] | 0.19 (0.08) [.02] | −0.03 (0.08) [.70] | 1,227 |

The general factor is positive and significant for both outcomes under regression and SEM. Its Bartlett estimate is much smaller and not significant for leisure, so G's direction is stable across methods while its magnitude is not. The liberal factor is negative for arts under regression and SEM, but imprecise under Bartlett (p = .28), so the arts result is more consistent than the Bartlett estimate alone suggests, though still method-dependent.

Sign change for leisure. The liberal factor's effect on solitary leisure reverses direction across methods. It is −0.04 (n.s.) under regression, +0.07 (n.s.) under Bartlett, and +0.19 (p = .02) under the SEM. The SEM estimate is the only significant positive liberal effect in any specification. The sign change is not explained by sample differences, since the regression and SEM use the same 1,227 cases. Measurement error correction in the SEM is the most likely source, because the leisure factor is weakly identified (walking loads .19), and the attenuation that regression scores impose would be largest for a weakly measured outcome. This explanation has not been tested directly, so the leisure liberal effect should not be reported as a finding until it is checked.

Script: `Scripts/diagnostics/model7_sem_activities.R`.

## 12. Pooled vs Per-Battery Oblimin Loadings

Source: `Scripts/diagnostics/oblimin_loading_comparison.R` (output in `cache/diagnostics/oblimin_loading_comparison.md`). Three oblimin EFAs (polychoric, WLS): one on all 36 network items, and separate ones on the weak and strong batteries. Each battery's 18 items appear as separate rows in the pooled solution, so the pooled loadings are reported separately for the weak-tie (Pooled-W) and strong-tie (Pooled-S) rows.


| Item | Pooled-W V | Pooled-W L | Pooled-W C | Pooled-S V | Pooled-S L | Pooled-S C | Weak V | Weak L | Weak C | Strong V | Strong L | Strong C |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| KnowHawaiiPI | 0.80 | 0.12 | -0.05 | 0.90 | -0.12 | 0.02 | 0.91 | -0.07 | 0.02 | 0.93 | -0.04 | -0.09 |
| KnowAmIndianppl | 0.71 | 0.04 | 0.06 | 0.84 | -0.20 | 0.15 | 0.81 | -0.11 | 0.14 | 0.84 | -0.12 | 0.03 |
| KnowMENAppl | 0.71 | 0.27 | -0.05 | 0.88 | -0.05 | 0.04 | 0.80 | 0.11 | 0.01 | 0.89 | 0.03 | -0.03 |
| KnowAsianppl | 0.56 | 0.44 | -0.07 | 0.73 | 0.10 | 0.03 | 0.62 | 0.33 | -0.04 | 0.71 | 0.15 | 0.03 |
| KnowBornElsewhere | 0.46 | 0.46 | -0.00 | 0.68 | 0.13 | 0.06 | 0.49 | 0.40 | -0.03 | 0.67 | 0.17 | 0.07 |
| Know2ndHome | 0.44 | 0.11 | 0.32 | 0.59 | -0.14 | 0.37 | 0.52 | 0.03 | 0.34 | 0.56 | -0.15 | 0.28 |
| KnowBlackppl | 0.36 | 0.45 | 0.12 | 0.60 | 0.07 | 0.20 | 0.39 | 0.43 | 0.08 | 0.57 | 0.13 | 0.21 |
| KnowHispanixppl | 0.32 | 0.62 | -0.05 | 0.62 | 0.21 | 0.01 | 0.35 | 0.57 | -0.09 | 0.60 | 0.24 | 0.07 |
| KnowCityppl | 0.28 | 0.55 | 0.03 | 0.42 | 0.29 | 0.13 | 0.31 | 0.51 | -0.02 | 0.34 | 0.30 | 0.28 |
| KnowWhiteppl | -0.20 | 0.54 | 0.61 | 0.05 | 0.13 | 0.69 | -0.12 | 0.61 | 0.47 | -0.12 | 0.13 | 0.87 |
| KnowLGBTQ | 0.20 | 0.73 | -0.15 | 0.40 | 0.45 | -0.10 | 0.15 | 0.73 | -0.23 | 0.34 | 0.58 | 0.05 |
| Knowmen | -0.13 | 0.70 | 0.44 | 0.20 | 0.26 | 0.52 | -0.09 | 0.78 | 0.27 | 0.03 | 0.31 | 0.73 |
| KnowLotsaChurch | 0.11 | 0.18 | 0.60 | 0.32 | -0.15 | 0.68 | 0.26 | 0.15 | 0.59 | 0.26 | -0.23 | 0.65 |
| KnowVeryLib | 0.10 | 0.78 | -0.05 | 0.29 | 0.50 | 0.02 | 0.08 | 0.77 | -0.13 | 0.19 | 0.65 | 0.21 |
| KnowLittleChurch | -0.09 | 0.65 | 0.22 | 0.12 | 0.35 | 0.33 | -0.02 | 0.67 | 0.12 | 0.02 | 0.35 | 0.50 |
| KnowRuralppl | -0.08 | 0.19 | 0.56 | 0.08 | -0.05 | 0.63 | 0.02 | 0.23 | 0.48 | 0.01 | -0.03 | 0.62 |
| Knowwomen | -0.05 | 0.60 | 0.44 | 0.21 | 0.17 | 0.59 | 0.03 | 0.64 | 0.34 | 0.09 | 0.12 | 0.74 |
| KnowVeryCons | -0.00 | 0.07 | 0.72 | 0.21 | -0.27 | 0.80 | 0.17 | 0.00 | 0.80 | 0.14 | -0.37 | 0.76 |

The pooled-W columns match the earlier comparison. Pooled-S shows that the same item can load differently in the pooled solution depending on which battery row is considered, most visibly for Hispanic/Latine, Black, and City, which load on pooled liberal on the weak row but on pooled variety on the strong row.

## 13. Cross-Loadings, Activity Structure, and Leisure Sensitivity

This section collects the cross-loading and leisure-outcome analyses run after Section 12. Every number below comes from the scripts listed at the end. Cross-loadings were flagged from the battery-wise oblimin EFAs unless noted. The network factors are the model 7 no-gender bifactor (G, L, C), and the outcomes are the activity factors. The freed network loadings at .30 were White (weak, on liberal, 0.61), Little church (strong, on conservative, 0.50), and Very conservative (strong, on liberal, −0.37).

### 13.1 Freeing cross-loadings in model 7

| Condition | Flag source | Items freed | CFI | RMSEA | SRMR | L–C r (SE) |
|---|---|---|---|---|---|---|
| No gender | Baseline | none | .872 | .099 | .084 | .40 (.04) |
| No gender | Per-battery EFA | Weak White; strong Little church; strong Very conservative | .889 | .093 | .077 | .28 (.04) |
| No gender | Pooled EFA | Weak White; strong Little church | .883 | .095 | .080 | .20 (.04) |
| All items | Baseline | none | .851 | .101 | .088 | .66 (.02) |
| All items | Per-battery EFA | Weak men, White, women; strong Little church, Very conservative, men | .874 | .094 | .081 | .43 (.03) |
| All items | Pooled EFA | Weak men, White, women; strong Little church | .875 | .093 | .081 | .27 (.04) |

Freeing the secondary loadings improves fit by a small amount but lowers the liberal–conservative correlation substantially. The pooled and per-battery flags differ only in whether strong Very conservative (per-battery −0.37; pooled below threshold) and strong men (per-battery 0.31; pooled below threshold) are freed. Neither difference was tested with a scaled chi-square difference test, and the flags were chosen from the same sample.

### 13.2 Activity CFA with cross-loadings freed

Three-factor activity model (20 items, arts / leisure / residual). The oblimin EFA did not give a separable residual factor: the residual anchors (gardening, home repair) load on the arts column, so the flagged list is not interpretable, and the freed model did not converge to a valid solution (CFI not computed, negative covariance eigenvalue).

Two-factor activity model (17 items, arts / leisure, with reading):

| Specification | CFI | RMSEA | SRMR | Arts–leisure r |
|---|---|---|---|---|
| Baseline | .962 | .102 | .058 | .77 |
| Freed at .30 (exercise on arts .40; reading on arts .35) | .966 | .098 | .052 | .04 |
| Freed at .40 (exercise on arts only) | .963 | .102 | .057 | .50 |
| Freed at .50 (no item qualifies) | .962 | .102 | .058 | .77 |

The arts–leisure correlation depends on whether reading's cross-loading is freed. Freeing reading lowers the correlation to near zero, while freeing exercise alone lowers it to .50.

Two-factor activity model without reading (16 items, leisure = walking and exercise):

| Specification | CFI | RMSEA | SRMR | Arts–leisure r |
|---|---|---|---|---|
| Baseline | .971 | .094 | .049 | .39 |
| Freed at .30 (exercise on arts, 0.39) | not computed | not computed | .049 | .11 |
| Freed at .40 (no item qualifies) | .971 | .094 | .049 | .39 |

Freeing exercise in the no-reading model produces a non-positive-definite covariance matrix, so that row is not interpretable. The baseline also shows negative residual variances, so the two-item leisure factor is not a clean solution.

### 13.3 Leisure liberal effect across specifications

SEM with N = 1,227, the same covariates as Tables 1 and 2, and network factors from model 7 (no gender).

| Leisure outcome | Network freed at .30 | Liberal b | p |
|---|---|---|---|
| Walk + exercise + reading (3 items) | No | 0.19 | .02 |
| Walk + exercise + reading (3 items) | Yes | 0.16 | .02 |
| Walk + exercise (2 items) | No | 0.06 | .33 |
| Walk + exercise (2 items) | Yes | 0.04 | .40 |
| Walk + exercise + hiking/camping (3 items) | No | 0.06 | .34 |
| Walking only (observed ordinal outcome, latent-response scale) | No | 0.27 | < .001 |

Freeing the network cross-loadings changes the leisure liberal estimate by at most .03 in every specification. The leisure liberal effect is therefore not a product of the cross-loading choice.

Reading is not the sole driver of the positive estimate. Walking alone shows a clear positive liberal association, and combining walking with exercise weakens it because exercise carries most of the two-item factor's loading (exercise .59 on leisure, walking .19 in the CFA). Adding reading restores a positive estimate. The earlier explanation that reading drives the effect is therefore withdrawn. The leisure liberal association depends on the leisure item set, and it should not be reported as a robust finding.

The single-item estimate is on a latent-response scale for an ordered observed outcome, so its magnitude is not comparable with the factor-based estimates. Its direction and significance are the only comparable quantities.

### 13.4 Scripts

- `Scripts/diagnostics/model7_crossloadings.R` (per-battery flags, Section 13.1; saves `cache/diagnostics/model7_crossloadings.rds`)
- `Scripts/diagnostics/model7_crossloadings_pooled.R` (pooled flags, Section 13.1; saves `cache/diagnostics/model7_crossloadings_pooled.rds`)
- `Scripts/diagnostics/activities_crossloadings.R` (three-factor activity attempt, Section 13.2)
- `Scripts/diagnostics/activities_crossloadings_2f.R` (two-factor activity model with reading, Section 13.2; saves `cache/diagnostics/activities_crossloadings_2f.rds`)
- `Scripts/diagnostics/activities_twofactor_noreading.R` (two-factor activity model without reading, Section 13.2)
- `Scripts/diagnostics/model7_sem_activities_freed.R` (leisure SEM with reading, with and without freed loadings, Section 13.3)
- `Scripts/diagnostics/model7_sem_activities_noreading.R` (leisure SEM without reading, with and without freed loadings, Section 13.3)
- `Scripts/diagnostics/leisure_outcome_alternatives.R` (walking-only and alternative three-item outcomes, Section 13.3)

### 13.5 Demographic Effects, Direct and Indirect (Marker Scaling)

Script: `Scripts/diagnostics/model7_mediation_sem.R` (with reading) and `Scripts/diagnostics/model7_mediation_sem_noreading.R`. Demographics predict the network factors (G, L, C) and both activity factors. Latent factors use marker scaling (`std.lv = FALSE`, first indicator fixed to 1), so arts and leisure effects are in the marker item's metric. Compare direction and significance across outcomes, not magnitudes. Total = direct + sum of indirect paths through G, L, and C. Model CFI .861, RMSEA .060.

Arts participation (with reading):

| Predictor | Direct | Via G | Via L | Via C | Total |
|---|---|---|---|---|---|
| Some college | 0.39 (p < .001) | 0.04 | −0.11 | −0.04 | 0.28 (p = .001) |
| College degree | 0.67 (p < .001) | 0.26 (p < .001) | −0.03 | 0.00 | 0.89 (p < .001) |
| Graduate degree | 0.66 (p < .001) | 0.37 (p < .001) | 0.06 | 0.01 | 1.09 (p < .001) |
| Black | −0.36 (p < .001) | 0.15 (p < .001) | 0.15 (p = .001) | 0.22 (p < .001) | 0.15 (p = .05) |
| Hispanic/Latine | −0.27 (p = .06) | 0.13 (p = .02) | 0.01 | 0.23 (p < .001) | 0.09 (p = .40) |
| AAPI | −0.78 (p < .001) | 0.07 | 0.13 (p = .04) | 0.29 (p < .001) | −0.30 (p = .006) |
| Multiracial/other | −0.34 (p < .001) | 0.07 (p = .08) | 0.04 | 0.16 (p < .001) | −0.08 (p = .27) |
| Politics (per point) | −0.03 (p = .18) | 0.02 (p = .02) | 0.11 (p < .001) | −0.05 (p < .001) | 0.05 (p < .001) |

Solitary leisure (with reading), direct and total only:

| Predictor | Direct | Total |
|---|---|---|
| Some college | 0.13 (p = .005) | 0.15 (p = .002) |
| College degree | 0.28 (p < .001) | 0.32 (p < .001) |
| Graduate degree | 0.27 (p < .001) | 0.32 (p < .001) |
| Black | 0.00 | −0.01 |
| Hispanic/Latine | −0.02 | −0.01 |
| AAPI | −0.06 | −0.08 |
| Multiracial/other | 0.02 | 0.01 |
| Politics (per point) | −0.01 | −0.01 (p = .06) |

Education effects on arts run mostly through direct paths, with the graduate and college effects also carried by variety (G). For race, lower conservative composition among Black, Hispanic/Latine, and AAPI respondents raises arts participation through C, which offsets a large negative direct effect, so the total race effects are small. Politics works through liberal composition (positive) and conservative composition (negative), which largely cancel. For leisure, race and politics have no meaningful total effect.

Mediation paths are cross-sectional, so the indirect effects assume no unmeasured confounding of network composition with arts participation beyond the measured covariates.

### 13.6 Direction of Effects: Networks → Activities vs Activities → Networks

Same data, same measurement model, same covariates, and the same number of free paths between the network and activity blocks (386 with reading, 379 without). Both are marker-scaled WLSMV models.

| Item set | Direction | χ² (scaled) | df | CFI | RMSEA | SRMR |
|---|---|---|---|---|---|---|
| With reading | Networks → activities | 8874.0 | 1657 | .861 | .060 | .075 |
| With reading | Activities → networks | 9087.7 | 1657 | .857 | .060 | .076 |
| Without reading | Networks → activities | 8732.3 | 1597 | .861 | .060 | .076 |
| Without reading | Activities → networks | 8437.3 | 1597 | .867 | .059 | .073 |

With reading, networks → activities fits slightly better (Δχ² = 214, same df). Without reading, activities → networks fits better (Δχ² = 295). The preferred direction therefore changes with the leisure item set. Both models have poor absolute fit (χ² ≫ df, CFI ≈ .86).

The two models have the same number of paths, so the χ² difference is descriptive. Because the data are cross-sectional, reversing the direction of paths between blocks does not change the fit when the blocks are saturated, and the differences here arise from the measurement constraints (orthogonal G, fixed residual covariances) rather than from causal ordering. The comparison therefore does not establish that networks cause activities or the reverse. Scripts: `Scripts/diagnostics/model7_direction_test.R` and `Scripts/diagnostics/model7_direction_test_noreading.R`.

### 13.7 Demographics Predicting Both Network Factors and Activities

Specification: the marker-scaled mediation SEM in 13.5 (with reading, N = 1,227, CFI .861, RMSEA .060), in which demographics predict G, L, and C and also the two activity factors directly. The no-reading specification gives nearly identical network-on-demographic paths, so only the with-reading values are shown. Estimates are in the marker metric, so they differ from the standardized-latent values in Section 11 (for example, the college-degree path to G is 0.44 here and 0.62 under standardized latents). Compare direction and significance, not magnitude, against the earlier tables.

| Demographic | G (variety) | L (liberal) | C (conservative) |
|---|---|---|---|
| Men (vs. women) | −0.01 (n.s.) | −0.13 (p = .001) | −0.12 (p = .002) |
| Non-binary (vs. women) | −0.27 (n.s.) | −0.09 (n.s.) | −0.24 (n.s.) |
| Some college (vs. none) | 0.06 (n.s.) | 0.17 (p = .02) | 0.09 (n.s.) |
| College degree | 0.44 (p < .001) | 0.04 (n.s.) | 0.00 (n.s.) |
| Graduate degree | 0.63 (p < .001) | −0.09 (n.s.) | −0.02 (n.s.) |
| Childhood arts (per unit) | 0.15 (p < .001) | −0.03 (p = .02) | −0.04 (p < .001) |
| Income | 0.01 (n.s.) | 0.01 (p = .02) | 0.01 (n.s.) |
| Age squared | −0.006 (p < .001) | 0.003 (p = .01) | 0.004 (p = .002) |
| Black (vs. White) | 0.26 (p < .001) | −0.23 (p < .001) | −0.51 (p < .001) |
| Hispanic/Latine | 0.22 (p = .02) | −0.01 (n.s.) | −0.55 (p < .001) |
| AAPI | 0.11 (n.s.) | −0.21 (p = .04) | −0.68 (p < .001) |
| Multiracial/other | 0.12 (p = .08) | −0.06 (n.s.) | −0.37 (p < .001) |
| Politics (per point) | 0.03 (p = .02) | −0.17 (p < .001) | 0.11 (p < .001) |

Education is associated with broader networks (G), not with more conservative or liberal composition. Race is associated with less conservative composition across all three non-White groups, and with less liberal composition for Black and AAPI respondents. Politics has opposing associations with liberal (negative) and conservative (positive) composition, the expected pattern for a respondent's own ideology. Men report less of both ideological composition types.

These paths describe how demographics relate to the composition of networks, not how composition is produced. The paths are conditional on the other covariates and on the activity equations, and they are cross-sectional. The fit cost of adding them is small (CFI .868 to .861). Scripts: `Scripts/diagnostics/model7_mediation_sem.R` and `Scripts/diagnostics/model7_mediation_sem_noreading.R`.
