# Reliability and Sensitivity Analysis: Activities Factor Model

**Project:** Beyond Network Variety in Arts Attendance (SSR-D-26-01726)
**Date:** November 6, 2026
**Scripts:** `Scripts/07_measurement_reliability_cfa.R` (reliability and baseline CFA), `Scripts/08_solitary_robustness.R` (operationalization sensitivity)
**Outputs:** `cache/07_reliability_cfa.rds`, `cache/08_solitary_robustness.rds`, `cache/tableR1_reliability.md`, `cache/tableR2_cfa_fit.md`, `cache/tableR3_solitary_robustness.md`

## 1. Motivation

The measurement-model requests from Reviewer 1 (ordinal CFA) and Reviewer 4 (polychoric/WLSMV as a defensible option), together with the reliability request (McDonald's omega with Cronbach's alpha as a secondary statistic), apply to all three measurement models in the paper. This report isolates the activities battery: the twenty practice-frequency items underlying Public Arts Participation, Solitary Leisure, and the residual DIY-practical dimension. The tie batteries are covered in the companion report (`tie_cfa_sensitivity_report.md`). The activities model is where the paper's inferential weight rests most heavily, and it is also home to the measurement model's weakest link, the three-item solitary-leisure factor, whose reliability and robustness receive the bulk of the attention below.

## 2. Baseline Specification and Reliability

The factor structure matches the paper's varimax PCA solution, with items assigned to their highest-loading factor: fourteen items on arts participation, three on solitary leisure (go_for_walk, exercise_or_yoga, read_novel_poem_or_play), and three on the residual DIY-practical contrast (gardening, home_auto_repair, fast_food with a negative loading). One assignment deserves note: the reading item loads with solitary leisure rather than with arts participation, so the confirmatory model follows the data rather than an a priori grouping.

McDonald's omega from the fitted ordinal CFA is .957 for arts participation (alpha .940), .389 for solitary leisure (alpha .428), and .545 for the residual contrast (alpha .382). The two low values call for different treatments. The residual factor's omega is not a meaningful evaluative standard because that dimension is a statistical residual used only as a discriminant-validity check; its internal structure is exactly what a contrast factor should look like, with gardening and home repair hanging together (polychoric r = .47) while fast_food sits at the negative pole with near-zero correlations to the other items (r = −.09 and .01) and a loading of only .20. The solitary-leisure factor is the genuine concern and is treated in Sections 4 and 5.

## 3. Ordinal CFA Fit (WLSMV, Polychoric)

The three-factor activities model was fit in `lavaan` with WLSMV estimation on the polychoric matrix (N = 1,243; the fifteen missing responses on the activity items are handled by lavaan's pairwise treatment under WLSMV). Standardized loadings range from .20 to .90, with the .20 belonging to fast_food on the residual contrast and the remaining substantive loadings sitting in the .50 to .90 range.

| Statistic | Value |
|---|---:|
| chi-square (scaled) | 2051.2 |
| df | 167 |
| CFI | 0.956 |
| TLI | 0.950 |
| RMSEA | 0.095 [0.092, 0.099] |
| SRMR | 0.061 |

The model meets conventional thresholds on CFI and SRMR, with RMSEA at the high edge of the acceptable range. Against the usual CFI ≥ .95, RMSEA ≤ .08 combination, the activities model is defensible but not spotless, and the response can report it as such: the factor structure underlying the paper's main outcome is well supported, with the RMSEA elevation the honest qualification.

## 4. The Solitary-Leisure Reliability Problem

The solitary-leisure factor's three items intercorrelate at only .18 to .34 (polychoric), and go_for_walk loads at just .21 in the confirmatory model. The weak common dimension, not item scarcity alone, is what drives omega to .39. Because factor-score noise attenuates regression coefficients toward zero, the consequence for Table 2 is directional: existing significant effects survive despite the noise and are therefore if anything conservative, but null findings against this outcome are correspondingly weak evidence. Any write-up of null results for this outcome should carry that qualification.

## 5. Operationalization Sensitivity Analysis

Table 2's conclusions were re-examined under three alternative operationalizations of solitary leisure, holding the specification, sample (the N = 1,227 complete-case modeling sample), and estimator fixed. Variant A is a two-item PCA score from go_for_walk and exercise_or_yoga, dropping the weakly loading reading item. Variant B is a formative index, the equal-weight mean of z-scored responses to all three items, imposing no common-factor assumption. Variant C is a full re-estimation of the activities factor analysis with the reading item removed from the item set, re-deriving all three factor scores; all variant scores were standardized before entering the regressions.

| Variant | Term | M1 | M2 | M3 | M4 | Weak = Strong Wald (m2) p |
|---|---|---|---|---|---|---:|
| Canonical | weak_variety | 0.233* | 0.161* | 0.198* | 0.139* | < .001 |
| Canonical | strong_variety | −0.170* | −0.175* | −0.132* | −0.153* | < .001 |
| A: 2-item PCA | weak_variety | 0.179* | 0.096 | 0.150* | 0.077 | .096 |
| A: 2-item PCA | strong_variety | −0.031 | −0.059 | −0.002 | −0.043 | .096 |
| B: formative mean-z | weak_variety | 0.216* | 0.125* | 0.183* | 0.106* | .048 |
| B: formative mean-z | strong_variety | −0.035 | −0.061 | −0.002 | −0.044 | .048 |
| C: 19-item PCA reassignment | weak_variety | 0.218* | 0.158* | 0.187* | 0.135* | < .001 |
| C: 19-item PCA reassignment | strong_variety | −0.172* | −0.192* | −0.137* | −0.168* | < .001 |

Three conclusions follow. First, the weak-tie variety effect on solitary leisure is stable across every operationalization, positive and significant in all four model specifications under the canonical solution, the formative index, and the reassigned factor model, and in two of four specifications under the bare two-item scale. Second, the strong-tie variety effect is the fragile result: it holds under the canonical and reassigned factor models but is not detectable under the two-item scale or the equal-weight index. The reassigned factor model differs from the two-item scale in an important respect: once reading leaves the item set, the leisure component becomes a broader active everyday-leisure dimension anchored by walking (.78) and exercising (.63) with secondary loadings from fast_food (.38), hiking (.30), and fancy restaurants (.35), which is closer in spirit to the canonical construct than the bare two-item scale is. The formative index (variant B), which imposes no common-factor assumption at all, still preserves the weak-tie effect and keeps the weak-vs-strong contrast significant at p = .048, which is useful middle ground to report. Third, the reassigned arts factor correlates at .996 with the canonical arts factor, so Table 1 is untouched by any of this.

## 6. Reliability Caveat for Variant C

A reliability caveat attaches to variant C specifically. With reading dropped, the leisure factor retains only two indicators, and a two-indicator factor cannot separate reliability from the inter-item correlation. The WLSMV CFA on the reassigned structure in fact warns of negative estimated residual variances (a Heywood case), so the nominally high omega for the two-item leisure factor under variant C (.78) is an artifact of that local underidentification and should not be reported as evidence of reliability. The honest figures for a two-item scale are alpha-style bounds of roughly .40 (Pearson) to .50 (polychoric-implied). Dropping reading makes the factor structure cleaner but does nothing for the reliability problem.

## 7. No Additional Items Are Available

The full survey extract (`grabbagdata111925_full.dta`, 179 columns) contains several additional cultural batteries — twenty photograph liking judgments, four "Is It Art?" items, ten fashion preferences, ten food-taste items, sixteen cuisine-familiarity items, and several evaluative dispositions — but none of them are practice-frequency items. They are all evaluative-taste judgments, a different construct from the activities battery. Correlating every candidate item against the canonical solitary-leisure factor score, the strongest association in the entire set is r = .18 (avocado), with the median near .05. Folding any of them in would depress reliability further while changing what the factor means. The survey simply contains no additional participation items about home-based or solitary activities that could anchor the weak factor; the fix, if a future data collection is on the table, is adding more home-based leisure activity items at the design stage.

## 8. Conclusions for the Reviewer Response

The recommended framing keeps the canonical three-item solitary-leisure specification as the paper's main model and reports this work as a reliability and sensitivity supplement. The response can state that the activities model meets conventional CFA thresholds with RMSEA at the high edge; that all substantive factors show strong reliability (arts participation omega .957) with the solitary-leisure factor the acknowledged weak point; that its measurement noise biases regression coefficients toward zero, making the surviving significant effects conservative; that the weak-tie variety finding is robust across every operationalization tested while the strong-tie effect on solitary leisure should be described with its weighting sensitivity acknowledged; and that the survey contains no additional items that could strengthen the weak factor, making this an instrument-design limitation rather than an analysis choice.
