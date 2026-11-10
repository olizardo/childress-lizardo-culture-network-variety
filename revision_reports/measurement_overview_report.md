# Measurement Models Overview: Reliability, Ordinal CFA, and Sensitivity

**Project:** Beyond Network Variety in Arts Attendance (SSR-D-26-01726)
**Date:** November 6, 2026
**Scripts:** `Scripts/07_measurement_reliability_cfa.R`, `Scripts/08_solitary_robustness.R`, `Scripts/09_no_gender_robustness.R`

## 1. Purpose

This is the cross-battery overview for the measurement-model requests in the SSR-D-26-01726 reviews: Reviewer 1 asked directly for an ordinal confirmatory factor analysis, Reviewer 4 named the polychoric/WLSMV approach as a defensible option, and the methodological brainstorm called for McDonald's omega for each factor with Cronbach's alpha as a secondary statistic, on the grounds that alpha's tau-equivalence assumption is doubtful for these heterogeneous item sets. Two companion reports carry the battery-specific detail: `activities_cfa_sensitivity_report.md` (activities battery, including the solitary-leisure operationalization sensitivity analysis) and `tie_cfa_sensitivity_report.md` (weak- and strong-tie CFA diagnostics, relaxed refits, four-factor test, and gender-item robustness). This document records the headline numbers and where each claim is documented in full.

## 2. Data and Measurement Models

All analyses use the canonical prepared data (`cache/01_prepared_data.rds`, N = 1,258; complete-case modeling sample N = 1,227). Both item batteries are ordinal: the twenty activity items on a 1–7 frequency scale, the eighteen group-position-generator items per tie type on a 0–4 count scale. The three measurement models match the paper's factor structure: Arts Participation, Solitary Leisure, and the residual DIY-practical dimension for the activities battery; Variety, Liberal Composition, and Conservative Composition for each tie battery. Items were assigned to their highest-loading factor in the varimax PCA solution, so the confirmatory models follow the same data-driven structure the paper reports. Two assignments deserve note: `read_novel_poem_or_play` loads with solitary leisure rather than arts participation, and the factor-to-item allocation differs between the tie batteries (variety: 5 items weak, 8 strong; liberal: 8 weak, 4 strong; conservative: 5 weak, 6 strong).

## 3. Reliability Overview

McDonald's omega was computed from each fitted ordinal CFA using `semTools::compRelSEM`; Cronbach's alpha was computed as a secondary statistic (polychoric-based for the tie batteries, Pearson for the 1–7 activity items, with EFA loading signs as reverse-keying).

| Model | Factor | n items | McDonald's omega | Cronbach's alpha |
|---|---|---:|---:|---:|
| Activities | Arts participation | 14 | 0.957 | 0.940 |
| Activities | Solitary leisure | 3 | 0.389 | 0.428 |
| Activities | Residual (DIY-practical) | 3 | 0.545 | 0.382 |
| Weak ties | Variety | 5 | 0.881 | 0.890 |
| Weak ties | Liberal composition | 8 | 0.881 | 0.897 |
| Weak ties | Conservative composition | 5 | 0.809 | 0.869 |
| Strong ties | Variety | 8 | 0.917 | 0.923 |
| Strong ties | Liberal composition | 4 | 0.794 | 0.798 |
| Strong ties | Conservative composition | 6 | 0.859 | 0.890 |

All seven substantive factors show omega between .79 and .96. The two three-item factors are the exceptions, and they mean different things. The residual factor's low omega is expected and unproblematic: it is a statistical contrast dimension used only as a discriminant-validity check, and its structure is exactly what a contrast should look like (gardening and home repair at r = .47; fast_food at the negative pole with a .20 loading). The solitary-leisure factor is the genuine weak spot: its items intercorrelate at only .18 to .34, and because score noise attenuates regression coefficients toward zero, its low reliability makes the surviving significant effects conservative while weakening any null findings against that outcome. Sections 4–5 of `activities_cfa_sensitivity_report.md` treat this in full.

## 4. Ordinal CFA Fit Overview

Each three-factor structure was fit in `lavaan` with WLSMV estimation on the polychoric matrix. Standardized loadings range from .20 to .90 for the activities model (the .20 is fast_food on the residual contrast) and from .57 to .90 for the tie models.

| Model | chi-square (scaled) | df | CFI | TLI | RMSEA [90% CI] | SRMR |
|---|---:|---:|---:|---:|---|---:|
| Activities | 2051.2 | 167 | 0.956 | 0.950 | 0.095 [0.092, 0.099] | 0.061 |
| Weak ties | 2487.4 | 132 | 0.891 | 0.874 | 0.119 [0.115, 0.123] | 0.082 |
| Strong ties | 1944.1 | 132 | 0.918 | 0.905 | 0.105 [0.100, 0.109] | 0.073 |

The activities model meets conventional thresholds on CFI and SRMR, with RMSEA at the high edge of the acceptable range. The tie models show acceptable SRMR values but sub-.95 CFI and RMSEA above .10. That misfit is not diffuse: the tie report locates it in three interpretable sources (antonym-pair bipolarity, a minority-versus-majority clustering within variety, and the theoretically ambiguous gender items), shows that relaxing the offending parameters moves the weak-tie model to CFI .934 / RMSEA .065 and the strong-tie model to CFI .940 / RMSEA .091, demonstrates that all substantive omegas are invariant to the relaxation, tests and rejects a four-factor restructuring (the fourth factor is a two-item City–Rural bipolar contrast, not a construct), and shows that dropping all four gender items replicates every regression verdict in Tables 1 and 2.

### Two-factor alternative for activities

Dropping the residual items and fitting a two-factor model (arts and leisure, 17 items, N = 1,243, WLSMV) gives CFI .962 and RMSEA .102, against CFI .962 and RMSEA .102 for a one-factor model on the same 17 items. The scaled chi-square difference is 23.8 on 1 df (p ≈ 1e-6), but the descriptive fit does not change, and with this sample size a significant difference test for one extra factor is weak evidence of a distinct dimension. The arts and leisure factors correlate .77, so they share most of their variance. The leisure factor rests on three items with uneven loadings: exercise or yoga (.59), reading (.50), and walking (.19). Walking contributes little to the factor, and a factor defined by one weak indicator is poorly identified. The three-factor model fits a 20-item set and is not directly comparable, though its RMSEA (.095) is lower. The data support treating everyday activity as a broadly shared dimension with a weak secondary leisure component. The orthogonal varimax solution in the manuscript is not consistent with this .77 correlation, and the leisure factor should be described as weak in any reliability or validity discussion. Script: analysis run in session scratch space, not yet saved to `Scripts/`.

## 5. Sensitivity Results in Brief

The solitary-leisure operationalization analysis (full table in Section 5 of `activities_cfa_sensitivity_report.md`) re-estimates Table 2 under a two-item PCA scale, a formative equal-weight index, and a reassigned 19-item factor model. The weak-tie variety effect on solitary leisure is positive and significant under every operationalization except the weakest specification of the weakest variant, and the reassigned arts factor correlates at .996 with the canonical arts factor, leaving Table 1 untouched. The strong-tie variety effect on solitary leisure holds under the canonical and reassigned factor models but is not detectable under the two-item scale or the equal-weight index, so it should be described with that qualification. The formative index, which imposes no common-factor assumption at all, still preserves the weak-tie effect and keeps the weak-vs-strong contrast significant at p = .048.

## 6. Recommendations for the Response

Keep the canonical specifications as the paper's main measurement models and report this work as a measurement and sensitivity supplement. On reliability, state that all substantive factors show McDonald's omega between .79 and .96, that the residual factor's omega is not a meaningful evaluative standard for a contrast dimension, and that the solitary-leisure factor's low reliability biases coefficients toward zero in the conservative direction. On fit, report the WLSMV statistics in full and present the tie-model diagnostics as located, interpretable, and demonstrably inconsequential for the paper's estimates. On solitary leisure, acknowledge the weighting sensitivity of the strong-tie effect rather than presenting it as a firm finding, and note that the survey contains no additional items that could strengthen the weak factor, making this an instrument-design limitation rather than an analysis choice.
