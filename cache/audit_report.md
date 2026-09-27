# Manuscript statistic audit -- 2026-09-27 08:31 PDT

34 checks: **11 OK**, **23 MISMATCH**, **0 NOT_FOUND**.

| id | description | reported | actual | status | note |
|---|---|---|---|---|---|
| phi_variety_liberal_weak | Phi(variety, liberal), weak ties | 0.35 | 0 | MISMATCH |  |
| phi_variety_liberal_strong | Phi(variety, liberal), strong ties | 0.26 | 0 | MISMATCH |  |
| phi_variety_conservative_weak | Phi(variety, conservative), weak ties | 0.25 | 0 | MISMATCH |  |
| phi_variety_conservative_strong | Phi(variety, conservative), strong ties | 0.44 | 0 | MISMATCH |  |
| phi_lib_cons_weak | Phi(liberal, conservative), weak ties | 0.39 | 0 | MISMATCH |  |
| phi_lib_cons_strong | Phi(liberal, conservative), strong ties | 0.28 | 0 | MISMATCH |  |
| tableA5_strong_variety | Table A5: strong-tie variety vs. raw count (r) | 0.79 | 0.6122 | MISMATCH |  |
| tableA5_strong_liberal | Table A5: strong-tie liberal composition vs. raw count (r) | 0.54 | 0.5004 | MISMATCH |  |
| tableA5_strong_conservative | Table A5: strong-tie conservative composition vs. raw count (r) | 0.73 | 0.5032 | MISMATCH |  |
| tableA5_weak_liberal | Table A5: weak-tie liberal composition vs. raw count (r) | 0.76 | 0.585 | MISMATCH |  |
| tableA5_weak_variety | Table A5: weak-tie variety vs. raw count (r) | 0.7 | 0.5621 | MISMATCH |  |
| tableA5_weak_conservative | Table A5: weak-tie conservative composition vs. raw count (r) | 0.62 | 0.4587 | MISMATCH |  |
| arts_wald_stat | Arts Wald test statistic (F/Chisq, df=1) | 19.41 | 19.2 | MISMATCH |  |
| arts_wald_df2 | Arts Wald test residual df | 1213 | 1211 | MISMATCH |  |
| residual_wald_m1m3_chisq | Residual leisure: composition block Chisq (vs. variety-only model) | 4.05 | 5.991 | MISMATCH |  |
| residual_wald_m1m3_p | Residual leisure: composition block p (vs. variety-only model) | 0.399 | 0.1998 | MISMATCH |  |
| residual_wald_m2m4_chisq | Residual leisure: composition block Chisq (vs. full covariate model) | 7.07 | 5.607 | MISMATCH |  |
| residual_wald_m2m4_p | Residual leisure: composition block p (vs. full covariate model) | 0.132 | 0.2305 | MISMATCH |  |
| leisure_weak_min | Table 2: weak-tie variety coefficient, min across Models 1-4 | 0.13 | 0.1392 | MISMATCH |  |
| leisure_weak_max | Table 2: weak-tie variety coefficient, max across Models 1-4 | 0.21 | 0.2328 | MISMATCH |  |
| leisure_strong_min | Table 2: |strong-tie variety coefficient|, min across Models 1-4 | 0.08 | 0.1319 | MISMATCH |  |
| leisure_child_arts_m4_p | Table 2: childhood arts p-value, Model 4 | 0.02 | 0.03793 | MISMATCH |  |
| arts_race_latine_p | Table 1: Hispanic/Latine vs. White p-value, Model 2 | 0.07 | 0.04954 | MISMATCH |  |
| act_kmo | Activities factor model KMO | 0.958 | 0.9584 | OK |  |
| act_var_pct | Activities factor model cumulative variance explained (%) | 58.7 | 58.66 | OK |  |
| tie_kmo_weak | Weak-tie factor model KMO | 0.938 | 0.938 | OK |  |
| tie_kmo_strong | Strong-tie factor model KMO | 0.935 | 0.9355 | OK |  |
| tie_var_pct_weak | Weak-tie factor model variance explained (%) | 61 | 61.22 | OK |  |
| tie_var_pct_strong | Strong-tie factor model variance explained (%) | 61 | 61.22 | OK |  |
| arts_wald_p | Arts Wald test p-value | < 0.001 | 1.176e-05 | OK | reported as an upper-bound threshold (e.g. p < .001) |
| leisure_strong_max | Table 2: |strong-tie variety coefficient|, max across Models 1-4 | 0.17 | 0.1754 | OK |  |
| leisure_child_arts_m4_b | Table 2: childhood arts coefficient, Model 4 | 0.04 | 0.03813 | OK |  |
| arts_race_aapi_p | Table 1: AAPI vs. White p-value, Model 2 | < 0.001 | 4.426e-07 | OK | reported as an upper-bound threshold (e.g. p < .001) |
| arts_race_mixed_p | Table 1: multiracial vs. White p-value, Model 2 | 0.005 | 3.433e-05 | OK |  |
