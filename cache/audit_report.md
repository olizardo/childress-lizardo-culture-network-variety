# Manuscript statistic audit -- 2026-09-27 09:52 PDT

30 checks: **30 OK**, **0 MISMATCH**, **0 NOT_FOUND**.

| id | description | reported | actual | status | note |
|---|---|---|---|---|---|
| act_kmo | Activities factor model KMO | 0.958 | 0.9584 | OK |  |
| act_var_pct | Activities factor model cumulative variance explained (%) | 58.7 | 58.66 | OK |  |
| tie_kmo_weak | Weak-tie factor model KMO | 0.938 | 0.938 | OK |  |
| tie_kmo_strong | Strong-tie factor model KMO | 0.935 | 0.9355 | OK |  |
| tie_var_pct_weak | Weak-tie factor model variance explained (%) | 61 | 61.22 | OK |  |
| tie_var_pct_strong | Strong-tie factor model variance explained (%) | 61 | 61.22 | OK |  |
| phi_variety | Phi(variety, liberal/conservative composition), weak & strong ties -- varimax orthogonality check | 0 | 0 | OK |  |
| phi_lib_cons | Phi(liberal, conservative composition), weak & strong ties -- varimax orthogonality check | 0 | 0 | OK |  |
| tableA2_strong_variety | Table A2: strong-tie variety vs. raw count (r) | 0.61 | 0.6122 | OK |  |
| tableA2_strong_liberal | Table A2: strong-tie liberal composition vs. raw count (r) | 0.5 | 0.5004 | OK |  |
| tableA2_strong_conservative | Table A2: strong-tie conservative composition vs. raw count (r) | 0.5 | 0.5032 | OK |  |
| tableA2_weak_liberal | Table A2: weak-tie liberal composition vs. raw count (r) | 0.59 | 0.585 | OK |  |
| tableA2_weak_variety | Table A2: weak-tie variety vs. raw count (r) | 0.56 | 0.5621 | OK |  |
| tableA2_weak_conservative | Table A2: weak-tie conservative composition vs. raw count (r) | 0.46 | 0.4587 | OK |  |
| arts_wald_stat | Arts Wald test statistic (F/Chisq, df=1) | 19.2 | 19.2 | OK |  |
| arts_wald_df2 | Arts Wald test residual df | 1211 | 1211 | OK |  |
| arts_wald_p | Arts Wald test p-value | < 0.001 | 1.176e-05 | OK | reported as an upper-bound threshold (e.g. p < .001) |
| residual_wald_m1m3_chisq | Residual leisure: composition block Chisq (vs. variety-only model) | 5.99 | 5.991 | OK |  |
| residual_wald_m1m3_p | Residual leisure: composition block p (vs. variety-only model) | 0.2 | 0.1998 | OK |  |
| residual_wald_m2m4_chisq | Residual leisure: composition block Chisq (vs. full covariate model) | 5.61 | 5.607 | OK |  |
| residual_wald_m2m4_p | Residual leisure: composition block p (vs. full covariate model) | 0.231 | 0.2305 | OK |  |
| leisure_weak_min | Table 2: weak-tie variety coefficient, min across Models 1-4 | 0.14 | 0.1392 | OK |  |
| leisure_weak_max | Table 2: weak-tie variety coefficient, max across Models 1-4 | 0.23 | 0.2328 | OK |  |
| leisure_strong_min | Table 2: |strong-tie variety coefficient|, min across Models 1-4 | 0.13 | 0.1319 | OK |  |
| leisure_strong_max | Table 2: |strong-tie variety coefficient|, max across Models 1-4 | 0.18 | 0.1754 | OK |  |
| leisure_child_arts_m4_b | Table 2: childhood arts coefficient, Model 4 | 0.04 | 0.03813 | OK |  |
| leisure_child_arts_m4_p | Table 2: childhood arts p-value, Model 4 | 0.04 | 0.03793 | OK |  |
| arts_race_aapi_p | Table 1: AAPI vs. White p-value, Model 2 | < 0.001 | 4.426e-07 | OK | reported as an upper-bound threshold (e.g. p < .001) |
| arts_race_mixed_p | Table 1: multiracial vs. White p-value, Model 2 | < 0.001 | 3.433e-05 | OK | reported as an upper-bound threshold (e.g. p < .001) |
| arts_race_latine_p | Table 1: Hispanic/Latine vs. White p-value, Model 2 | 0.05 | 0.04954 | OK |  |
