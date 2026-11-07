# Manuscript statistic audit -- 2026-11-06 13:45 PST

30 checks: **10 OK**, **20 MISMATCH**, **0 NOT_FOUND**.

| id | description | reported | actual | status | note |
|---|---|---|---|---|---|
| phi_variety | Phi(variety, liberal/conservative composition), weak & strong ties -- varimax orthogonality check | 0 | 0.4395 | MISMATCH |  |
| phi_lib_cons | Phi(liberal, conservative composition), weak & strong ties -- varimax orthogonality check | 0 | 0.394 | MISMATCH |  |
| tableA2_strong_variety | Table A2: strong-tie variety vs. raw count (r) | 0.61 | 0.7886 | MISMATCH |  |
| tableA2_strong_liberal | Table A2: strong-tie liberal composition vs. raw count (r) | 0.5 | 0.5378 | MISMATCH |  |
| tableA2_strong_conservative | Table A2: strong-tie conservative composition vs. raw count (r) | 0.5 | 0.7316 | MISMATCH |  |
| tableA2_weak_liberal | Table A2: weak-tie liberal composition vs. raw count (r) | 0.59 | 0.7554 | MISMATCH |  |
| tableA2_weak_variety | Table A2: weak-tie variety vs. raw count (r) | 0.56 | 0.6974 | MISMATCH |  |
| tableA2_weak_conservative | Table A2: weak-tie conservative composition vs. raw count (r) | 0.46 | 0.6155 | MISMATCH |  |
| arts_wald_stat | Arts Wald test statistic (F/Chisq, df=1) | 19.2 | 19.41 | MISMATCH |  |
| residual_wald_m1m3_chisq | Residual leisure: composition block Chisq (vs. variety-only model) | 5.99 | 4.052 | MISMATCH |  |
| residual_wald_m1m3_p | Residual leisure: composition block p (vs. variety-only model) | 0.2 | 0.3991 | MISMATCH |  |
| residual_wald_m2m4_chisq | Residual leisure: composition block Chisq (vs. full covariate model) | 5.61 | 7.072 | MISMATCH |  |
| residual_wald_m2m4_p | Residual leisure: composition block p (vs. full covariate model) | 0.231 | 0.1321 | MISMATCH |  |
| leisure_weak_min | Table 2: weak-tie variety coefficient, min across Models 1-4 | 0.14 | 0.1257 | MISMATCH |  |
| leisure_weak_max | Table 2: weak-tie variety coefficient, max across Models 1-4 | 0.23 | 0.213 | MISMATCH |  |
| leisure_strong_min | Table 2: |strong-tie variety coefficient|, min across Models 1-4 | 0.13 | 0.08079 | MISMATCH |  |
| leisure_strong_max | Table 2: |strong-tie variety coefficient|, max across Models 1-4 | 0.18 | 0.1699 | MISMATCH |  |
| leisure_child_arts_m4_p | Table 2: childhood arts p-value, Model 4 | 0.04 | 0.02376 | MISMATCH |  |
| arts_race_mixed_p | Table 1: multiracial vs. White p-value, Model 2 | < 0.001 | 0.005272 | MISMATCH | reported as an upper-bound threshold (e.g. p < .001) |
| arts_race_latine_p | Table 1: Hispanic/Latine vs. White p-value, Model 2 | 0.05 | 0.07177 | MISMATCH |  |
| act_kmo | Activities factor model KMO | 0.958 | 0.9584 | OK |  |
| act_var_pct | Activities factor model cumulative variance explained (%) | 58.7 | 58.66 | OK |  |
| tie_kmo_weak | Weak-tie factor model KMO | 0.938 | 0.938 | OK |  |
| tie_kmo_strong | Strong-tie factor model KMO | 0.935 | 0.9355 | OK |  |
| tie_var_pct_weak | Weak-tie factor model variance explained (%) | 61 | 61.22 | OK |  |
| tie_var_pct_strong | Strong-tie factor model variance explained (%) | 61 | 61.22 | OK |  |
| arts_wald_df2 | Arts Wald test residual df | 1211 | 1211 | OK |  |
| arts_wald_p | Arts Wald test p-value | < 0.001 | 1.052e-05 | OK | reported as an upper-bound threshold (e.g. p < .001) |
| leisure_child_arts_m4_b | Table 2: childhood arts coefficient, Model 4 | 0.04 | 0.04153 | OK |  |
| arts_race_aapi_p | Table 1: AAPI vs. White p-value, Model 2 | < 0.001 | 4.819e-06 | OK | reported as an upper-bound threshold (e.g. p < .001) |
