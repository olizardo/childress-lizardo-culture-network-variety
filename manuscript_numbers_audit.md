# Manuscript statistic audit -- 2026-11-07 10:03 PST

38 checks: **12 OK**, **18 MISMATCH**, **8 NOT_FOUND**.

| id | description | reported | actual | status | note |
|---|---|---|---|---|---|
| act_kmo | Activities factor model KMO | 0.958 | 0.9552 | MISMATCH |  |
| act_var_pct | Activities factor model cumulative variance explained (%) | 58.7 | 63.21 | MISMATCH |  |
| tie_var_pct_weak | Weak-tie factor model variance explained (%) | 61 | 68.04 | MISMATCH |  |
| tie_var_pct_strong | Strong-tie factor model variance explained (%) | 61 | 68.02 | MISMATCH |  |
| tableA2_strong_variety | Table A2: strong-tie variety vs. raw count (r) | 0.61 | 0.6413 | MISMATCH |  |
| tableA2_strong_liberal | Table A2: strong-tie liberal composition vs. raw count (r) | 0.5 | 0.4688 | MISMATCH |  |
| tableA2_weak_liberal | Table A2: weak-tie liberal composition vs. raw count (r) | 0.59 | 0.5118 | MISMATCH |  |
| tableA2_weak_variety | Table A2: weak-tie variety vs. raw count (r) | 0.56 | 0.6472 | MISMATCH |  |
| tableA2_weak_conservative | Table A2: weak-tie conservative composition vs. raw count (r) | 0.46 | 0.408 | MISMATCH |  |
| arts_wald_stat | Arts Wald test statistic (F/Chisq, df=1) | 19.2 | 16.52 | MISMATCH |  |
| residual_wald_m1m3_chisq | Residual leisure: composition block Chisq (vs. variety-only model) | 5.99 | 7.821 | MISMATCH |  |
| residual_wald_m1m3_p | Residual leisure: composition block p (vs. variety-only model) | 0.2 | 0.09834 | MISMATCH |  |
| residual_wald_m2m4_chisq | Residual leisure: composition block Chisq (vs. full covariate model) | 5.61 | 5.105 | MISMATCH |  |
| residual_wald_m2m4_p | Residual leisure: composition block p (vs. full covariate model) | 0.231 | 0.2767 | MISMATCH |  |
| leisure_strong_max | Table 2: |strong-tie variety coefficient|, max across Models 1-4 | 0.18 | 0.195 | MISMATCH |  |
| leisure_child_arts_m4_b | Table 2: childhood arts coefficient, Model 4 | 0.04 | 0.0338 | MISMATCH |  |
| leisure_child_arts_m4_p | Table 2: childhood arts p-value, Model 4 | 0.04 | 0.06411 | MISMATCH |  |
| arts_race_latine_p | Table 1: Hispanic/Latine vs. White p-value, Model 2 | 0.05 | 0.04125 | MISMATCH |  |
| rel_omega_range | Reliability: substantive omega range | NA | NA | NOT_FOUND | pattern not matched in current manuscript text -- check wording, or the claim may have been removed/reworded |
| rel_leisure_corr | Reliability: solitary-leisure inter-item correlation range | NA | NA | NOT_FOUND | pattern not matched in current manuscript text -- check wording, or the claim may have been removed/reworded |
| cfa_load_act | CFA: activities standardized loading range | NA | NA | NOT_FOUND | pattern not matched in current manuscript text -- check wording, or the claim may have been removed/reworded |
| cfa_load_ties | CFA: tie-model standardized loading range | NA | NA | NOT_FOUND | pattern not matched in current manuscript text -- check wording, or the claim may have been removed/reworded |
| rob_arts_r | Robustness: variant-C vs. canonical arts factor correlation | NA | NA | NOT_FOUND | pattern not matched in current manuscript text -- check wording, or the claim may have been removed/reworded |
| rob_wald_B | Robustness: formative-index weak-vs-strong Wald p | NA | NA | NOT_FOUND | pattern not matched in current manuscript text -- check wording, or the claim may have been removed/reworded |
| rob_nogender_wald | No-gender robustness: weak-vs-strong variety Wald p-values | NA | NA | NOT_FOUND | pattern not matched in current manuscript text -- check wording, or the claim may have been removed/reworded |
| rob_nogender_comp | No-gender robustness: composition-block Wald chi-squares | NA | NA | NOT_FOUND | pattern not matched in current manuscript text -- check wording, or the claim may have been removed/reworded |
| tie_kmo_weak | Weak-tie factor model KMO | 0.938 | 0.9385 | OK |  |
| tie_kmo_strong | Strong-tie factor model KMO | 0.935 | 0.9347 | OK |  |
| phi_variety | Phi(variety, liberal/conservative composition), weak & strong ties -- varimax orthogonality check | 0 | 0 | OK |  |
| phi_lib_cons | Phi(liberal, conservative composition), weak & strong ties -- varimax orthogonality check | 0 | 0 | OK |  |
| tableA2_strong_conservative | Table A2: strong-tie conservative composition vs. raw count (r) | 0.5 | 0.4928 | OK |  |
| arts_wald_df2 | Arts Wald test residual df | 1211 | 1211 | OK |  |
| arts_wald_p | Arts Wald test p-value | < 0.001 | 4.824e-05 | OK | reported as an upper-bound threshold (e.g. p < .001) |
| leisure_weak_min | Table 2: weak-tie variety coefficient, min across Models 1-4 | 0.14 | 0.1403 | OK |  |
| leisure_weak_max | Table 2: weak-tie variety coefficient, max across Models 1-4 | 0.23 | 0.232 | OK |  |
| leisure_strong_min | Table 2: |strong-tie variety coefficient|, min across Models 1-4 | 0.13 | 0.1339 | OK |  |
| arts_race_aapi_p | Table 1: AAPI vs. White p-value, Model 2 | < 0.001 | 7.426e-08 | OK | reported as an upper-bound threshold (e.g. p < .001) |
| arts_race_mixed_p | Table 1: multiracial vs. White p-value, Model 2 | < 0.001 | 1.132e-05 | OK | reported as an upper-bound threshold (e.g. p < .001) |
