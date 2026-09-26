#!/usr/bin/env Rscript
# Scripts/02_fit_models.R
#
# Fits the four-model hierarchical OLS sequence (variety only -> + demographics
# -> + ideological composition -> + both) for each of the two cultural
# participation outcomes (Table 1: Public Arts Participation; Table 2:
# Solitary Leisure), with HC1 (Stata vce(robust)-equivalent) standard errors.
# Also runs the Wald test comparing the weak- vs strong-tie variety
# coefficients reported in-text for the arts participation model.
#
# Also fits the same four-model sequence on the residual/DIY-practical
# leisure factor (Table 3) as a discriminant-validity check: if ideological
# composition is a specific predictor of arts/leisure participation rather
# than a generic correlate of any activity battery, the composition terms
# should be jointly null here even though they are jointly significant for
# Tables 1-2. Nested Wald tests (composition terms = 0, HC1-robust) are run
# for all three outcomes at both the m1->m3 and m2->m4 steps.
#
# Outputs cached to cache/02_models.rds for downstream table/figure scripts.

suppressPackageStartupMessages({
  library(dplyr)
  library(estimatr)
  library(car)
})

dir.create("cache", showWarnings = FALSE, recursive = TRUE)

message("[1/3] Loading prepared data...")
prep <- readRDS("cache/01_prepared_data.rds")
df <- prep$df

covar_terms <- "gender_f + educ_cat + child_arts + income + age2 + race_f + poli"
variety_terms <- "weak_variety + strong_variety"
composition_terms <- "weak_lib_comp + weak_cons_comp + strong_lib_comp + strong_cons_comp"

model_vars <- c(
  "arts_participation", "solitary_leisure", "residual_leisure",
  "weak_variety", "strong_variety", "weak_lib_comp", "weak_cons_comp",
  "strong_lib_comp", "strong_cons_comp",
  "gender_f", "educ_cat", "educ", "child_arts", "income", "age2", "race_f", "poli"
)
mdat <- df[complete.cases(df[model_vars]), model_vars]
message(sprintf("  Complete-case modeling sample: N = %d", nrow(mdat)))
stopifnot(!anyNA(mdat$residual_leisure))

message("[2/3] Fitting hierarchical model sequence for each outcome...")

fit_four_models <- function(outcome, data) {
  f1 <- as.formula(paste(outcome, "~", variety_terms))
  f2 <- as.formula(paste(outcome, "~", variety_terms, "+", covar_terms))
  f3 <- as.formula(paste(outcome, "~", variety_terms, "+", composition_terms))
  f4 <- as.formula(paste(outcome, "~", variety_terms, "+", composition_terms, "+", covar_terms))
  list(
    m1 = lm_robust(f1, data = data, se_type = "HC1"),
    m2 = lm_robust(f2, data = data, se_type = "HC1"),
    m3 = lm_robust(f3, data = data, se_type = "HC1"),
    m4 = lm_robust(f4, data = data, se_type = "HC1")
  )
}

arts_models    <- fit_four_models("arts_participation", mdat)
leisure_models <- fit_four_models("solitary_leisure", mdat)
residual_models <- fit_four_models("residual_leisure", mdat)

message("[3/4] Running Wald tests for weak- vs. strong-tie variety coefficients...")

wald_test <- function(model) {
  linearHypothesis(model, "weak_variety = strong_variety", white.adjust = "hc1")
}
arts_wald    <- wald_test(arts_models$m2)
leisure_wald <- wald_test(leisure_models$m2)

message("[4/4] Running nested Wald tests for the composition-terms block (m1->m3, m2->m4)...")

comp_hyp <- c(
  "weak_lib_comp = 0", "weak_cons_comp = 0",
  "strong_lib_comp = 0", "strong_cons_comp = 0"
)
nested_composition_test <- function(model_full) {
  linearHypothesis(model_full, comp_hyp, white.adjust = "hc1")
}

arts_nested_m1_m3    <- nested_composition_test(arts_models$m3)
arts_nested_m2_m4    <- nested_composition_test(arts_models$m4)
leisure_nested_m1_m3 <- nested_composition_test(leisure_models$m3)
leisure_nested_m2_m4 <- nested_composition_test(leisure_models$m4)
residual_nested_m1_m3 <- nested_composition_test(residual_models$m3)
residual_nested_m2_m4 <- nested_composition_test(residual_models$m4)

out <- list(
  mdat = mdat,
  arts_models = arts_models,
  leisure_models = leisure_models,
  residual_models = residual_models,
  arts_wald = arts_wald,
  leisure_wald = leisure_wald,
  comp_hyp = comp_hyp,
  arts_nested_m1_m3 = arts_nested_m1_m3,
  arts_nested_m2_m4 = arts_nested_m2_m4,
  leisure_nested_m1_m3 = leisure_nested_m1_m3,
  leisure_nested_m2_m4 = leisure_nested_m2_m4,
  residual_nested_m1_m3 = residual_nested_m1_m3,
  residual_nested_m2_m4 = residual_nested_m2_m4,
  covar_terms = covar_terms,
  variety_terms = variety_terms,
  composition_terms = composition_terms
)
saveRDS(out, "cache/02_models.rds")
message("Done. Model objects saved to cache/02_models.rds (Table 1: arts_models, Table 2: leisure_models, Table 3: residual_models).")
