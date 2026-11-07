#!/usr/bin/env Rscript
# Verification: refit Table 1/2 sequences with PCA scores vs CFA (WLSMV) scores
# on the same respondents. Run from the project root.

suppressPackageStartupMessages({
  library(lavaan)
  library(estimatr)
  library(car)
})

prep <- readRDS("cache/01_prepared_data.rds")
str07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df

covar_terms <- "gender_f + educ_cat + child_arts + income + age2 + race_f + poli"
variety_terms <- "weak_variety + strong_variety"
composition_terms <- "weak_lib_comp + weak_cons_comp + strong_lib_comp + strong_cons_comp"

# Fit one ordinal CFA on the item structure from script 07 and return
# lavaan factor scores for the rows that are complete on those items.
cfa_scores <- function(items_by_factor, data) {
  model_string <- paste(
    sapply(names(items_by_factor), function(fid)
      sprintf("%s =~ %s", fid, paste(names(items_by_factor[[fid]]), collapse = " + "))),
    collapse = "\n")
  all_items <- unlist(lapply(items_by_factor, names))
  sub <- data[complete.cases(data[all_items]), all_items, drop = FALSE]
  sub[] <- lapply(sub, function(x) as.numeric(unclass(x)))
  fit <- cfa(model_string, data = sub, ordered = all_items,
             estimator = "WLSMV", std.lv = TRUE)
  sc <- as.data.frame(lavPredict(fit))
  list(scores = sc, rows = as.integer(rownames(sub)), fit = fit)
}

act_cfa  <- cfa_scores(str07$activities, df)
weak_cfa <- cfa_scores(str07$weak, df)
strong_cfa <- cfa_scores(str07$strong, df)

# Common analytic sample: complete on every variable in either specification
model_vars_pca <- c(
  "arts_participation", "solitary_leisure", "residual_leisure",
  "weak_variety", "strong_variety", "weak_lib_comp", "weak_cons_comp",
  "strong_lib_comp", "strong_cons_comp",
  "gender_f", "educ_cat", "child_arts", "income", "age2", "race_f", "poli"
)
common_rows <- Reduce(intersect, list(act_cfa$rows, weak_cfa$rows, strong_cfa$rows))

cfa_df <- df[common_rows, ]
cfa_df$arts_participation <- act_cfa$scores$arts[match(common_rows, act_cfa$rows)]
cfa_df$solitary_leisure   <- act_cfa$scores$leisure[match(common_rows, act_cfa$rows)]
cfa_df$residual_leisure   <- act_cfa$scores$residual[match(common_rows, act_cfa$rows)]
cfa_df$weak_variety       <- weak_cfa$scores$variety[match(common_rows, weak_cfa$rows)]
cfa_df$weak_lib_comp      <- weak_cfa$scores$liberal[match(common_rows, weak_cfa$rows)]
cfa_df$weak_cons_comp     <- weak_cfa$scores$conservative[match(common_rows, weak_cfa$rows)]
cfa_df$strong_variety     <- strong_cfa$scores$variety[match(common_rows, strong_cfa$rows)]
cfa_df$strong_lib_comp    <- strong_cfa$scores$liberal[match(common_rows, strong_cfa$rows)]
cfa_df$strong_cons_comp   <- strong_cfa$scores$conservative[match(common_rows, strong_cfa$rows)]

pca_df <- df[common_rows, ]

# Predictor-side CFA scores: only the explanatory factors replaced
pred_cfa_df <- pca_df
for (v in c("weak_variety","weak_lib_comp","weak_cons_comp","strong_variety","strong_lib_comp","strong_cons_comp")) {
  pred_cfa_df[[v]] <- cfa_df[[v]]
}
# Outcome-side CFA scores: only the dependent factors replaced
out_cfa_df <- pca_df
for (v in c("arts_participation","solitary_leisure","residual_leisure")) {
  out_cfa_df[[v]] <- cfa_df[[v]]
}

fit_four <- function(outcome, data) {
  f <- function(rhs) as.formula(paste(outcome, "~", rhs))
  list(
    m2 = lm_robust(f(paste(variety_terms, "+", covar_terms)), data = data, se_type = "HC1"),
    m4 = lm_robust(f(paste(variety_terms, "+", composition_terms, "+", covar_terms)),
                   data = data, se_type = "HC1")
  )
}

comp_hyp <- c("weak_lib_comp = 0", "weak_cons_comp = 0",
              "strong_lib_comp = 0", "strong_cons_comp = 0")

arms <- list(
  "PCA (all)"          = list(outcome = pca_df,     predictors = pca_df),
  "CFA predictors only" = list(outcome = pca_df,     predictors = pred_cfa_df),
  "CFA outcome only"   = list(outcome = out_cfa_df, predictors = pca_df),
  "CFA both sides"     = list(outcome = cfa_df,     predictors = cfa_df)
)

# Rebuild the analysis data from the outcome-arm and predictor-arm columns
combine_arm <- function(arm) {
  d <- arm$predictors
  d$arts_participation <- arm$outcome$arts_participation
  d$solitary_leisure   <- arm$outcome$solitary_leisure
  d$residual_leisure   <- arm$outcome$residual_leisure
  d
}

run_outcome <- function(outcome) {
  cat("\n=== Outcome:", outcome, " N =", nrow(pca_df), "(before covariate listwise deletion) ===\n")
  rows <- lapply(names(arms), function(nm) {
    d <- combine_arm(arms[[nm]])
    mods <- fit_four(outcome, d)
    cf <- function(m, t) if (t %in% names(m$coefficients)) round(m$coefficients[t], 3) else NA
    pv <- function(m, t) if (t %in% names(m$p.value)) signif(m$p.value[t], 2) else NA
    w <- linearHypothesis(mods$m4, comp_hyp, white.adjust = "hc1")
    data.frame(
      arm = nm,
      weak_var_m4 = cf(mods$m4, "weak_variety"), p = pv(mods$m4, "weak_variety"),
      strong_var_m4 = cf(mods$m4, "strong_variety"), p = pv(mods$m4, "strong_variety"),
      weak_lib_m4 = cf(mods$m4, "weak_lib_comp"), p2 = pv(mods$m4, "weak_lib_comp"),
      strong_cons_m4 = cf(mods$m4, "strong_cons_comp"), p3 = pv(mods$m4, "strong_cons_comp"),
      comp_chisq = round(w$Chisq[2], 1), comp_p = signif(w$`Pr(>Chisq)`[2], 2),
      stringsAsFactors = FALSE
    )
  })
  print(do.call(rbind, rows), row.names = FALSE)
}

run_outcome("arts_participation")
run_outcome("solitary_leisure")
