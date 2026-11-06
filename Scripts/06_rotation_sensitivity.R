#!/usr/bin/env Rscript
# Scripts/06_rotation_sensitivity.R
#
# Rotation sensitivity analysis for the three factor-analytic measurement
# models (20-item activity battery; 18-item weak-tie and 18-item strong-tie
# group position generators). Re-runs the measurement models and the
# hierarchical OLS sequence under BOTH orthogonal (varimax) and oblique
# (direct oblimin) rotation, using the current data and pipeline logic, so
# the two specifications can be compared side by side.
#
# This script is self-contained and does NOT modify the canonical outputs
# of Scripts/01_prepare_data.R and Scripts/02_fit_models.R (which remain
# varimax). Everything here is written to cache/rotation_sensitivity/.
#
# Outputs
#   cache/rotation_sensitivity/measurement_<rotation>.rds
#   cache/rotation_sensitivity/models_<rotation>.rds
#   cache/rotation_sensitivity/prepared_<rotation>.rds          (layout of cache/01_prepared_data.rds)
#   cache/rotation_sensitivity/canonical_models_<rotation>.rds  (layout of cache/02_models.rds)
#   cache/rotation_sensitivity/varimax_vs_oblimin_results.md
#
# Reproducibility checks performed at run time
#   1. The varimax specification is recomputed here and compared against the
#      canonical cache/01_prepared_data.rds (factor scores must agree to
#      1e-8). If it does not, the script stops, because the oblimin
#      comparison would otherwise be relative to a different baseline.
#   2. Each specification's factor-to-label mapping is asserted to be one-to-one.
#
# Run from the project root:
#   Rscript Scripts/06_rotation_sensitivity.R

suppressPackageStartupMessages({
  library(haven)
  library(psych)
  library(GPArotation)
  library(estimatr)
  library(car)
})

ROTATIONS <- c("varimax", "oblimin")
OUT_DIR <- "cache/rotation_sensitivity"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------
# Item definitions (identical to Scripts/01_prepare_data.R)
# ------------------------------------------------------------------------
activity_items <- c(
  "museum", "art_gallery", "symphony_orchestra_opera", "gardening",
  "carnival_fair_amusement_park", "music_concert_festival", "play_or_musical",
  "library", "fancy_restaurant", "fast_food", "go_for_walk", "exercise_or_yoga",
  "dance_performance", "movie_theater", "read_novel_poem_or_play",
  "attend_sports", "home_auto_repair", "hiking_camping_boating",
  "historic_site", "go_to_festival"
)
network_weak_items <- c(
  "KnowLGBTQ_weak", "KnowLotsaChurch_weak", "KnowLittleChurch_weak", "KnowVeryLib_weak",
  "KnowVeryCons_weak", "KnowAsianppl_weak", "KnowHispanixppl_weak", "KnowBlackppl_weak",
  "KnowWhiteppl_weak", "Know2ndHome_weak", "KnowBornElsewhere_weak", "KnowMENAppl_weak",
  "KnowHawaiiPI_weak", "KnowAmIndianppl_weak", "KnowCityppl_weak", "KnowRuralppl_weak",
  "Knowwomen_weak", "Knowmen_weak"
)
network_strong_items <- gsub("_weak$", "_strong", network_weak_items)

# ------------------------------------------------------------------------
# Measurement models
# ------------------------------------------------------------------------

# Rotation-agnostic factor identification: map substantive labels to the
# column index of the rotated solution using theoretically anchoring items.
label_activity_factors <- function(loadings_mat) {
  leisure_id   <- unname(which.max(colMeans(loadings_mat[c("go_for_walk", "exercise_or_yoga"), , drop = FALSE])))
  remaining_id <- setdiff(1:3, leisure_id)
  residual_id  <- remaining_id[which.max(abs(loadings_mat["fast_food", remaining_id]))]
  arts_id      <- setdiff(remaining_id, residual_id)
  c(arts = arts_id, leisure = leisure_id, residual = residual_id)
}

label_tie_factors <- function(loadings_mat, suffix) {
  anchors <- list(
    variety      = paste0(c("KnowHawaiiPI", "KnowAmIndianppl", "KnowMENAppl"), suffix),
    liberal      = paste0(c("KnowVeryLib", "KnowLGBTQ"), suffix),
    conservative = paste0(c("KnowVeryCons", "KnowLotsaChurch"), suffix)
  )
  sapply(anchors, function(items) unname(which.max(colMeans(loadings_mat[items, , drop = FALSE]))))
}

# Factor-correlation matrix with rows/columns named by substantive label.
# Orthogonal solutions return NULL Phi; the identity is used in that case.
named_phi <- function(fa, idx) {
  nm <- names(idx)[match(seq_len(3), idx)]
  if (is.null(fa$Phi)) {
    return(diag(3) |> `dimnames<-`(list(nm, nm)))
  }
  p <- fa$Phi
  rownames(p) <- colnames(p) <- nm
  p[names(idx), names(idx)]
}

# Flip the sign of a factor so its loading on a marker item is positive,
# carrying the flip through loadings, scores, and Phi together.
orient_factor <- function(fa, factor_col, marker_item, want_positive = TRUE) {
  current <- unclass(fa$loadings)[marker_item, factor_col]
  if ((current > 0) != want_positive) {
    fa$loadings[, factor_col] <- -fa$loadings[, factor_col]
    fa$scores[, factor_col]   <- -fa$scores[, factor_col]
    if (!is.null(fa$Phi)) {
      fa$Phi[, factor_col] <- -fa$Phi[, factor_col]
      fa$Phi[factor_col, ] <- -fa$Phi[factor_col, ]
    }
  }
  fa
}

fit_measurement <- function(df, rotation) {
  message(sprintf("  [%s] activity battery", rotation))
  act_df <- as.data.frame(df[activity_items])
  act_fa <- principal(act_df, nfactors = 3, rotate = rotation, scores = TRUE)
  act_idx <- label_activity_factors(unclass(act_fa$loadings))
  stopifnot(length(unique(act_idx)) == 3)
  # Residual factor: positive on gardening/home repair, negative on fast food.
  act_fa <- orient_factor(act_fa, act_idx["residual"], "fast_food", want_positive = FALSE)

  message(sprintf("  [%s] weak-tie battery", rotation))
  weak_df <- as.data.frame(df[network_weak_items])
  weak_fa <- principal(weak_df, nfactors = 3, rotate = rotation, scores = TRUE)
  weak_idx <- label_tie_factors(unclass(weak_fa$loadings), "_weak")
  stopifnot(length(unique(weak_idx)) == 3)

  message(sprintf("  [%s] strong-tie battery", rotation))
  strong_df <- as.data.frame(df[network_strong_items])
  strong_fa <- principal(strong_df, nfactors = 3, rotate = rotation, scores = TRUE)
  strong_idx <- label_tie_factors(unclass(strong_fa$loadings), "_strong")
  stopifnot(length(unique(strong_idx)) == 3)

  list(
    rotation = rotation,
    act = list(
      fa = act_fa, idx = act_idx, kmo = KMO(cor(act_df, use = "complete.obs"))$MSA,
      var_explained = act_fa$Vaccounted["Cumulative Var", 3],
      phi = named_phi(act_fa, act_idx)
    ),
    weak = list(
      fa = weak_fa, idx = weak_idx, kmo = KMO(cor(weak_df, use = "complete.obs"))$MSA,
      var_explained = weak_fa$Vaccounted["Cumulative Var", 3],
      phi = named_phi(weak_fa, weak_idx)
    ),
    strong = list(
      fa = strong_fa, idx = strong_idx, kmo = KMO(cor(strong_df, use = "complete.obs"))$MSA,
      var_explained = strong_fa$Vaccounted["Cumulative Var", 3],
      phi = named_phi(strong_fa, strong_idx)
    )
  )
}

# Attach scored variables to the analytic data frame.
attach_scores <- function(df, meas) {
  df$arts_participation <- meas$act$fa$scores[, meas$act$idx["arts"]]
  df$solitary_leisure   <- meas$act$fa$scores[, meas$act$idx["leisure"]]
  df$residual_leisure   <- meas$act$fa$scores[, meas$act$idx["residual"]]
  df$weak_variety       <- meas$weak$fa$scores[, meas$weak$idx["variety"]]
  df$weak_lib_comp      <- meas$weak$fa$scores[, meas$weak$idx["liberal"]]
  df$weak_cons_comp     <- meas$weak$fa$scores[, meas$weak$idx["conservative"]]
  df$strong_variety     <- meas$strong$fa$scores[, meas$strong$idx["variety"]]
  df$strong_lib_comp    <- meas$strong$fa$scores[, meas$strong$idx["liberal"]]
  df$strong_cons_comp   <- meas$strong$fa$scores[, meas$strong$idx["conservative"]]
  df
}

# ------------------------------------------------------------------------
# Covariates and hierarchical models (identical to Scripts/01 and 02)
# ------------------------------------------------------------------------
recode_covariates <- function(df) {
  df$educ_cat <- factor(
    dplyr::case_when(
      df$educ %in% 1:3 ~ "HS or less",
      df$educ %in% 4:5 ~ "Some college",
      df$educ == 6     ~ "College degree",
      df$educ == 7     ~ "Graduate degree"
    ),
    levels = c("HS or less", "Some college", "College degree", "Graduate degree")
  )
  df$gender_f <- factor(as_factor(df$gender), levels = c("Woman", "Man", "Non-binary/Trans"))
  df$race_f   <- relevel(factor(as_factor(df$race3)), ref = "white")
  # Variety-count validation measures (Table A2 in the manuscript), as in
  # Scripts/01_prepare_data.R: groups with >= 1 or >= 2 acquaintances.
  weak_mat   <- as.matrix(df[network_weak_items])
  strong_mat <- as.matrix(df[network_strong_items])
  df$weak_variety_count1   <- rowSums(weak_mat >= 2, na.rm = TRUE)
  df$weak_variety_count2   <- rowSums(weak_mat >= 3, na.rm = TRUE)
  df$strong_variety_count1 <- rowSums(strong_mat >= 2, na.rm = TRUE)
  df$strong_variety_count2 <- rowSums(strong_mat >= 3, na.rm = TRUE)
  df
}

covar_terms      <- "gender_f + educ_cat + child_arts + income + age2 + race_f + poli"
variety_terms    <- "weak_variety + strong_variety"
composition_terms <- "weak_lib_comp + weak_cons_comp + strong_lib_comp + strong_cons_comp"

fit_four_models <- function(outcome, data) {
  f <- function(rhs) as.formula(paste(outcome, "~", rhs))
  list(
    m1 = lm_robust(f(variety_terms), data = data, se_type = "HC1"),
    m2 = lm_robust(f(paste(variety_terms, "+", covar_terms)), data = data, se_type = "HC1"),
    m3 = lm_robust(f(paste(variety_terms, "+", composition_terms)), data = data, se_type = "HC1"),
    m4 = lm_robust(f(paste(variety_terms, "+", composition_terms, "+", covar_terms)), data = data, se_type = "HC1")
  )
}

# Hypothesis tests matching Scripts/02_fit_models.R (HC1 Wald tests).
run_wald_tests <- function(mods) {
  wald_test <- function(model) {
    linearHypothesis(model, "weak_variety = strong_variety", white.adjust = "hc1")
  }
  comp_hyp <- c(
    "weak_lib_comp = 0", "weak_cons_comp = 0",
    "strong_lib_comp = 0", "strong_cons_comp = 0"
  )
  nested <- function(model) linearHypothesis(model, comp_hyp, white.adjust = "hc1")
  list(
    arts_wald = wald_test(mods$arts$m2),
    leisure_wald = wald_test(mods$leisure$m2),
    comp_hyp = comp_hyp,
    arts_nested_m1_m3 = nested(mods$arts$m3),
    arts_nested_m2_m4 = nested(mods$arts$m4),
    leisure_nested_m1_m3 = nested(mods$leisure$m3),
    leisure_nested_m2_m4 = nested(mods$leisure$m4),
    residual_nested_m1_m3 = nested(mods$residual$m3),
    residual_nested_m2_m4 = nested(mods$residual$m4)
  )
}

# Re-shape to the object layout that Scripts/audit_manuscript_stats.R reads
# from cache/01_prepared_data.rds and cache/02_models.rds, so the audit can be
# run unchanged against either rotation.
as_canonical_prep <- function(meas, df) {
  list(
    df = df,
    act_fa = meas$act$fa, act_factor_id = meas$act$idx,
    act_kmo = meas$act$kmo, act_var_explained = meas$act$var_explained,
    act_phi = meas$act$phi,
    weak_fit = list(fa = meas$weak$fa, idx = meas$weak$idx, kmo = meas$weak$kmo,
                    var_explained = meas$weak$var_explained, phi = meas$weak$phi),
    strong_fit = list(fa = meas$strong$fa, idx = meas$strong$idx, kmo = meas$strong$kmo,
                      var_explained = meas$strong$var_explained, phi = meas$strong$phi)
  )
}

as_canonical_models <- function(mods, tests) {
  c(
    list(
      arts_models = mods$arts,
      leisure_models = mods$leisure,
      residual_models = mods$residual,
      mdat = NULL
    ),
    tests
  )
}

fit_models <- function(df) {
  model_vars <- c(
    "arts_participation", "solitary_leisure", "residual_leisure",
    "weak_variety", "strong_variety", "weak_lib_comp", "weak_cons_comp",
    "strong_lib_comp", "strong_cons_comp",
    "gender_f", "educ_cat", "educ", "child_arts", "income", "age2", "race_f", "poli"
  )
  mdat <- df[complete.cases(df[model_vars]), model_vars]
  list(
    n = nrow(mdat),
    arts = fit_four_models("arts_participation", mdat),
    leisure = fit_four_models("solitary_leisure", mdat),
    residual = fit_four_models("residual_leisure", mdat)
  )
}

# ------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------
message("[0] Loading raw data and recoding covariates...")
raw_df <- read_dta("grabbagdata111925.dta")
base_df <- recode_covariates(as.data.frame(raw_df))
message(sprintf("  Loaded %d respondents.", nrow(base_df)))

results <- list()
for (rot in ROTATIONS) {
  message(sprintf("[1] Measurement models: %s", rot))
  meas <- fit_measurement(base_df, rot)
  scored_df <- attach_scores(base_df, meas)
  message(sprintf("[2] Hierarchical models: %s", rot))
  mods <- fit_models(scored_df)
  tests <- run_wald_tests(mods)
  saveRDS(meas, file.path(OUT_DIR, sprintf("measurement_%s.rds", rot)))
  saveRDS(mods, file.path(OUT_DIR, sprintf("models_%s.rds", rot)))
  saveRDS(as_canonical_prep(meas, scored_df),
          file.path(OUT_DIR, sprintf("prepared_%s.rds", rot)))
  saveRDS(as_canonical_models(mods, tests),
          file.path(OUT_DIR, sprintf("canonical_models_%s.rds", rot)))
  results[[rot]] <- list(meas = meas, mods = mods, df = scored_df)
}

# Reproducibility check: varimax here must match the canonical pipeline.
if (file.exists("cache/01_prepared_data.rds")) {
  canon <- readRDS("cache/01_prepared_data.rds")$df
  check_vars <- c("arts_participation", "solitary_leisure", "residual_leisure",
                  "weak_variety", "weak_lib_comp", "weak_cons_comp",
                  "strong_variety", "strong_lib_comp", "strong_cons_comp")
  na_match <- vapply(check_vars, function(v) {
    identical(is.na(canon[[v]]), is.na(results$varimax$df[[v]]))
  }, logical(1))
  stopifnot(all(na_match))
  max_diff <- max(vapply(check_vars, function(v) {
    max(abs(canon[[v]] - results$varimax$df[[v]]), na.rm = TRUE)
  }, numeric(1)))
  message(sprintf("Reproducibility check (varimax vs canonical cache): max abs score difference = %.2e", max_diff))
  if (max_diff > 1e-8) {
    stop("Varimax recomputation does not match cache/01_prepared_data.rds; aborting comparison.")
  }
} else {
  message("Canonical cache/01_prepared_data.rds not found; skipping varimax reproducibility check.")
}

# ------------------------------------------------------------------------
# Comparison report
# ------------------------------------------------------------------------
fmt_num <- function(x, digits = 2) ifelse(abs(x) < 0.10 & digits == 2, "", sprintf(paste0("%.", digits, "f"), x))

loading_block <- function(title, key, item_names) {
  rows <- lapply(ROTATIONS, function(rot) {
    b <- results[[rot]]$meas[[key]]
    L <- unclass(b$fa$loadings)
    L <- L[, unname(b$idx[names(b$idx)]), drop = FALSE]
    colnames(L) <- names(b$idx)
    L
  })
  names(rows) <- ROTATIONS
  fac <- colnames(rows[[1]])
  hdr <- c("Item", paste0("Varimax: ", fac), paste0("Oblimin: ", fac))
  body <- vapply(seq_len(nrow(rows$varimax)), function(i) {
    paste0("| ", rownames(rows$varimax)[i], " | ",
           paste(c(fmt_num(rows$varimax[i, ]), fmt_num(rows$oblimin[i, ])), collapse = " | "), " |")
  }, character(1))
  c(paste0("### ", title), "",
    paste0("| ", paste(hdr, collapse = " | "), " |"),
    paste0("|", paste(rep("---", length(hdr)), collapse = "|"), "|"),
    body, "")
}

phi_block <- function(title, key) {
  fmt_m <- function(m) {
    body <- vapply(seq_len(nrow(m)), function(i) {
      paste0("| ", rownames(m)[i], " | ", paste(sprintf("%.3f", m[i, ]), collapse = " | "), " |")
    }, character(1))
    c(paste0("| | ", paste(colnames(m), collapse = " | "), " |"),
      paste0("|", paste(rep("---", ncol(m) + 1), collapse = "|"), "|"),
      body)
  }
  c(paste0("**", title, "**"), "",
    "Varimax (identity by construction):", "", fmt_m(results$varimax$meas[[key]]$phi), "",
    "Oblimin (pattern-based factor correlations):", "", fmt_m(results$oblimin$meas[[key]]$phi), "")
}

summary_rows <- function(battery, key) {
  vapply(ROTATIONS, function(rot) {
    b <- results[[rot]]$meas[[key]]
    pv <- b$fa$Vaccounted["Proportion Var", ]
    pv <- pv[unname(b$idx[names(b$idx)])]
    sprintf("| %s | %s | %s | %s | %.3f | %.3f |", battery, rot,
            paste(names(pv), collapse = " / "),
            paste(sprintf("%.3f", pv), collapse = " / "),
            b$kmo, sum(pv))
  }, character(1))
}

coef_row <- function(model, term) {
  if (!term %in% names(model$coefficients)) return(c("", ""))
  est <- model$coefficients[[term]]
  p   <- model$p.value[[term]]
  star <- ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
  c(sprintf("%.3f%s", est, star), sprintf("%.3f", p))
}

coef_table <- function(outcome_label, outcome_key, model_name, terms) {
  header <- c("Term", paste0(c("Varimax b", "Varimax p", "Oblimin b", "Oblimin p"), collapse = " | "))
  body <- vapply(terms, function(tm) {
    v <- coef_row(results$varimax$mods[[outcome_key]][[model_name]], tm)
    o <- coef_row(results$oblimin$mods[[outcome_key]][[model_name]], tm)
    paste0("| ", tm, " | ", v[1], " | ", v[2], " | ", o[1], " | ", o[2], " |")
  }, character(1))
  c(paste0("#### ", outcome_label, " (", model_name, ")"), "",
    "| Term | Varimax b | Varimax p | Oblimin b | Oblimin p |",
    "|---|---|---|---|---|",
    body, "")
}

score_corr_block <- function() {
  pairs <- c("arts_participation", "solitary_leisure", "residual_leisure",
             "weak_variety", "weak_lib_comp", "weak_cons_comp",
             "strong_variety", "strong_lib_comp", "strong_cons_comp")
  vals <- vapply(pairs, function(v) {
    cor(results$varimax$df[[v]], results$oblimin$df[[v]], use = "complete.obs")
  }, numeric(1))
  c("| Score | r (varimax vs oblimin) |", "|---|---|",
    sprintf("| %s | %.3f |", pairs, vals), "")
}

md <- c(
  "# Rotation Sensitivity: Varimax vs. Oblimin (Current Pipeline)",
  "",
  sprintf("Generated by `Scripts/06_rotation_sensitivity.R` on %s from `grabbagdata111925.dta` (N = %d).",
          format(Sys.Date(), "%Y-%m-%d"), nrow(results$varimax$df)),
  "Both specifications use the same items, the same covariate recoding, and the same complete-case sample. Oblimin loadings are pattern loadings. Loadings with absolute value below 0.10 are blank.",
  "",
  "## Variance Explained and KMO",
  "",
  "| Battery | Rotation | Factors | Proportion Variance | KMO | Total |",
  "|---|---|---|---|---|---|",
  summary_rows("Activities", "act"),
  summary_rows("Weak ties", "weak"),
  summary_rows("Strong ties", "strong"),
  "",
  "Under oblique rotation, per-factor proportions from psych are approximate, since oblique factor shares are not strictly additive. Totals are rotation-invariant.",
  "",
  "## Inter-Factor Correlations (Phi)",
  "",
  phi_block("Activities", "act"),
  phi_block("Weak ties", "weak"),
  phi_block("Strong ties", "strong"),
  "## Loadings",
  "",
  loading_block("Activity battery (20 items)", "act", activity_items),
  loading_block("Weak-tie battery (18 items)", "weak", network_weak_items),
  loading_block("Strong-tie battery (18 items)", "strong", network_strong_items),
  "## Factor Score Agreement",
  "",
  score_corr_block(),
  "## Regression Coefficients",
  "",
  "Coefficients from HC1-robust OLS on the complete-case sample. Significance: * p < .05, ** p < .01, *** p < .001.",
  "",
  coef_table("Public arts participation", "arts", "m4",
             c("weak_variety", "strong_variety", "weak_lib_comp", "weak_cons_comp", "strong_lib_comp", "strong_cons_comp")),
  coef_table("Solitary leisure", "leisure", "m4",
             c("weak_variety", "strong_variety", "weak_lib_comp", "weak_cons_comp", "strong_lib_comp", "strong_cons_comp")),
  coef_table("Residual / DIY practical leisure", "residual", "m4",
             c("weak_variety", "strong_variety", "weak_lib_comp", "weak_cons_comp", "strong_lib_comp", "strong_cons_comp"))
)
writeLines(md, file.path(OUT_DIR, "varimax_vs_oblimin_results.md"))
message(sprintf("Done. Comparison written to %s", file.path(OUT_DIR, "varimax_vs_oblimin_results.md")))
