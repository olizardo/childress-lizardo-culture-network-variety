#!/usr/bin/env Rscript
# Scripts/08_solitary_robustness.R
#
# Robustness check for the solitary-leisure outcome (Table 2), motivated by
# its low reliability (omega = 0.39, alpha = 0.43) and weak inter-item
# correlations among go_for_walk, exercise_or_yoga, read_novel_poem_or_play.
# Because score noise attenuates coefficients toward zero, existing
# significant effects survive, but null findings are weak evidence. This
# script re-estimates the Table 2 four-model sequence (and, where the
# activities factor model itself changes, Table 1 as well) under three
# alternative operationalizations of solitary leisure:
#
#   A. two-item PCA score (go_for_walk + exercise_or_yoga; drops the
#      weakly-loading reading item)
#   B. formative index: row mean of z-scored items across all three items
#      (no common-factor assumption)
#   C. full 19-item activities PCA with read_novel_poem_or_play removed
#      from the item set (reassigns reading out of the leisure slot),
#      re-deriving all three factor scores; Tables 1 and 2 both refit
#
# Each variant is compared against the canonical Table 2 (and Table 1 for
# variant C) on: weak- and strong-tie variety coefficients in m1-m4 with
# HC1 p-values, the weak-vs-strong variety Wald test (m2), and the
# composition-block nested Wald tests (m1->m3, m2->m4).
#
# Output: cache/08_solitary_robustness.rds, cache/tableR3_solitary_robustness.md

suppressPackageStartupMessages({
  library(dplyr)
  library(estimatr)
  library(car)
  library(psych)
  library(GPArotation)
})

dir.create("cache", showWarnings = FALSE, recursive = TRUE)

prep <- readRDS("cache/01_prepared_data.rds")
df <- prep$df
mods_canon <- readRDS("cache/02_models.rds")

covar_terms <- mods_canon$covar_terms
variety_terms <- mods_canon$variety_terms
composition_terms <- mods_canon$composition_terms

fit_four_models <- function(outcome, data) {
  list(
    m1 = lm_robust(as.formula(paste(outcome, "~", variety_terms)), data = data, se_type = "HC1"),
    m2 = lm_robust(as.formula(paste(outcome, "~", variety_terms, "+", covar_terms)), data = data, se_type = "HC1"),
    m3 = lm_robust(as.formula(paste(outcome, "~", variety_terms, "+", composition_terms)), data = data, se_type = "HC1"),
    m4 = lm_robust(as.formula(paste(outcome, "~", variety_terms, "+", composition_terms, "+", covar_terms)), data = data, se_type = "HC1")
  )
}

summarize_sequence <- function(models, label) {
  do.call(rbind, lapply(names(models), function(m) {
    mod <- models[[m]]
    do.call(rbind, lapply(c("weak_variety", "strong_variety"), function(v) {
      data.frame(variant = label, model = m, term = v,
                 est = unname(mod$coefficients[v]),
                 se = unname(mod$std.error[v]),
                 p = unname(mod$p.value[v]),
                 stringsAsFactors = FALSE)
    }))
  }))
}

mdat <- mods_canon$mdat
message(sprintf("Modeling sample: N = %d (same complete cases as Tables 1-2).", nrow(mdat)))

model_vars <- colnames(mdat)   # full canonical variable set from 02_fit_models.R

format_p <- function(p) ifelse(p < 0.001, "< .001", sprintf("%.3f", p))

make_dat <- function(outcome_col) {
  cols <- unique(c(setdiff(model_vars, "solitary_leisure"), outcome_col))
  dat <- df[complete.cases(df[cols]), cols]
  dat
}

# ------------------------------------------------------------------------
# Variant A: two-item PCA (walk + exercise)
# ------------------------------------------------------------------------
message("[1/4] Variant A: two-item PCA score...")
faA <- principal(df[c("go_for_walk", "exercise_or_yoga")], nfactors = 1, scores = TRUE)
if (unclass(faA$loadings)["go_for_walk", 1] < 0) {
  faA$scores[, 1] <- -faA$scores[, 1]
}
df$sol_leis_A <- as.numeric(scale(faA$scores[, 1]))

# ------------------------------------------------------------------------
# Variant B: formative mean-z index (all three items)
# ------------------------------------------------------------------------
message("[2/4] Variant B: formative mean-z index...")
z3 <- lapply(c("go_for_walk", "exercise_or_yoga", "read_novel_poem_or_play"),
             function(x) as.numeric(scale(df[[x]])))
df$sol_leis_B <- as.numeric(scale(rowMeans(do.call(cbind, z3))))

# ------------------------------------------------------------------------
# Variant C: full 19-item activities PCA, reading item removed
# ------------------------------------------------------------------------
message("[3/4] Variant C: 19-item activities PCA (reading dropped)...")
items19 <- setdiff(prep$activity_items, "read_novel_poem_or_play")
act19 <- as.data.frame(df[items19])
faC <- principal(act19, nfactors = 3, rotate = "varimax", scores = TRUE)
lmC <- unclass(faC$loadings)
# Relabel with the same anchors as 01_prepare_data.R
leisure_id   <- unname(which.max(colMeans(lmC[c("go_for_walk", "exercise_or_yoga"), ])))
remaining_id <- setdiff(1:3, leisure_id)
residual_id  <- remaining_id[which.max(abs(lmC["fast_food", remaining_id]))]
arts_id      <- setdiff(remaining_id, residual_id)
stopifnot(length(unique(c(leisure_id, residual_id, arts_id))) == 3)
if (lmC["fast_food", residual_id] > 0) {
  faC$loadings[, residual_id] <- -faC$loadings[, residual_id]
  faC$scores[, residual_id] <- -faC$scores[, residual_id]
  lmC <- unclass(faC$loadings)
}
if (lmC["go_for_walk", leisure_id] < 0) {
  faC$scores[, leisure_id] <- -faC$scores[, leisure_id]
}
df$arts_participation_C <- as.numeric(scale(faC$scores[, arts_id]))
df$sol_leis_C <- as.numeric(scale(faC$scores[, leisure_id]))
df$residual_leisure_C <- as.numeric(scale(faC$scores[, residual_id]))

# Sanity: variant C should reproduce the canonical arts factor closely
r_arts_canon <- suppressWarnings(cor(df$arts_participation, df$arts_participation_C,
                                     use = "complete.obs"))

# ------------------------------------------------------------------------
# Refit sequences and summarize
# ------------------------------------------------------------------------
message("[4/4] Refitting model sequences under each variant...")
variants <- list(
  list(label = "A: 2-item PCA", outcome = "sol_leis_A", refit_arts = FALSE),
  list(label = "B: formative mean-z", outcome = "sol_leis_B", refit_arts = FALSE),
  list(label = "C: 19-item PCA reassignment", outcome = "sol_leis_C", refit_arts = TRUE)
)

results <- list()
for (v in variants) {
  dat <- make_dat(v$outcome)
  results[[v$label]] <- list(
    leisure = summarize_sequence(fit_four_models(v$outcome, dat), v$label),
    wald = car::linearHypothesis(fit_four_models(v$outcome, dat)$m2,
                                 "weak_variety = strong_variety",
                                 white.adjust = "hc1"),
    comp_m1m3 = car::linearHypothesis(fit_four_models(v$outcome, dat)$m3,
                                      mods_canon$comp_hyp, white.adjust = "hc1"),
    comp_m2m4 = car::linearHypothesis(fit_four_models(v$outcome, dat)$m4,
                                      mods_canon$comp_hyp, white.adjust = "hc1"),
    arts = if (v$refit_arts) summarize_sequence(fit_four_models("arts_participation_C", make_dat("arts_participation_C")), v$label) else NULL
  )
}

# Canonical comparison rows
results[["Canonical"]] <- list(
  leisure = summarize_sequence(mods_canon$leisure_models, "Canonical"),
  wald = mods_canon$leisure_wald,
  comp_m1m3 = mods_canon$leisure_nested_m1_m3,
  comp_m2m4 = mods_canon$leisure_nested_m2_m4,
  arts = NULL
)

# ------------------------------------------------------------------------
# Markdown summary table
# ------------------------------------------------------------------------
rows <- do.call(rbind, lapply(names(results), function(lbl) {
  r <- results[[lbl]]$leisure
  w <- results[[lbl]]$wald
  do.call(rbind, lapply(c("weak_variety", "strong_variety"), function(t) {
    sub <- r[r$term == t, ]
    paste_cols <- sapply(seq_len(nrow(sub)), function(i)
      sprintf("%.3f%s (%.3f, p = %s)", sub$est[i],
              ifelse(sub$p[i] < 0.05, "*", ""),
              sub$se[i], format_p(sub$p[i])))
    data.frame(Variant = lbl, Term = t,
               M1 = paste_cols[1], M2 = paste_cols[2],
               M3 = paste_cols[3], M4 = paste_cols[4],
               `Weak=Strong Wald (m2) p` = sprintf("%.3f", tail(na.omit(w[["Pr(>Chisq)"]]), 1)),
               stringsAsFactors = FALSE, check.names = FALSE)
  }))
}))

out <- list(results = results, r_arts_canon = r_arts_canon, rows = rows)
saveRDS(out, "cache/08_solitary_robustness.rds")

con <- file("cache/tableR3_solitary_robustness.md", "w")
writeLines(c(
  "Coefficients on weak- and strong-tie variety for the solitary-leisure outcome,",
  "canonical vs. three alternative operationalizations (HC1 robust SEs).",
  "",
  "| Variant | Term | M1 | M2 | M3 | M4 | Weak=Strong Wald (m2) p |",
  "|---|---|---|---|---|---|---:|",
  apply(rows, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
), con)
close(con)

message("Done. cache/08_solitary_robustness.rds, cache/tableR3_solitary_robustness.md")
