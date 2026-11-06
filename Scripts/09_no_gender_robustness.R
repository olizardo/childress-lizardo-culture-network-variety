#!/usr/bin/env Rscript
# Scripts/09_no_gender_robustness.R
#
# Robustness check motivated by the CFA diagnostics (see
# measurement_overview_report.md): the Knowmen_/Knowwomen_ items are
# theoretically ambiguous indicators of ideological composition, carry the
# largest modification indices in the weak-tie CFA (men: MI = 343 on
# conservative, 237 on variety), and switch factor allegiance between the
# weak and strong batteries. This script drops all four gender items from
# the tie factor models (16 items per battery), re-derives the weak- and
# strong-tie factor scores with the same anchoring scheme as
# 01_prepare_data.R, and refits the full four-model hierarchical sequence
# (HC1) for both substantive outcomes (Tables 1 and 2), plus the
# weak-vs-strong variety Wald test and the composition-block nested Wald
# tests, comparing everything against the canonical results.
#
# Output: cache/09_no_gender_robustness.rds, cache/tableR4_no_gender.md

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
model_vars <- colnames(mods_canon$mdat)

format_p <- function(p) ifelse(p < 0.001, "< .001", sprintf("%.3f", p))

make_dat <- function(outcome_col) {
  cols <- unique(c(setdiff(model_vars, c("solitary_leisure", "arts_participation")), outcome_col))
  dat <- df[complete.cases(df[cols]), cols]
  dat
}

fit_four_models <- function(outcome, data) {
  list(
    m1 = lm_robust(as.formula(paste(outcome, "~", variety_terms)), data = data, se_type = "HC1"),
    m2 = lm_robust(as.formula(paste(outcome, "~", variety_terms, "+", covar_terms)), data = data, se_type = "HC1"),
    m3 = lm_robust(as.formula(paste(outcome, "~", variety_terms, "+", composition_terms)), data = data, se_type = "HC1"),
    m4 = lm_robust(as.formula(paste(outcome, "~", variety_terms, "+", composition_terms, "+", covar_terms)), data = data, se_type = "HC1")
  )
}

# ------------------------------------------------------------------------
# Refit tie factor models without the gender items (16 items each)
# ------------------------------------------------------------------------
message("[1/3] Refitting tie factor models without gender items...")

label_tie_factors <- function(loadings_mat, suffix) {
  anchors <- list(
    variety      = paste0(c("KnowHawaiiPI", "KnowAmIndianppl", "KnowMENAppl"), suffix),
    liberal      = paste0(c("KnowVeryLib", "KnowLGBTQ"), suffix),
    conservative = paste0(c("KnowVeryCons", "KnowLotsaChurch"), suffix)
  )
  sapply(anchors, function(items) unname(which.max(colMeans(loadings_mat[items, , drop = FALSE]))))
}

refit_ties <- function(items16, suffix) {
  X <- as.data.frame(df[items16])
  fa <- principal(X, nfactors = 3, rotate = "varimax", scores = TRUE)
  idx <- label_tie_factors(unclass(fa$loadings), suffix)
  stopifnot(length(unique(idx)) == 3)
  lapply(stats::setNames(names(idx), names(idx)), function(fid) {
    v <- fa$scores[, idx[fid]]
    as.numeric(scale(v))
  })
}

weak16  <- grep("Know(men|women)_weak$", prep$network_weak_items, value = TRUE, invert = TRUE)
strong16 <- grep("Know(men|women)_strong$", prep$network_strong_items, value = TRUE, invert = TRUE)
stopifnot(length(weak16) == 16, length(strong16) == 16)

weak_ng  <- refit_ties(weak16, "_weak")
strong_ng <- refit_ties(strong16, "_strong")
df$ng_weak_variety <- weak_ng$variety
df$ng_weak_lib_comp <- weak_ng$liberal
df$ng_weak_cons_comp <- weak_ng$conservative
df$ng_strong_variety <- strong_ng$variety
df$ng_strong_lib_comp <- strong_ng$liberal
df$ng_strong_cons_comp <- strong_ng$conservative

# ------------------------------------------------------------------------
# Refit the four-model sequences on the no-gender factor scores
# ------------------------------------------------------------------------
message("[2/3] Refitting Tables 1-2 sequences on no-gender factor scores...")

ng_dat <- df[complete.cases(df[unique(c(setdiff(model_vars, c("arts_participation", "solitary_leisure")),
                                            c("ng_weak_variety", "ng_strong_variety",
                                              "ng_weak_lib_comp", "ng_weak_cons_comp",
                                              "ng_strong_lib_comp", "ng_strong_cons_comp",
                                              "arts_participation", "solitary_leisure")))]), ]
# Rename the no-gender factors to the canonical term names so the shared
# formulas apply unchanged.
ng_dat$weak_variety <- ng_dat$ng_weak_variety
ng_dat$strong_variety <- ng_dat$ng_strong_variety
ng_dat$weak_lib_comp <- ng_dat$ng_weak_lib_comp
ng_dat$weak_cons_comp <- ng_dat$ng_weak_cons_comp
ng_dat$strong_lib_comp <- ng_dat$ng_strong_lib_comp
ng_dat$strong_cons_comp <- ng_dat$ng_strong_cons_comp

ng_arts <- fit_four_models("arts_participation", ng_dat)
ng_leis <- fit_four_models("solitary_leisure", ng_dat)

message("[3/3] Wald tests...")
ng_arts_wald <- car::linearHypothesis(ng_arts$m2, "weak_variety = strong_variety", white.adjust = "hc1")
ng_leis_wald <- car::linearHypothesis(ng_leis$m2, "weak_variety = strong_variety", white.adjust = "hc1")
ng_arts_comp_m2m4 <- car::linearHypothesis(ng_arts$m4, mods_canon$comp_hyp, white.adjust = "hc1")
ng_leis_comp_m2m4 <- car::linearHypothesis(ng_leis$m4, mods_canon$comp_hyp, white.adjust = "hc1")

# ------------------------------------------------------------------------
# Summarize
# ------------------------------------------------------------------------
summarize <- function(models, label, outcome) {
  do.call(rbind, lapply(names(models), function(m) {
    mod <- models[[m]]
    do.call(rbind, lapply(c("weak_variety", "strong_variety",
                            "weak_lib_comp", "weak_cons_comp",
                            "strong_lib_comp", "strong_cons_comp"), function(v) {
      if (is.na(mod$coefficients[v])) return(NULL)
      data.frame(outcome = outcome, variant = label, model = m, term = v,
                 est = unname(mod$coefficients[v]), se = unname(mod$std.error[v]),
                 p = unname(mod$p.value[v]), stringsAsFactors = FALSE)
    }))
  }))
}
all_rows <- rbind(
  summarize(mods_canon$arts_models, "Canonical", "Arts"),
  summarize(mods_canon$leisure_models, "Canonical", "Leisure"),
  summarize(ng_arts, "No gender", "Arts"),
  summarize(ng_leis, "No gender", "Leisure")
)

out <- list(
  rows = all_rows,
  ng_arts_wald = ng_arts_wald, ng_leis_wald = ng_leis_wald,
  ng_arts_comp_m2m4 = ng_arts_comp_m2m4, ng_leis_comp_m2m4 = ng_leis_comp_m2m4,
  weak16 = weak16, strong16 = strong16
)
saveRDS(out, "cache/09_no_gender_robustness.rds")

# Markdown: focus rows = variety terms and composition terms in m4 (the
# fully specified model) plus the m1-m4 variety trajectory.
mk <- function(d) {
  do.call(rbind, lapply(split(d, paste(d$outcome, d$variant, d$term)), function(s) {
    s <- s[order(s$model), ]
    cells <- rep("", 4)
    cells[match(s$model, c("m1", "m2", "m3", "m4"))] <-
      sprintf("%.3f%s", s$est, ifelse(s$p < 0.05, "*", ""))
    data.frame(Outcome = s$outcome[1], Variant = s$variant[1], Term = s$term[1],
               M1 = cells[1], M2 = cells[2], M3 = cells[3], M4 = cells[4],
               stringsAsFactors = FALSE)
  }))
}
tab <- mk(all_rows)
con <- file("cache/tableR4_no_gender.md", "w")
writeLines(c(
  "Variety and composition coefficients (HC1) for Tables 1-2, canonical vs. no-gender tie factor models.",
  "",
  "| Outcome | Variant | Term | M1 | M2 | M3 | M4 |",
  "|---|---|---|---|---|---|---|",
  apply(tab, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")),
  "",
  sprintf("No-gender Wald (weak = strong variety, m2): arts p = %s; leisure p = %s",
          format_p(ng_arts_wald[["Pr(>Chisq)"]][2]), format_p(ng_leis_wald[["Pr(>Chisq)"]][2])),
  sprintf("No-gender composition block (m2->m4): arts chi2 = %.2f, p = %s; leisure chi2 = %.2f, p = %s",
          ng_arts_comp_m2m4$Chisq[2], format_p(ng_arts_comp_m2m4[["Pr(>Chisq)"]][2]),
          ng_leis_comp_m2m4$Chisq[2], format_p(ng_leis_comp_m2m4[["Pr(>Chisq)"]][2]))
), con)
close(con)

message("Done. cache/09_no_gender_robustness.rds, cache/tableR4_no_gender.md")
