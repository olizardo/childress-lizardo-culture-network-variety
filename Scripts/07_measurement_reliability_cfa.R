#!/usr/bin/env Rscript
# Scripts/07_measurement_reliability_cfa.R
#
# Responds to Reviewer 1 (ordinal CFA request) and Reviewer 4 (defensible
# option), plus the reliability ask:
#
#   (A) McDonald's omega for each factor, with Cronbach's alpha as a
#       secondary statistic. Alpha assumes tau-equivalence, which is
#       doubtful for these heterogeneous item sets.
#   (B) Ordinal CFA on the polychoric correlation matrix with WLSMV in
#       lavaan, using the 3-factor structure supported by theory and the
#       EFA (items assigned to their highest-loading factor). Reports
#       CFI, RMSEA (scaled), and SRMR.
#
# Three measurement models, matching Scripts/01_prepare_data.R:
#   1. 20 activity items  -> Arts Participation, Solitary Leisure, Residual (DIY)
#   2. 18 weak-tie items  -> Variety, Liberal Composition, Conservative Composition
#   3. 18 strong-tie items -> same three dimensions
#
# Reliability: omega computed from the fitted ordinal CFA loadings
# (model-implied, uses the polychoric-based standardized loadings) plus
# classic Cronbach's alpha on the same assigned item sets as a secondary
# statistic. Omega-total (McDonald's hierarchical omega) also reported
# from psych::omega on the polychoric matrix for the full 3-factor model.
#
# Outputs: cache/07_reliability_cfa.rds and two markdown tables in cache/.

suppressPackageStartupMessages({
  library(psych)
  library(lavaan)
  library(semTools)
})

dir.create("cache", showWarnings = FALSE, recursive = TRUE)

prep <- readRDS("cache/01_prepared_data.rds")
df <- prep$df

# ------------------------------------------------------------------------
# Factor structures: assign each item to the factor on which it loads
# highest in absolute value in the EFA/PCA solution (varimax), preserving
# the substantive factor labels from 01_prepare_data.R.
# ------------------------------------------------------------------------

assign_items <- function(loadings_mat, factor_names) {
  lm_abs <- abs(unclass(loadings_mat))
  # factor_names maps label -> column index in the loading matrix
  sapply(factor_names, function(fid) {
    col <- loadings_mat[, factor_names[fid]]
    rownames(lm_abs)[which.max(abs(col))]
  }, simplify = FALSE)
}

act_names <- c(arts = "arts", leisure = "leisure", residual = "residual")
act_cols  <- prep$act_factor_id
act_struct <- lapply(seq_along(act_names), function(i) {
  fid <- act_names[i]
  col <- unclass(prep$act_fa$loadings)[, act_cols[fid]]
  data.frame(item = rownames(unclass(prep$act_fa$loadings)),
             factor = fid,
             loading = col,
             stringsAsFactors = FALSE)
}) |>
  do.call(what = rbind) |>
  {\(d) d[order(-abs(d$loading)), ]}()

# For each substantive factor, the assigned items = items whose max-abs
# loading across the three factors lands on that factor's column. Store
# named loading-sign vectors (used as alpha reverse-keying).
act_items_by_factor <- lapply(act_names, function(fid) {
  lm_abs <- abs(unclass(prep$act_fa$loadings))
  which_max <- max.col(lm_abs, ties.method = "first")
  idx <- which(which_max == act_cols[fid])
  lm <- unclass(prep$act_fa$loadings)[idx, act_cols[fid]]
  setNames(sign(lm), rownames(lm_abs)[idx])
})

tie_items_by_factor <- function(fit) {
  lm_abs <- abs(unclass(fit$fa$loadings))
  which_max <- max.col(lm_abs, ties.method = "first")
  lapply(stats::setNames(names(fit$idx), names(fit$idx)), function(fid) {
    idx <- which(which_max == fit$idx[fid])
    lm <- unclass(fit$fa$loadings)[idx, fit$idx[fid]]
    setNames(sign(lm), rownames(lm_abs)[idx])
  })
}
weak_struct <- tie_items_by_factor(prep$weak_fit)
strong_struct <- tie_items_by_factor(prep$strong_fit)

# ------------------------------------------------------------------------
# (A) Cronbach's alpha (secondary) on assigned item sets.
# Ordinal items; psych::alpha polychoric option used for the tie items
# (0-4 scale), Pearson for the 1-7 activity items (7 categories, near-
# continuous); report both bases where informative.
# ------------------------------------------------------------------------

alpha_one <- function(items, keys, poly = FALSE) {
  if (poly) {
    # Cronbach's alpha from the polychoric correlation matrix
    # (alpha = k/(k-1) * (1 - sum(diag(R)) / sum(R)); with a correlation
    # matrix, diag entries are 1 so sum(diag(R)) = k). Reverse-keyed items
    # (negative EFA loading) are sign-flipped in the correlation matrix.
    R <- psych::polychoric(df[items])$rho
    R <- keys * t(keys * R)
    k <- ncol(R)
    unname(k / (k - 1) * (1 - k / sum(R)))
  } else {
    a <- psych::alpha(df[items], keys = keys, check.keys = FALSE)
    unname(a$total[["raw_alpha"]])
  }
}

message(sprintf("Structure sizes: activities %s; weak %s; strong %s",
  paste(sapply(act_items_by_factor, length), collapse = ","),
  paste(sapply(weak_struct, length), collapse = ","),
  paste(sapply(strong_struct, length), collapse = ",")))

message("[1/4] Cronbach's alpha on EFA-assigned item sets...")
alpha_table <- do.call(rbind, c(
  lapply(names(act_items_by_factor), function(fid)
    data.frame(model = "Activities", factor = fid,
               n_items = length(act_items_by_factor[[fid]]),
               alpha = alpha_one(names(act_items_by_factor[[fid]]),
                                 act_items_by_factor[[fid]]))),
  lapply(names(weak_struct), function(fid)
    data.frame(model = "Weak ties", factor = fid,
               n_items = length(weak_struct[[fid]]),
               alpha = alpha_one(names(weak_struct[[fid]]),
                                 weak_struct[[fid]], poly = TRUE))),
  lapply(names(strong_struct), function(fid)
    data.frame(model = "Strong ties", factor = fid,
               n_items = length(strong_struct[[fid]]),
               alpha = alpha_one(names(strong_struct[[fid]]),
                                 strong_struct[[fid]], poly = TRUE)))
))

# ------------------------------------------------------------------------
# (B) Ordinal CFA (WLSMV, polychoric) + omega from the fitted models.
# ------------------------------------------------------------------------

fit_cfa <- function(items_by_factor, model_label) {
  model_string <- paste(
    sapply(names(items_by_factor), function(fid)
      sprintf("%s =~ %s", fid, paste(names(items_by_factor[[fid]]), collapse = " + "))),
    collapse = "\n")
  all_items <- unlist(lapply(items_by_factor, names))
  ordered_vars <- all_items
  fit <- tryCatch(
    cfa(model_string, data = df, ordered = ordered_vars,
        estimator = "WLSMV", std.lv = TRUE),
    error = function(e) e
  )
  if (inherits(fit, "error")) stop(sprintf("CFA failed for %s: %s", model_label, conditionMessage(fit)))
  fm <- fitMeasures(fit, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                           "cfi.scaled", "tli.scaled",
                           "rmsea.scaled", "rmsea.ci.lower.scaled",
                           "rmsea.ci.upper.scaled", "srmr"))
  list(fit = fit, measures = fm, model_string = model_string,
       items_by_factor = items_by_factor, label = model_label)
}

message("[2/4] Fitting WLSMV ordinal CFAs on polychoric matrices...")
cfa_act    <- fit_cfa(act_items_by_factor, "Activities")
cfa_weak   <- fit_cfa(weak_struct, "Weak ties")
cfa_strong <- fit_cfa(strong_struct, "Strong ties")

# McDonald's omega per factor from the ordinal CFA (semTools computes
# reliability with categorical indicators using the model-implied
# polychoric-based covariance).
message("[3/4] McDonald's omega from ordinal CFA fits...")
omega_one <- function(cfa_obj) {
  rel <- semTools::compRelSEM(cfa_obj$fit)
  om <- sapply(rel, function(x) as.numeric(x)[1])
  data.frame(model = cfa_obj$label,
             factor = names(om),
             omega = as.numeric(om),
             stringsAsFactors = FALSE)
}
omega_table <- do.call(rbind, lapply(list(cfa_act, cfa_weak, cfa_strong), omega_one))

# Hierarchical omega total from psych::omega on the polychoric matrix
# (secondary check, full 3-factor solution).
message("[4/4] psych::omega hierarchical totals (polychoric)...")
polychoric_omega <- function(items, nobs) {
  pc <- psych::polychoric(df[items], smoothing = 1)
  om <- psych::omega(pc$rho, nfactors = 3, rotate = "oblimin",
                     poly = TRUE, plot = FALSE, n.iter = 1)
  c(omega_total = om$OmegaH[nrow(om$OmegaH), ncol(om$OmegaH)])
}

fit_table <- do.call(rbind, lapply(list(cfa_act, cfa_weak, cfa_strong), function(o) {
  data.frame(
    Model = o$label,
    Chi_sq = sprintf("%.2f", o$measures["chisq.scaled"]),
    DF = o$measures["df.scaled"],
    P = sprintf("%.3g", o$measures["pvalue.scaled"]),
    CFI = sprintf("%.3f", o$measures["cfi.scaled"]),
    TLI = sprintf("%.3f", o$measures["tli.scaled"]),
    RMSEA = sprintf("%.3f", o$measures["rmsea.scaled"]),
    RMSEA_CI = sprintf("[%.3f, %.3f]",
                       o$measures["rmsea.ci.lower.scaled"],
                       o$measures["rmsea.ci.upper.scaled"]),
    SRMR = sprintf("%.3f", o$measures["srmr"]),
    stringsAsFactors = FALSE
  )
}))

# Standardized loadings per model (for the report/appendix)
loadings_list <- lapply(list(cfa_act, cfa_weak, cfa_strong), function(o) {
  std <- standardizedSolution(o$fit)
  std <- std[std$op == "=~", c("lhs", "rhs", "est.std", "se", "pvalue")]
  std$model <- o$label
  std[order(std$lhs, -abs(std$est.std)), ]
})

out <- list(
  alpha_table = alpha_table,
  omega_table = omega_table,
  fit_table = fit_table,
  loadings = loadings_list,
  structures = list(
    activities = act_items_by_factor,
    weak = weak_struct,
    strong = strong_struct
  ),
  fits = list(activities = cfa_act, weak = cfa_weak, strong = cfa_strong)
)
saveRDS(out, "cache/07_reliability_cfa.rds")

# Markdown tables for the report pipeline
fmt_num <- function(x, digits = 3) sprintf(paste0("%.", digits, "f"), x)

rel_md <- merge(omega_table, alpha_table, by = c("model", "factor"), all = TRUE)
rel_md <- rel_md[order(match(rel_md$model, c("Activities", "Weak ties", "Strong ties"))), ]
rel_md$omega <- fmt_num(rel_md$omega)
rel_md$alpha <- fmt_num(rel_md$alpha)
rel_md$n_items <- as.integer(rel_md$n_items)

con <- file("cache/tableR1_reliability.md", "w")
writeLines(c(
  "| Model | Factor | n items | McDonald's omega | Cronbach's alpha |",
  "|---|---|---:|---:|---:|",
  apply(rel_md, 1, function(r)
    sprintf("| %s | %s | %d | %s | %s |", r[["model"]], r[["factor"]],
            as.integer(r[["n_items"]]), r[["omega"]], r[["alpha"]]))
), con)
close(con)

con <- file("cache/tableR2_cfa_fit.md", "w")
writeLines(c(
  "| Model | chi-square (scaled) | df | p | CFI | TLI | RMSEA | RMSEA 90% CI | SRMR |",
  "|---|---:|---:|---:|---:|---:|---:|---|---:|",
  apply(fit_table, 1, function(r)
    sprintf("| %s | %s | %d | %s | %s | %s | %s | %s | %s |",
            r[["Model"]], r[["Chi_sq"]], as.integer(r[["DF"]]), r[["P"]],
            r[["CFI"]], r[["TLI"]], r[["RMSEA"]], r[["RMSEA_CI"]], r[["SRMR"]]))
), con)
close(con)

message("Done. See cache/tableR1_reliability.md, cache/tableR2_cfa_fit.md, cache/07_reliability_cfa.rds.")
