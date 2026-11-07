#' ESEM (target rotation) measurement models for the weak-tie and strong-tie batteries.
#'
#' Fills the open item from the polychoric migration: the activities battery has a
#' target-rotation ESEM but the tie batteries do not, because an earlier attempt found
#' that lavaan 0.7-3's efa(..., rotation = "target") ignored the target matrix.
#' This script retries with two specifications and verifies each:
#'   (a) rotation = "target" (lavaan's pst: off-target zeros pulled toward zero,
#'       on-target values ignored), target matrix 1 = primary, 0 = target-to-zero;
#'   (b) rotation = "target.strict" (GPArotation strict target), target matrix
#'       NA = free, 0 = pinned to zero. The 1s from (a) must become NA here, because
#'       strict pins every non-NA cell, and pinning primary loadings at 1 is nonsense.
#'
#' Item sorting: script 07 per-battery sorting (16 items, gender items excluded).
#' Estimation: WLSMV, polychoric, complete cases per battery.
#' Run from the project root: Rscript Scripts/diagnostics/esem_ties_target.R
#' Writes cache/diagnostics/esem_ties_target.rds and a log to stdout.

suppressPackageStartupMessages({
  library(lavaan)
})

prep <- readRDS("cache/01_prepared_data.rds")
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df

fac_names <- c("V", "L", "C")

fit_esem <- function(tie, rot) {
  s <- s07[[tie]]
  items_list <- lapply(c("variety", "liberal", "conservative"), function(f) {
    items <- names(s[[f]])
    items[!grepl("^Know(men|women)_", items)]
  })
  items <- unlist(items_list)

  if (rot == "target") {
    target <- matrix(0, nrow = length(items), ncol = 3, dimnames = list(items, fac_names))
    for (k in seq_along(items_list)) target[items_list[[k]], fac_names[k]] <- 1
  } else {
    target <- matrix(NA_real_, nrow = length(items), ncol = 3, dimnames = list(items, fac_names))
    for (k in seq_along(items_list)) target[items_list[[k]], fac_names[k]] <- 0
  }

  dat <- df[complete.cases(df[items]), items, drop = FALSE]
  dat[] <- lapply(dat, function(x) as.numeric(unclass(x)))

  fit <- efa(data = dat, nfactors = 3, ordered = items, estimator = "WLSMV",
    rotation = list(rot, target = target), output = "lavaan")

  # Off-target check: loadings where the target is 0 must be near zero for
  # the target to count as honored.
  lam <- as.matrix(inspect(fit, "std")$lambda)
  colnames(lam) <- fac_names
  off <- lam[target == 0]
  fm <- fitMeasures(fit, c("chisq.scaled", "df", "cfi.scaled", "rmsea.scaled",
    "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr"))

  cat(sprintf("\n===== %s ties, rotation = %s, N = %d =====\n", tie, rot, nrow(dat)))
  cat(sprintf("converged=%s\n", lavInspect(fit, "converged")))
  cat(sprintf("chi2=%.1f df=%d CFI=%.3f RMSEA=%.3f [%.3f, %.3f] SRMR=%.3f\n",
    fm[1], fm[2], fm[3], fm[4], fm[5], fm[6], fm[7]))
  cat(sprintf("Off-target check: max |loading| where target = 0 is %.3f (%s)\n",
    max(abs(off)), if (max(abs(off)) < 0.15) "TARGET HONORED" else "TARGET LOOSE OR IGNORED"))
  cat("\nStandardized loadings (primary target in parentheses):\n")
  pr <- matrix(sprintf("%.2f", lam), nrow = nrow(lam), ncol = ncol(lam),
    dimnames = list(rownames(lam), colnames(lam)))
  for (i in seq_len(nrow(lam))) {
    tgt <- which(!is.na(target[i, ]) & target[i, ] == 1)
    if (length(tgt)) pr[i, tgt] <- sprintf("(%.2f)", lam[i, tgt])
  }
  print(pr, quote = FALSE)
  cat("\nFactor correlations:\n")
  print(round(as.matrix(lavInspect(fit, "cor.lv")), 3))

  list(fit = fit, target = target, loadings = lam,
    max_off = max(abs(off[!is.na(off)])), fitmeasures = fm)
}

res <- list()
for (tie in c("weak", "strong")) {
  for (rot in c("target", "target.strict")) {
    res[[paste(tie, rot)]] <- tryCatch(fit_esem(tie, rot), error = function(e) {
      cat(sprintf("ERROR in %s/%s: %s\n", tie, rot, conditionMessage(e)))
      e
    })
  }
}

cat("\n\n===== Summary: off-target max |loading| =====\n")
for (nm in names(res)) {
  if (inherits(res[[nm]], "error")) {
    cat(sprintf("%-24s FAILED: %s\n", nm, conditionMessage(res[[nm]])))
  } else {
    cat(sprintf("%-24s max off-target |loading| = %.3f\n", nm, res[[nm]]$max_off))
  }
}

saveRDS(res, "cache/diagnostics/esem_ties_target.rds")

# ---- Scaled difference tests: ESEM (target) vs the baseline three-factor CFA ----
# SB2001-style scaled difference with per-model slot scaling factors, the method
# consistent with the manuscript's Table 1 column (see activities_reporting_gaps.R).
sf_of <- function(f) slot(f, "test")[[2]]$scaling.factor
sb <- function(unres, res_model, label) {
  dfu <- unlist(fitMeasures(unres, "df")); dfr <- unlist(fitMeasures(res_model, "df"))
  m <- dfr - dfu
  cd <- (dfr * sf_of(res_model) - dfu * sf_of(unres)) / m
  Td <- (unlist(fitMeasures(res_model, "chisq.scaled")) -
    unlist(fitMeasures(unres, "chisq.scaled"))) / cd
  p <- pchisq(Td, m, lower.tail = FALSE)
  cat(sprintf("%s: scaled diff chisq = %.1f on %d df, p = %.4g\n", label, Td, m, p))
}
cat("\n===== Scaled difference tests: three-factor CFA (restricted) vs ESEM target =====\n")
for (tie in c("weak", "strong")) {
  s <- s07[[tie]]
  items_list <- lapply(c("variety", "liberal", "conservative"), function(f) {
    items <- names(s[[f]])
    items[!grepl("^Know(men|women)_", items)]
  })
  model_string <- paste(sprintf("%s =~ %s", c("V", "L", "C"),
    vapply(items_list, paste, character(1), collapse = " + ")), collapse = "\n")
  all_items <- unlist(items_list)
  cfa_fit <- cfa(model_string, data = df, ordered = all_items,
    estimator = "WLSMV", std.lv = TRUE)
  cat(sprintf("\nBaseline CFA %s ties: chi2=%.1f df=%d CFI=%.3f RMSEA=%.3f SRMR=%.3f\n",
    tie, fitMeasures(cfa_fit, "chisq.scaled"), fitMeasures(cfa_fit, "df"),
    fitMeasures(cfa_fit, "cfi.scaled"), fitMeasures(cfa_fit, "rmsea.scaled"),
    fitMeasures(cfa_fit, "srmr")))
  sb(res[[paste(tie, "target")]]$fit, cfa_fit, paste(tie, "ties: ESEM target vs CFA"))
}

cat("\nDone.\n")
