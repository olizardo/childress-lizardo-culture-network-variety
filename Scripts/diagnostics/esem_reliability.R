#' Fit indices and composite reliability for the set-ESEM structural models.
#'
#' Part 1 (fit indices): the robust (scaled) chi-square correction cannot be
#' computed for the set-ESEM models because lavaan cannot invert the information
#' matrix (tried with information = "default" and "expected"; both fail). The
#' unadjusted ML-discrepancy indices (chisq, CFI, TLI, RMSEA) ARE available and
#' are reported here, alongside SRMR which needs no inversion. These unadjusted
#' values are compared against the CFA baselines' scaled values in the text with
#' an explicit methodological note.
#'
#' Part 2 (reliability): model-implied composite reliability (omega) for each EFA
#' factor, computed from the standardized lambda, psi, and theta matrices:
#'   omega_k = (w' L Psi L' w) / (w' L Psi L' w + w' Theta w)
#' with w the standardized loadings of factor k on all items (cross-loadings
#' included, as in ESEM composite reliability). A target-only variant (off-target
#' loadings zeroed in w) is a secondary descriptive check. The earlier manual
#' formula (sum lam^2 with unique variances 1 - lam^2) was wrong under
#' cross-loadings and is retired.
#'
#' Run from the project root: Rscript Scripts/diagnostics/esem_reliability.R
#' Writes cache/diagnostics/esem_reliability.rds and a log to stdout.

suppressPackageStartupMessages({
  library(lavaan)
})

res_sem <- readRDS("cache/diagnostics/sem_set_esem_full.rds")
res_meas <- readRDS("cache/diagnostics/esem_ties_target.rds")
s07 <- readRDS("cache/07_reliability_cfa.rds")

cat("===== Part 1. Unadjusted fit indices for the set-ESEM structural models =====\n\n")
fit_out <- list()
for (nm in c("sep_weak", "sep_strong")) {
  fit <- res_sem[[nm]]$fit
  cat(sprintf("--- %s ---\n", res_sem[[nm]]$label))
  fm <- fitMeasures(fit, c("chisq", "df", "cfi", "tli", "rmsea",
    "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))
  cat(sprintf("chisq=%.1f df=%d CFI=%.3f TLI=%.3f RMSEA=%.3f [%.3f, %.3f] SRMR=%.3f\n",
    fm[1], fm[2], fm[3], fm[4], fm[5], fm[6], fm[7], fm[8]))
  fit_out[[nm]] <- fm
}

cat("\nCFA measurement baselines, no-gender models (scaled indices; from cache/diagnostics/separate_tie_measurement_cfa.rds):\n")
mt <- readRDS("cache/diagnostics/separate_tie_measurement_cfa.rds")
print(mt[mt$model == "full baseline", c("tie", "N", "CFI", "RMSEA", "SRMR", "VL", "VC", "LC")])

cat("\n===== Part 2. Model-implied composite reliability (omega) per EFA factor =====\n\n")
rel_out <- list()
for (tie in c("weak", "strong")) {
  obj <- res_meas[[paste(tie, "target")]]
  if (inherits(obj, "try-error") || is.null(obj$fit)) next
  fit <- obj$fit
  st <- inspect(fit, "std")
  lam <- as.matrix(st$lambda)
  psi <- as.matrix(st$psi)
  theta <- as.matrix(st$theta)
  tgt <- obj$target
  cat(sprintf("--- %s ties, ESEM (pst target) ---\n", tie))
  for (k in seq_len(ncol(lam))) {
    w <- lam[, k]
    common <- as.numeric(t(w) %*% (lam %*% psi %*% t(lam)) %*% w)
    unique <- as.numeric(t(w) %*% theta %*% w)
    omega <- common / (common + unique)
    w0 <- w; w0[tgt[, k] != 1] <- 0
    common0 <- as.numeric(t(w0) %*% (lam %*% psi %*% t(lam)) %*% w0)
    unique0 <- as.numeric(t(w0) %*% theta %*% w0)
    omega0 <- common0 / (common0 + unique0)
    cat(sprintf("Factor %s: omega = %.3f (target-only variant %.3f)\n",
      colnames(lam)[k], omega, omega0))
    rel_out[[paste(tie, colnames(lam)[k])]] <- c(omega = omega, omega_target_only = omega0)
  }
}

cat("\nCFA-baseline omegas (script 07, compRelSEM):\n")
print(s07$omega_table)

out <- list(fit_indices = fit_out, omegas = rel_out)
saveRDS(out, "cache/diagnostics/esem_reliability.rds")
cat("\nDone.\n")
