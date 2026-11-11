#' Fit decomposition for the fixed-loading structural models: how much of the fit
#' loss comes from fixing the loadings, and how much from the predictor specification.
#'
#' Fits, on the structural complete-case sample (all 20 activity items + the 16 tie
#' items of the battery + the covariates, N = 1,227):
#'   M0a: free target-rotated activity ESEM (lavaan::efa, WLSMV)
#'   M0t: free target-rotated tie ESEM per battery (lavaan::efa, WLSMV)
#'   M1 : fixed-loadings measurement model, both blocks, all six factors freely
#'        correlated, no covariates. Loadings are the same fixed values used in the
#'        reported model (activity block from the N = 1,243 item-complete sample,
#'        tie block from the N = 1,258 item-complete sample).
#'   M2 : the reported covariate model, read from
#'        cache/diagnostics/sem_gpa_fixed_sep_covars.rds (same N = 1,227 sample).
#'
#' Reading: M0 vs M1 isolates the cost of fixing the loadings (with the caveat that
#' M0 fits each block separately, so df and chi-square scale differ). M1 vs M2
#' isolates the predictor specification (covariates predicting all factors and the
#' network-to-activity regressions replacing free cross-block covariances).
#'
#' Run from the project root:
#'   nohup Rscript --no-save --no-restore Scripts/diagnostics/sem_gpa_fixed_fit_decomp.R \
#'     > cache/manuscript/build_logs/fit_decomp.log 2>&1 &
#' Writes cache/diagnostics/sem_gpa_fixed_fit_decomp.rds.

suppressPackageStartupMessages({
  library(lavaan)
})
stopifnot(requireNamespace("psych", quietly = TRUE), requireNamespace("GPArotation", quietly = TRUE))

prep <- readRDS("cache/01_prepared_data.rds")
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df

act <- s07$activities
arts <- names(act$arts)
leis <- names(act$leisure)
resid_act <- names(act$residual)
all_act <- c(arts, leis, resid_act)
act_il <- list(arts, leis, resid_act)

target_for <- function(fac_items, fac_names) {
  items <- unlist(fac_items)
  m <- matrix(0, nrow = length(items), ncol = length(fac_names), dimnames = list(items, fac_names))
  for (k in seq_along(fac_items)) m[fac_items[[k]], fac_names[k]] <- 1
  m
}
act_tgt <- target_for(act_il, c("A", "Lis", "R"))

df$educ_ord <- as.numeric(df$educ_cat)
df$educ_ord[is.na(df$educ_cat)] <- NA
covars <- c("gender_f", "educ_ord", "child_arts", "income", "age2", "race_f", "poli")

gpa_loadings <- function(d, items, target) {
  rho <- psych::polychoric(d, smooth = TRUE, progress = FALSE)$rho
  uf <- unclass(psych::fa(rho, nfactors = 3, fm = "minres", rotate = "none", n.obs = nrow(d))$loadings)
  g <- unclass(GPArotation::targetT(uf, Target = ifelse(target == 1, NA, 0))$loadings)[items, ]
  for (k in 1:3) g[, k] <- g[, k] * sign(sum(g[target[, k] == 1, k]))
  colnames(g) <- colnames(target)
  g
}

fixed_block_syntax <- function(g, fac_names) {
  lines <- vapply(seq_along(fac_names), function(k) {
    sprintf("%s =~ %s", fac_names[k],
      paste(sprintf("%.6f*%s", g[, k], rownames(g)), collapse = " + "))
  }, character(1))
  paste(lines, collapse = "\n")
}

num_cols <- function(d) { d[] <- lapply(d, function(x) as.numeric(unclass(x))); d }

act_block <- fixed_block_syntax(
  gpa_loadings(num_cols(df[complete.cases(df[all_act]), all_act, drop = FALSE]),
    all_act, act_tgt),
  c("A", "Lis", "R"))

fm_names <- c("chisq.scaled", "df.scaled", "cfi.scaled", "rmsea.scaled", "srmr")
cov_fit <- readRDS("cache/diagnostics/sem_gpa_fixed_sep_covars.rds")

out <- list()
for (tie in c("weak", "strong")) {
  s <- s07[[tie]]
  pre <- if (tie == "weak") "W" else "S"
  fn <- paste0(pre, c("v", "l", "c"))
  il <- lapply(c("variety", "liberal", "conservative"), function(f) {
    x <- names(s[[f]]); x[!grepl("^Know(men|women)_", x)]
  })
  items <- unlist(il)
  tie_tgt <- target_for(il, fn)
  need <- unique(c(all_act, items, covars))
  cc <- complete.cases(df[need])
  cat(sprintf("\n===== %s ties, structural sample N = %d =====\n", tie, sum(cc)))

  # --- M0a: free activity ESEM on the structural sample
  act_dat <- num_cols(df[cc, all_act, drop = FALSE])
  fit0a <- efa(data = act_dat, nfactors = 3, ordered = all_act, estimator = "WLSMV",
    rotation = list("target", target = act_tgt), output = "lavaan")
  fm0a <- fitMeasures(fit0a, fm_names)
  cat(sprintf("M0a free activity ESEM:      chi2=%.1f df=%g CFI=%.3f RMSEA=%.3f SRMR=%.3f\n",
    fm0a[1], fm0a[2], fm0a[3], fm0a[4], fm0a[5]))

  # --- M0t: free tie ESEM on the structural sample
  tie_dat <- num_cols(df[cc, items, drop = FALSE])
  fit0t <- efa(data = tie_dat, nfactors = 3, ordered = items, estimator = "WLSMV",
    rotation = list("target", target = tie_tgt), output = "lavaan")
  fm0t <- fitMeasures(fit0t, fm_names)
  cat(sprintf("M0t free tie ESEM:           chi2=%.1f df=%g CFI=%.3f RMSEA=%.3f SRMR=%.3f\n",
    fm0t[1], fm0t[2], fm0t[3], fm0t[4], fm0t[5]))

  # --- M1: fixed loadings, both blocks, all six factors freely correlated, no covariates
  dd <- num_cols(df[complete.cases(df[items]), items, drop = FALSE])
  g_n <- gpa_loadings(dd, items, tie_tgt)
  syn_cov <- paste(combn(c("A", "Lis", "R", fn), 2, function(p) sprintf("%s ~~ %s", p[1], p[2])),
    collapse = "\n")
  syn1 <- paste(act_block, fixed_block_syntax(g_n, fn), syn_cov, sep = "\n")
  fit1 <- sem(syn1, data = cbind(act_dat, tie_dat), ordered = c(all_act, items),
    estimator = "WLSMV", std.lv = TRUE, fixed.x = TRUE)
  stopifnot(lavInspect(fit1, "converged"))
  fm1 <- fitMeasures(fit1, fm_names)
  cat(sprintf("M1 fixed, no covariates:     chi2=%.1f df=%g CFI=%.3f RMSEA=%.3f SRMR=%.3f\n",
    fm1[1], fm1[2], fm1[3], fm1[4], fm1[5]))

  # --- M2: the reported covariate model (cached, same sample)
  fm2 <- fitMeasures(cov_fit[[tie]]$fit, fm_names)
  cat(sprintf("M2 fixed, with covariates:   chi2=%.1f df=%g CFI=%.3f RMSEA=%.3f SRMR=%.3f\n",
    fm2[1], fm2[2], fm2[3], fm2[4], fm2[5]))

  out[[tie]] <- list(n = sum(cc), fm0a = fm0a, fm0t = fm0t, fm1 = fm1, fm2 = fm2,
    fit0a = fit0a, fit0t = fit0t, fit1 = fit1, net_loadings = g_n)
}

saveRDS(out, "cache/diagnostics/sem_gpa_fixed_fit_decomp.rds")
cat("\nDone.\n")
