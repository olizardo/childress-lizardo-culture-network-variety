#' Second-stage fit decomposition for the fixed-loading structural models.
#'
#' Extends sem_gpa_fixed_fit_decomp.R (M0a/M0t/M1/M2) by splitting the M1 -> M2
#' fit loss into its two sources and testing a predictor-specification relaxation.
#' All fits use the same fixed target-rotated loadings and the structural
#' complete-case sample (N = 1,227), matching the reported models.
#'
#' Per tie type:
#'   M1c: covariates predict all six factors, all cross-block residual covariances
#'        free, no network-to-activity regressions (cost of the covariates alone).
#'   M1n: network-to-activity regressions only, no covariates (cost of the network
#'        paths alone).
#'   M2 : the reported covariate model (cached, sem_gpa_fixed_sep_covars.rds).
#'   M2s: M2 plus age squared added to the covariate set predicting all six
#'        factors. Nested in M2, so a scaled WLSMV difference test applies
#'        (lavTestLRT, scaled.shifted).
#'
#' Nested comparisons: M1n vs M2 isolates the covariate regressions given the
#' network paths; M2 vs M2s tests the age-squared addition. M1, M1c, and M2 are
#' not mutually nested and are compared on fit indices only.
#'
#' Run from the project root:
#'   nohup Rscript --no-save --no-restore Scripts/diagnostics/sem_gpa_fixed_fit_decomp_regress.R \
#'     > cache/manuscript/build_logs/fit_decomp_regress.log 2>&1 &
#' Writes cache/diagnostics/sem_gpa_fixed_fit_decomp_regress.rds.

suppressPackageStartupMessages({
  library(lavaan)
})
stopifnot(requireNamespace("psych", quietly = TRUE), requireNamespace("GPArotation", quietly = TRUE))

prep <- readRDS("cache/01_prepared_data.rds")
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
cov_fit <- readRDS("cache/diagnostics/sem_gpa_fixed_sep_covars.rds")
df <- prep$df

act <- s07$activities
arts <- names(act$arts)
leis <- names(act$leis)
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
df$age_sq <- df$age2^2
covars <- c("gender_f", "educ_ord", "child_arts", "income", "age2", "race_f", "poli")
covars_sq <- c(covars, "age_sq")

make_data <- function(need, covset) {
  cc <- df[complete.cases(df[need]), need, drop = FALSE]
  cf <- cc[covset]
  cf[] <- lapply(cf, function(x) if (is.numeric(x)) x else factor(x))
  mm <- model.matrix(as.formula(paste("~", paste(covset, collapse = " + "))), data = cf)[, -1, drop = FALSE]
  dm <- as.data.frame(mm)
  names(dm) <- make.names(names(dm))
  item_cols <- setdiff(need, covset)
  dat <- cbind(cc[item_cols], dm)
  dat[item_cols] <- lapply(dat[item_cols], function(x) as.numeric(unclass(x)))
  list(dat = dat, cov_names = names(dm), n = nrow(dat), raw = cc)
}

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

act_block <- fixed_block_syntax(
  gpa_loadings(
    {d <- df[complete.cases(df[all_act]), all_act, drop = FALSE]; d[] <- lapply(d, function(x) as.numeric(unclass(x))); d},
    all_act, act_tgt),
  c("A", "Lis", "R"))
act_disturb <- paste(combn(c("A", "Lis", "R"), 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")

fit_sem <- function(syn, dat, ordered_vars) {
  sem(syn, data = dat, ordered = ordered_vars, estimator = "WLSMV", std.lv = TRUE, fixed.x = TRUE)
}

fm_names <- c("chisq.scaled", "df.scaled", "cfi.scaled", "rmsea.scaled", "srmr")
report <- function(label, fit) {
  fm <- fitMeasures(fit, fm_names)
  cat(sprintf("%-28s chi2=%7.1f df=%4g CFI=%.3f RMSEA=%.3f SRMR=%.3f\n",
    label, fm[1], fm[2], fm[3], fm[4], fm[5]))
  fm
}

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
  d <- make_data(need, covars)
  dd <- df[complete.cases(df[need]), items, drop = FALSE]
  dd[] <- lapply(dd, function(x) as.numeric(unclass(x)))
  g_n <- gpa_loadings(dd, items, tie_tgt)
  net_block <- fixed_block_syntax(g_n, fn)
  cat(sprintf("\n===== %s ties, N = %d =====\n", tie, d$n))

  # --- M1c: covariates only, cross-block covariances free, no network paths
  cov_to_all <- paste(sprintf("%s ~ %s", c("A", "Lis", "R", fn), paste(d$cov_names, collapse = " + ")),
    collapse = "\n")
  cross_cov <- paste(combn(c("A", "Lis", "R", fn), 2, function(p) sprintf("%s ~~ %s", p[1], p[2])),
    collapse = "\n")
  syn_m1c <- paste(act_block, net_block, cov_to_all, cross_cov, sep = "\n")
  fit_m1c <- fit_sem(syn_m1c, d$dat, c(all_act, items))
  stopifnot(lavInspect(fit_m1c, "converged"))
  fm_m1c <- report("M1c covariates only", fit_m1c)

  # --- M1n: network-to-activity paths only, no covariates
  dat_items <- d$dat[c(all_act, items)]
  net_to_act <- paste(sprintf("%s ~ %s", c("A", "Lis", "R"), paste(fn, collapse = " + ")), collapse = "\n")
  net_disturb <- paste(combn(fn, 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")
  syn_m1n <- paste(act_block, net_block, net_to_act, net_disturb, act_disturb, sep = "\n")
  fit_m1n <- fit_sem(syn_m1n, dat_items, c(all_act, items))
  stopifnot(lavInspect(fit_m1n, "converged"))
  fm_m1n <- report("M1n network paths only", fit_m1n)

  # --- M2: the reported model (cached)
  fm_m2 <- report("M2 reported covariate model", cov_fit[[tie]]$fit)

  # --- M2s: M2 plus age squared
  need_sq <- unique(c(need, "age_sq"))
  d_sq <- make_data(need_sq, covars_sq)
  stopifnot(d_sq$n == d$n)
  syn_m2s <- paste(act_block, net_block,
    sprintf("%s ~ %s", fn, paste(d_sq$cov_names, collapse = " + ")),
    sprintf("%s ~ %s", c("A", "Lis", "R"), paste(c(fn, d_sq$cov_names), collapse = " + ")),
    net_disturb, sep = "\n")
  fit_m2s <- fit_sem(syn_m2s, d_sq$dat, c(all_act, items))
  stopifnot(lavInspect(fit_m2s, "converged"))
  fm_m2s <- report("M2s M2 + age squared", fit_m2s)

  # --- nested scaled difference tests
  cat("\nScaled WLSMV difference tests:\n")
  lrt_cov <- tryCatch(lavTestLRT(fit_m1n, fit_m2s$model_id, method = "satorra.bentler.2010"),
    error = function(e) NULL)
  # M1n vs M2 (adding covariate regressions); use fits directly
  lrt_cov <- tryCatch(lavTestLRT(fit_m1n, cov_fit[[tie]]$fit), error = function(e) NULL)
  if (!is.null(lrt_cov)) print(lrt_cov) else cat("M1n vs M2 LRT failed\n")
  lrt_age2 <- tryCatch(lavTestLRT(cov_fit[[tie]]$fit, fit_m2s), error = function(e) NULL)
  if (!is.null(lrt_age2)) print(lrt_age2) else cat("M2 vs M2s LRT failed\n")

  # --- age-squared effect on the substantive paths
  pe2 <- parameterEstimates(fit_m2s, standardized = TRUE)
  pe1 <- parameterEstimates(cov_fit[[tie]]$fit, standardized = TRUE)
  keep <- function(pe) pe[pe$op == "~" & pe$lhs %in% c("A", "Lis", "R") & pe$rhs %in% fn,
    c("lhs", "rhs", "std.all")]
  cmp <- merge(keep(pe1), keep(pe2), by = c("lhs", "rhs"), suffixes = c("_m2", "_m2s"))
  cmp$diff <- cmp$std.all_m2s - cmp$std.all_m2
  cat("\nNetwork-to-activity standardized paths, M2 vs M2s:\n")
  print(transform(cmp, std.all_m2 = round(std.all_m2, 3), std.all_m2s = round(std.all_m2s, 3),
    diff = round(diff, 3)), row.names = FALSE)

  out[[tie]] <- list(n = d$n, fit_m1c = fit_m1c, fm_m1c = fm_m1c,
    fit_m1n = fit_m1n, fm_m1n = fm_m1n, fm_m2 = fm_m2,
    fit_m2s = fit_m2s, fm_m2s = fm_m2s, path_compare = cmp,
    lrt_cov = lrt_cov, lrt_age2 = lrt_age2)
}

saveRDS(out, "cache/diagnostics/sem_gpa_fixed_fit_decomp_regress.rds")
cat("\nDone.\n")
