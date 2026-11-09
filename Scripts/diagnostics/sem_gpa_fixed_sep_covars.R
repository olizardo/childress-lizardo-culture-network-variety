#' Separate by-tie structural ESEMs with covariates, all loadings fixed at external target rotations.
#'
#' Focused run of the separate by-tie models from Scripts/diagnostics/sem_gpa_fixed_full.R.
#' Each model has one network block (weak or strong; gender items excluded) and the three-block
#' activity EFA, all loadings fixed from a minres solution rotated outside sem() with
#' GPArotation::targetT. Covariates (gender, education, childhood arts, income, age, race,
#' politics) predict every factor, with fixed.x = TRUE.
#'
#' Covariate check: because loadings are fixed, rotation is not evaluated on the model-implied
#' covariance, so the covariate-to-block pathology from lavaan's internal rotation should not
#' occur. The check compares the model-implied correlation of politics with each item against
#' the observed polyserial correlation.
#'
#' Run from the project root: Rscript Scripts/diagnostics/sem_gpa_fixed_sep_covars.R
#' Writes cache/diagnostics/sem_gpa_fixed_sep_covars.rds.

suppressPackageStartupMessages({
  library(lavaan)
})
stopifnot(requireNamespace("psych", quietly = TRUE), requireNamespace("GPArotation", quietly = TRUE),
  requireNamespace("polycor", quietly = TRUE))

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

make_data <- function(need) {
  cc <- df[complete.cases(df[need]), need, drop = FALSE]
  cf <- cc[covars]
  cf[] <- lapply(cf, function(x) if (is.numeric(x)) x else factor(x))
  mm <- model.matrix(as.formula(paste("~", paste(covars, collapse = " + "))), data = cf)[, -1, drop = FALSE]
  dm <- as.data.frame(mm)
  names(dm) <- make.names(names(dm))
  item_cols <- setdiff(need, covars)
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

structural <- function(net_fac, cov_names) {
  demo_to_net <- paste(sprintf("%s ~ %s", net_fac, paste(cov_names, collapse = " + ")), collapse = "\n")
  demo_to_act <- paste(sprintf("%s ~ %s", c("A", "Lis", "R"), paste(c(net_fac, cov_names), collapse = " + ")), collapse = "\n")
  net_disturb <- paste(combn(net_fac, 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")
  paste(demo_to_net, demo_to_act, net_disturb, act_disturb, sep = "\n")
}

tie_setup <- function(tie) {
  s <- s07[[tie]]
  pre <- if (tie == "weak") "W" else "S"
  fn <- paste0(pre, c("v", "l", "c"))
  il <- lapply(c("variety", "liberal", "conservative"), function(f) {
    x <- names(s[[f]]); x[!grepl("^Know(men|women)_", x)]
  })
  list(fac_names = fn, items_list = il, items = unlist(il), target = target_for(il, fn))
}

out <- list()
for (tie in c("weak", "strong")) {
  ts <- tie_setup(tie)
  need <- unique(c(all_act, ts$items, covars))
  d <- make_data(need)
  dd <- df[complete.cases(df[need]), ts$items, drop = FALSE]
  dd[] <- lapply(dd, function(x) as.numeric(unclass(x)))
  g_n <- gpa_loadings(dd, ts$items, ts$target)
  syn <- paste(act_block, fixed_block_syntax(g_n, ts$fac_names),
    structural(ts$fac_names, d$cov_names), sep = "\n")
  cat(sprintf("\n===== Separate fixed-ESEM with covariates, %s ties, N = %d =====\n", tie, d$n))
  fit <- tryCatch(
    sem(syn, data = d$dat, ordered = c(all_act, ts$items), estimator = "WLSMV",
      std.lv = TRUE, fixed.x = TRUE),
    error = function(e) e)
  if (inherits(fit, "error")) {
    cat("FAILED:", conditionMessage(fit), "\n")
    out[[tie]] <- list(failed = TRUE, message = conditionMessage(fit))
    next
  }
  conv <- lavInspect(fit, "converged")
  cat(sprintf("converged=%s npar=%d\n", conv, lavInspect(fit, "npar")))
  if (!conv) { out[[tie]] <- list(failed = FALSE, fit = fit, converged = FALSE); next }
  fm <- fitMeasures(fit, c("chisq.scaled", "df.scaled", "cfi.scaled", "rmsea.scaled", "srmr"))
  cat(sprintf("scaled chisq=%.1f df=%g CFI=%.3f RMSEA=%.3f SRMR=%.3f\n", fm[1], fm[2], fm[3], fm[4], fm[5]))
  eig <- eigen(lavInspect(fit, "cov.lv"), symmetric = TRUE, only.values = TRUE)$values
  cat(sprintf("Latent covariance PD: %s (min eigenvalue %.4f)\n", all(eig > 0), min(eig)))

  pe <- parameterEstimates(fit, standardized = TRUE)
  paths <- pe[pe$op == "~" & pe$lhs %in% c("A", "Lis", "R") & pe$rhs %in% ts$fac_names,
              c("lhs", "rhs", "est", "se", "pvalue", "std.all")]
  cat("\nNetwork-to-activity paths:\n")
  print(transform(paths, est = round(est, 3), se = round(se, 3), pvalue = signif(pvalue, 3),
                  std.all = round(std.all, 3)), row.names = FALSE)
  nc <- pe[pe$op == "~~" & pe$lhs %in% ts$fac_names & pe$rhs %in% ts$fac_names & pe$lhs != pe$rhs,
           c("lhs", "rhs", "std.all")]
  cat("\nNetwork factor disturbance correlations:\n")
  print(transform(nc[!duplicated(t(apply(nc[, c("lhs", "rhs")], 1, sort))), ], std.all = round(std.all, 3)),
        row.names = FALSE)

  # Covariate check: sign of politics -> network factor paths vs loading-weighted observed
  # polyserial correlation of politics with the network items
  pol_obs <- vapply(ts$items, function(v) {
    x <- d$raw[[v]]; p <- d$raw$poli
    ok <- !is.na(x) & !is.na(p)
    polycor::polyserial(p[ok], as.numeric(unclass(x))[ok], ML = TRUE)
  }, numeric(1))
  fac_obs <- vapply(seq_along(ts$fac_names), function(k) {
    w <- g_n[, k]; sum(w * pol_obs) / sum(abs(w))
  }, numeric(1))
  pol_path <- pe[pe$op == "~" & pe$rhs == "poli" & pe$lhs %in% ts$fac_names, c("lhs", "est", "std.all")]
  chk <- data.frame(factor = ts$fac_names, observed_loading_weighted_polyserial = round(fac_obs, 3),
    path_from_poli_std = round(pol_path$std.all[match(ts$fac_names, pol_path$lhs)], 3))
  cat("\nPolitics: observed vs fitted (per network factor):\n")
  print(chk, row.names = FALSE)
  cat("Item-level observed polyserial with politics:\n")
  print(data.frame(item = ts$items, polyserial = round(pol_obs, 3)), row.names = FALSE)

  out[[tie]] <- list(failed = FALSE, converged = TRUE, fit = fit, paths = paths,
    loadings = g_n, poli_check = chk, poli_item_obs = pol_obs)
}

saveRDS(out, "cache/diagnostics/sem_gpa_fixed_sep_covars.rds")
cat("\nDone.\n")
