#' Joint fixed-loading structural ESEM (helpers from split-half script) of covariate-conditioned fixed-loading ESEM paths across two rotations.
#'
#' Separate by-tie models (weak and strong network block plus the activity EFA block, gender
#' items excluded, covariates predicting every factor, WLSMV, fixed.x = TRUE). Loadings are
#' fixed from a minres solution rotated outside sem() under two oblique rotations:
#'   target   : GPArotation::targetT to the script 07 sorting (the current specification)
#'   oblimin  : GPArotation::oblimin, unconstrained
#' Each rotation is recomputed within each random half (set.seed(4217)) of complete cases.
#' Rotated columns are matched to the script 07 sorting by permutation, then sign-aligned.
#'
#' Run from the project root in the background:
#'   nohup Rscript --no-save --no-restore Scripts/diagnostics/sem_gpa_fixed_splithalf.R > cache/diagnostics/sem_gpa_fixed_splithalf.log 2>&1 &
#' Writes cache/diagnostics/sem_gpa_fixed_splithalf.rds.

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

tie_setup <- function(tie) {
  s <- s07[[tie]]
  pre <- if (tie == "weak") "W" else "S"
  fn <- paste0(pre, c("v", "l", "c"))
  il <- lapply(c("variety", "liberal", "conservative"), function(f) {
    x <- names(s[[f]]); x[!grepl("^Know(men|women)_", x)]
  })
  list(fac_names = fn, items_list = il, items = unlist(il), target = target_for(il, fn))
}

# Consecutive category codes. Drops empty top categories (e.g., no category 7 in a half),
# which otherwise make psych::polychoric return NaN. Monotone relabeling only.
dense_codes <- function(x) as.numeric(factor(as.numeric(unclass(x))))

# Rotated loadings from a minres solution on the polychoric matrix of a data frame of items.
rotate_block <- function(d, items, target, method) {
  d[] <- lapply(d, dense_codes)
  rho <- psych::polychoric(d, smooth = TRUE, progress = FALSE, correct = 0)$rho
  uf <- unclass(psych::fa(rho, nfactors = 3, fm = "minres", rotate = "none", n.obs = nrow(d))$loadings)
  g <- if (method == "target") {
    unclass(GPArotation::targetT(uf, Target = ifelse(target == 1, NA, 0))$loadings)
  } else {
    unclass(GPArotation::oblimin(uf)$loadings)
  }
  g <- g[items, , drop = FALSE]
  # Match rotated columns to the script 07 sorting by the best permutation on target-weighted loadings
  perms <- list(c(1,2,3), c(1,3,2), c(2,1,3), c(2,3,1), c(3,1,2), c(3,2,1))
  score <- vapply(perms, function(p) sum(abs(g[, p]) * target), numeric(1))
  g <- g[, perms[[which.max(score)]], drop = FALSE]
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

act_disturb <- paste(combn(c("A", "Lis", "R"), 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")

structural <- function(net_fac, cov_names) {
  demo_to_net <- paste(sprintf("%s ~ %s", net_fac, paste(cov_names, collapse = " + ")), collapse = "\n")
  demo_to_act <- paste(sprintf("%s ~ %s", c("A", "Lis", "R"), paste(c(net_fac, cov_names), collapse = " + ")), collapse = "\n")
  net_disturb <- paste(combn(net_fac, 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")
  paste(demo_to_net, demo_to_act, net_disturb, act_disturb, sep = "\n")
}

make_data <- function(d_rows, need) {
  cc <- d_rows[complete.cases(d_rows[need]), need, drop = FALSE]
  cf <- d_rows[complete.cases(d_rows[need]), covars, drop = FALSE]
  cf[] <- lapply(cf, function(x) if (is.numeric(x)) x else factor(x))
  mm <- model.matrix(as.formula(paste("~", paste(covars, collapse = " + "))), data = cf)[, -1, drop = FALSE]
  dm <- as.data.frame(mm)
  names(dm) <- make.names(names(dm))
  item_cols <- setdiff(need, covars)
  dat <- cbind(cc[item_cols], dm)
  dat[item_cols] <- lapply(dat[item_cols], dense_codes)
  list(dat = dat, cov_names = names(dm), n = nrow(dat))
}


need_full <- unique(c(all_act, tie_setup("weak")$items, tie_setup("strong")$items, covars))
d_rows <- df[complete.cases(df[need_full]), , drop = FALSE]
ts_w <- tie_setup("weak"); ts_s <- tie_setup("strong")
d <- make_data(d_rows, need_full)

g_act <- rotate_block(d_rows[all_act], all_act, act_tgt, "target")
dw <- d_rows[ts_w$items]; ds <- d_rows[ts_s$items]
g_w <- rotate_block(dw, ts_w$items, ts_w$target, "target")
g_s <- rotate_block(ds, ts_s$items, ts_s$target, "target")

net_fac <- c(ts_w$fac_names, ts_s$fac_names)
base <- function(x) sub("_(weak|strong)$", "", x)
wi <- ts_w$items; si <- ts_s$items
matched <- intersect(base(wi), base(si))
matched_syn <- paste(sprintf("%s_weak ~~ %s_strong", matched, matched), collapse = "\n")

syn_core <- paste(fixed_block_syntax(g_act, c("A", "Lis", "R")), fixed_block_syntax(g_w, ts_w$fac_names),
  fixed_block_syntax(g_s, ts_s$fac_names), structural(net_fac, d$cov_names), sep = "\n")
all_items <- c(all_act, wi, si)
run <- function(label, syn) {
  cat(sprintf("\n===== %s, N = %d =====\n", label, d$n))
  fit <- tryCatch(sem(syn, data = d$dat, ordered = all_items, estimator = "WLSMV",
    std.lv = TRUE, fixed.x = TRUE), error = function(e) e)
  if (inherits(fit, "error")) { cat("FAILED:", conditionMessage(fit), "\n"); return(list(fit = NULL, msg = conditionMessage(fit))) }
  conv <- lavInspect(fit, "converged")
  cat(sprintf("converged=%s npar=%d\n", conv, lavInspect(fit, "npar")))
  if (!conv) return(list(fit = fit, converged = FALSE))
  eig <- eigen(lavInspect(fit, "cov.lv"), symmetric = TRUE, only.values = TRUE)$values
  cat(sprintf("Latent covariance PD: %s (min eigenvalue %.4f)\n", all(eig > 0), min(eig)))
  fm <- fitMeasures(fit, c("chisq.scaled", "df.scaled", "cfi.scaled", "rmsea.scaled", "srmr"))
  cat(sprintf("scaled chisq=%.1f df=%g CFI=%.3f RMSEA=%.3f SRMR=%.3f\n", fm[1], fm[2], fm[3], fm[4], fm[5]))
  pe <- parameterEstimates(fit, standardized = TRUE)
  paths <- pe[pe$op == "~" & pe$lhs %in% c("A", "Lis", "R") & pe$rhs %in% net_fac,
    c("lhs", "rhs", "est", "se", "pvalue", "std.all")]
  cat("\nNetwork-to-activity paths:\n")
  print(transform(paths, est = round(est, 3), se = round(se, 3), pvalue = signif(pvalue, 3), std.all = round(std.all, 3)), row.names = FALSE)
  nc <- pe[pe$op == "~~" & pe$lhs %in% net_fac & pe$rhs %in% net_fac & pe$lhs != pe$rhs, c("lhs", "rhs", "std.all")]
  cat("\nNetwork factor disturbance correlations:\n")
  print(transform(nc[!duplicated(t(apply(nc[, c("lhs", "rhs")], 1, sort))), ], std.all = round(std.all, 3)), row.names = FALSE)
  list(fit = fit, converged = TRUE, paths = paths)
}

out <- list()
out$no_matched <- run("Joint fixed-ESEM with covariates, no matched residuals", syn_core)
out$matched <- run(sprintf("Joint fixed-ESEM with covariates, %d matched residuals", length(matched)),
  paste(syn_core, matched_syn, sep = "\n"))
saveRDS(out, "cache/diagnostics/sem_gpa_fixed_joint_covars.rds")
cat("\nDone.\n")
