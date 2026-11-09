#' Split-half check of covariate-conditioned fixed-loading ESEM paths across two rotations.
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

# Complete-case rows of the full analysis sample, then random halves
need_full <- unique(c(all_act, tie_setup("weak")$items, tie_setup("strong")$items, covars))
cc_full <- df[complete.cases(df[need_full]), , drop = FALSE]
set.seed(4217)
half_id <- sample(c("A", "B"), nrow(cc_full), replace = TRUE)
halves <- list(full = seq_len(nrow(cc_full)), A = which(half_id == "A"), B = which(half_id == "B"))

extract_paths <- function(fit, fac_names) {
  pe <- parameterEstimates(fit, standardized = TRUE)
  pe[pe$op == "~" & pe$lhs %in% c("A", "Lis", "R") & pe$rhs %in% fac_names,
     c("lhs", "rhs", "est", "se", "pvalue", "std.all")]
}

fit_one <- function(rows, tie, method) {
  ts <- tie_setup(tie)
  d_rows <- cc_full[rows, , drop = FALSE]
  need <- unique(c(all_act, ts$items, covars))
  d <- make_data(d_rows, need)
  dd <- d_rows[ts$items]
  g_act <- rotate_block(d_rows[all_act], all_act, act_tgt, method)
  g_n <- rotate_block(dd, ts$items, ts$target, method)
  syn <- paste(fixed_block_syntax(g_act, c("A", "Lis", "R")), fixed_block_syntax(g_n, ts$fac_names),
    structural(ts$fac_names, d$cov_names), sep = "\n")
  fit <- tryCatch(
    sem(syn, data = d$dat, ordered = c(all_act, ts$items), estimator = "WLSMV",
      std.lv = TRUE, fixed.x = TRUE),
    error = function(e) e)
  if (inherits(fit, "error") || !lavInspect(fit, "converged")) {
    msg <- if (inherits(fit, "error")) conditionMessage(fit) else "not converged"
    return(list(ok = FALSE, n = d$n, message = msg))
  }
  eig <- eigen(lavInspect(fit, "cov.lv"), symmetric = TRUE, only.values = TRUE)$values
  fm <- fitMeasures(fit, c("cfi.scaled", "rmsea.scaled"))
  list(ok = TRUE, n = d$n, pd = all(eig > 0), cfi = unname(fm[1]), rmsea = unname(fm[2]),
    paths = extract_paths(fit, ts$fac_names), loadings = list(act = g_act, net = g_n))
}

out <- list()
for (method in c("target", "oblimin")) {
  for (h in names(halves)) {
    for (tie in c("weak", "strong")) {
      key <- paste(method, h, tie, sep = "_")
      cat(sprintf("\n===== %s | half %s | %s ties | start %s =====\n", method, h, tie, format(Sys.time(), "%H:%M:%S")))
      res <- fit_one(halves[[h]], tie, method)
      out[[key]] <- res
      if (!res$ok) { cat("FAILED:", res$message, "\n"); next }
      cat(sprintf("N=%d, PD=%s, CFI=%.3f, RMSEA=%.3f\n", res$n, res$pd, res$cfi, res$rmsea))
      print(transform(res$paths, est = round(est, 3), se = round(se, 3),
        pvalue = signif(pvalue, 3), std.all = round(std.all, 3)), row.names = FALSE)
    }
  }
}

saveRDS(out, "cache/diagnostics/sem_gpa_fixed_splithalf.rds")
cat("\nDone.\n")
