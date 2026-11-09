#' Mixed structural model with network loadings fixed at GPArotation target-rotated values.
#'
#' Diagnostic for where the ESEM instability arises. The network measurement model is the
#' same target-rotated EFA solution as in the ESEM blocks, but the rotation is computed
#' outside sem() (GPArotation::targetT on a minres solution from the polychoric matrix),
#' and the loadings are fixed in the model. Activity factors are the simple-structure CFA.
#' Comparing this with Scripts/diagnostics/sem_cfa_act_esem_net.R isolates the effect of
#' rotating inside sem().
#'
#' Estimation: WLSMV, polychoric, std.lv, fixed.x = TRUE, complete cases.
#' Run with sys.source("Scripts/diagnostics/sem_cfa_act_gpa_fixed_net.R", envir = new.env())
#' from the project root. Writes cache/diagnostics/sem_cfa_act_gpa_fixed_net.rds.

suppressPackageStartupMessages({
  library(lavaan)
})
options(mc.cores = 1)
stopifnot(requireNamespace("psych", quietly = TRUE), requireNamespace("GPArotation", quietly = TRUE))

prep <- readRDS("cache/01_prepared_data.rds")
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df

act <- s07$activities
arts <- names(act$arts)
leis <- names(act$leisure)
resid_act <- names(act$residual)
all_act <- c(arts, leis, resid_act)

act_cfa <- paste0(
  sprintf("A =~ %s\n", paste(arts, collapse = " + ")),
  sprintf("Lis =~ %s\n", paste(leis, collapse = " + ")),
  sprintf("R =~ %s\n", paste(resid_act, collapse = " + ")))
act_disturb <- paste(combn(c("A", "Lis", "R"), 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")

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
  list(dat = dat, cov_names = names(dm), n = nrow(dat))
}

target_for <- function(fac_items, fac_names) {
  items <- unlist(fac_items)
  m <- matrix(0, nrow = length(items), ncol = length(fac_names), dimnames = list(items, fac_names))
  for (k in seq_along(fac_items)) m[fac_items[[k]], fac_names[k]] <- 1
  m
}

# Target rotation outside sem(): minres solution, then GPArotation::targetT with the
# script 07 sorting as targets. Columns are sign-aligned to their target factor.
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

structural <- function(net_fac, cov_names) {
  demo_to_net <- paste(sprintf("%s ~ %s", net_fac, paste(cov_names, collapse = " + ")), collapse = "\n")
  demo_to_act <- paste(sprintf("%s ~ %s", c("A", "Lis", "R"), paste(c(net_fac, cov_names), collapse = " + ")), collapse = "\n")
  net_disturb <- paste(combn(net_fac, 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")
  paste(demo_to_net, demo_to_act, net_disturb, act_disturb, sep = "\n")
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
  tgt <- target_for(il, fn)

  need <- unique(c(all_act, items, covars))
  d <- make_data(need)
  dd <- df[complete.cases(df[need]), items, drop = FALSE]
  dd[] <- lapply(dd, function(x) as.numeric(unclass(x)))
  g <- gpa_loadings(dd, items, tgt)

  syn <- paste(act_cfa, fixed_block_syntax(g, fn), structural(fn, d$cov_names), sep = "\n")
  cat(sprintf("\n===== GPA-fixed network block, %s ties, N = %d =====\n", tie, d$n))
  fit <- tryCatch(
    sem(syn, data = d$dat, ordered = c(all_act, items), estimator = "WLSMV",
      std.lv = TRUE, fixed.x = TRUE),
    error = function(e) e)
  if (inherits(fit, "error")) {
    cat("FAILED:", conditionMessage(fit), "\n")
    out[[tie]] <- list(failed = TRUE, message = conditionMessage(fit))
    next
  }
  cat(sprintf("converged=%s npar=%d\n", lavInspect(fit, "converged"), lavInspect(fit, "npar")))
  fm <- fitMeasures(fit, c("chisq.scaled", "df.scaled", "cfi.scaled", "rmsea.scaled", "srmr"))
  cat(sprintf("scaled chisq=%.1f df=%g CFI=%.3f RMSEA=%.3f SRMR=%.3f\n",
    fm[1], fm[2], fm[3], fm[4], fm[5]))
  eig <- eigen(lavInspect(fit, "cov.lv"), symmetric = TRUE, only.values = TRUE)$values
  cat(sprintf("Latent covariance PD: %s (min eigenvalue %.4f)\n", all(eig > 0), min(eig)))
  pe <- parameterEstimates(fit, standardized = TRUE)
  paths <- pe[pe$op == "~" & pe$lhs %in% c("A", "Lis", "R") & pe$rhs %in% fn,
              c("lhs", "rhs", "est", "se", "pvalue", "std.all")]
  cat("\nNetwork-to-activity paths:\n")
  print(transform(paths, est = round(est, 3), se = round(se, 3), pvalue = signif(pvalue, 3),
                  std.all = round(std.all, 3)), row.names = FALSE)
  nc <- pe[pe$op == "~~" & pe$lhs %in% fn & pe$rhs %in% fn & pe$lhs != pe$rhs, c("lhs", "rhs", "std.all")]
  cat("\nNetwork factor disturbance correlations:\n")
  print(transform(nc[!duplicated(t(apply(nc[, c("lhs", "rhs")], 1, sort))), ], std.all = round(std.all, 3)),
        row.names = FALSE)
  out[[tie]] <- list(failed = FALSE, fit = fit, paths = paths, loadings = g)
}

saveRDS(out, "cache/diagnostics/sem_cfa_act_gpa_fixed_net.rds")
cat("\nDone.\n")
