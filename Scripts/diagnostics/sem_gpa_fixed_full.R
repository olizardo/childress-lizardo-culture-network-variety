#' Full ESEM-measurement structural models with all loadings fixed at external target rotations.
#'
#' Extends Scripts/diagnostics/sem_cfa_act_gpa_fixed_net.R to the activity battery. For each
#' battery (activities, weak ties, strong ties), a minres solution is computed on the polychoric
#' matrix and rotated to the script 07 target with GPArotation::targetT, outside sem(). The
#' resulting loadings, including cross-loadings, are fixed in the structural model. Rotation is
#' therefore not estimated inside sem(). Residual variances and all structural paths, including
#' disturbance covariances among network and among activity factors, are estimated.
#'
#' Two specifications: a joint model (weak and strong network blocks plus activities, with
#' disturbance covariances across tie types) and separate by-tie models (one network block plus
#' activities). Loadings are fixed, so their sampling uncertainty is not propagated.
#'
#' Estimation: WLSMV, polychoric, std.lv, fixed.x = TRUE, complete cases.
#' Run with sys.source("Scripts/diagnostics/sem_gpa_fixed_full.R", envir = new.env())
#' from the project root. Writes cache/diagnostics/sem_gpa_fixed_full.rds.

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
act_il <- list(arts, leis, resid_act)
act_target <- function(fac_items, fac_names) {
  items <- unlist(fac_items)
  m <- matrix(0, nrow = length(items), ncol = length(fac_names), dimnames = list(items, fac_names))
  for (k in seq_along(fac_items)) m[fac_items[[k]], fac_names[k]] <- 1
  m
}
act_tgt <- act_target(act_il, c("A", "Lis", "R"))

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

# Target rotation outside sem(): minres solution, then GPArotation::targetT, columns
# sign-aligned to their target factor.
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
  list(fac_names = fn, items_list = il, items = unlist(il), target = act_target(il, fn))
}
weak <- tie_setup("weak")
strong <- tie_setup("strong")

report <- function(label, fit, net_fac, n) {
  cat(sprintf("\n===== %s, N = %d =====\n", label, n))
  if (inherits(fit, "error")) { cat("FAILED:", conditionMessage(fit), "\n"); return(invisible(NULL)) }
  if (!lavInspect(fit, "converged")) { cat("NOT CONVERGED; fit indices and paths not reported\n"); return(invisible(NULL)) }
  cat(sprintf("converged=%s npar=%d\n", lavInspect(fit, "converged"), lavInspect(fit, "npar")))
  fm <- fitMeasures(fit, c("chisq.scaled", "df.scaled", "cfi.scaled", "rmsea.scaled", "srmr"))
  cat(sprintf("scaled chisq=%.1f df=%g CFI=%.3f RMSEA=%.3f SRMR=%.3f\n", fm[1], fm[2], fm[3], fm[4], fm[5]))
  eig <- eigen(lavInspect(fit, "cov.lv"), symmetric = TRUE, only.values = TRUE)$values
  cat(sprintf("Latent covariance PD: %s (min eigenvalue %.4f)\n", all(eig > 0), min(eig)))
  pe <- parameterEstimates(fit, standardized = TRUE)
  paths <- pe[pe$op == "~" & pe$lhs %in% c("A", "Lis", "R") & pe$rhs %in% net_fac,
              c("lhs", "rhs", "est", "se", "pvalue", "std.all")]
  cat("\nNetwork-to-activity paths:\n")
  print(transform(paths, est = round(est, 3), se = round(se, 3), pvalue = signif(pvalue, 3),
                  std.all = round(std.all, 3)), row.names = FALSE)
  nc <- pe[pe$op == "~~" & pe$lhs %in% net_fac & pe$rhs %in% net_fac & pe$lhs != pe$rhs, c("lhs", "rhs", "std.all")]
  cat("\nNetwork factor disturbance correlations:\n")
  print(transform(nc, std.all = round(std.all, 3)), row.names = FALSE)
  ac <- pe[pe$op == "~~" & pe$lhs %in% c("A", "Lis", "R") & pe$rhs %in% c("A", "Lis", "R") & pe$lhs != pe$rhs,
           c("lhs", "rhs", "std.all")]
  cat("\nActivity factor disturbance correlations:\n")
  print(transform(ac[!duplicated(t(apply(ac[, c("lhs", "rhs")], 1, sort))), ], std.all = round(std.all, 3)),
        row.names = FALSE)
  invisible(list(paths = paths))
}

fit_model <- function(syn, dat, ordered_vars) {
  tryCatch(sem(syn, data = dat, ordered = ordered_vars, estimator = "WLSMV",
    std.lv = TRUE, fixed.x = TRUE), error = function(e) e)
}

out <- list()

# Activity block: fixed loadings from the external target rotation of the activity battery.
need_act <- all_act
d_act <- df[complete.cases(df[need_act]), need_act, drop = FALSE]
d_act[] <- lapply(d_act, function(x) as.numeric(unclass(x)))
g_act <- gpa_loadings(d_act, all_act, act_tgt)
act_block <- fixed_block_syntax(g_act, c("A", "Lis", "R"))

# Joint model
need <- unique(c(all_act, weak$items, strong$items, covars))
d <- make_data(need)
dd <- df[complete.cases(df[need]), c(weak$items, strong$items), drop = FALSE]
dd[] <- lapply(dd, function(x) as.numeric(unclass(x)))
g_w <- gpa_loadings(dd[weak$items], weak$items, weak$target)
g_s <- gpa_loadings(dd[strong$items], strong$items, strong$target)
syn_joint <- paste(act_block, fixed_block_syntax(g_w, weak$fac_names), fixed_block_syntax(g_s, strong$fac_names),
  structural(c(weak$fac_names, strong$fac_names), d$cov_names), sep = "\n")
fit_joint <- fit_model(syn_joint, d$dat, c(all_act, weak$items, strong$items))
out$joint <- list(fit = fit_joint, loadings = list(act = g_act, weak = g_w, strong = g_s))
report("Joint fixed-ESEM (activities + weak + strong networks)", fit_joint,
  c(weak$fac_names, strong$fac_names), d$n)

# Separate by-tie models
for (tie in list(weak = weak, strong = strong)) {
  tie_name <- if (identical(tie, weak)) "weak" else "strong"
  need <- unique(c(all_act, tie$items, covars))
  d <- make_data(need)
  dd <- df[complete.cases(df[need]), tie$items, drop = FALSE]
  dd[] <- lapply(dd, function(x) as.numeric(unclass(x)))
  g_n <- gpa_loadings(dd, tie$items, tie$target)
  syn <- paste(act_block, fixed_block_syntax(g_n, tie$fac_names),
    structural(tie$fac_names, d$cov_names), sep = "\n")
  fit <- fit_model(syn, d$dat, c(all_act, tie$items))
  out[[paste0("sep_", tie_name)]] <- list(fit = fit, loadings = list(act = g_act, net = g_n))
  report(sprintf("Separate fixed-ESEM (activities + %s network)", tie_name), fit,
    tie$fac_names, d$n)
}

saveRDS(out, "cache/diagnostics/sem_gpa_fixed_full.rds")
cat("\nDone.\n")
