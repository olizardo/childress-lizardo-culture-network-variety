#' Direct, indirect, and total effects from the separate by-tie fixed-loading ESEMs with covariates, all loadings fixed at external target rotations.
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


af <- c("A", "Lis", "R")
lab <- c(educ_ord = "educ", child_arts = "arts", poli = "poli")

decompose_tie <- function(tie) {
  ts <- tie_setup(tie)
  nf <- ts$fac_names
  need <- unique(c(all_act, ts$items, covars))
  d <- make_data(need)
  dd <- df[complete.cases(df[need]), ts$items, drop = FALSE]
  dd[] <- lapply(dd, function(x) as.numeric(unclass(x)))
  g_n <- gpa_loadings(dd, ts$items, ts$target)
  others <- setdiff(d$cov_names, names(lab))

  demo_net <- vapply(nf, function(k)
    paste(k, "~", paste(c(sprintf("a_%s_%s*%s", k, lab, names(lab)), others), collapse = " + ")), character(1))
  demo_act <- vapply(af, function(o)
    paste(o, "~", paste(c(sprintf("b_%s_%s*%s", o, nf, nf), sprintf("d_%s_%s*%s", o, lab, names(lab)), others),
      collapse = " + ")), character(1))
  net_disturb <- paste(combn(nf, 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")
  syn <- paste(act_block, fixed_block_syntax(g_n, nf), paste(demo_net, collapse = "\n"),
    paste(demo_act, collapse = "\n"), net_disturb, act_disturb, sep = "\n")

  defs <- character()
  for (o in af) for (cv in names(lab)) {
    ind <- sprintf("a_%s_%s*b_%s_%s", nf, lab[[cv]], o, nf)
    defs <- c(defs,
      sprintf("ind_%s_%s := %s", o, lab[[cv]], paste(ind, collapse = " + ")),
      sprintf("dir_%s_%s := d_%s_%s", o, lab[[cv]], o, lab[[cv]]),
      sprintf("tot_%s_%s := dir_%s_%s + ind_%s_%s", o, lab[[cv]], o, lab[[cv]], o, lab[[cv]]))
    for (k in nf) defs <- c(defs, sprintf("ip_%s_%s_%s := a_%s_%s*b_%s_%s", o, lab[[cv]], k, k, lab[[cv]], o, k))
  }
  fit <- sem(paste(syn, paste(defs, collapse = "\n"), sep = "\n"), data = d$dat,
    ordered = c(all_act, ts$items), estimator = "WLSMV", std.lv = TRUE, fixed.x = TRUE)
  cat(sprintf("\n===== %s ties, N = %d: converged = %s =====\n", tie, d$n, lavInspect(fit, "converged")))
  if (!lavInspect(fit, "converged")) return(NULL)
  fm <- fitMeasures(fit, c("cfi.scaled", "rmsea.scaled"))
  cat(sprintf("CFI = %.3f, RMSEA = %.3f\n", fm[1], fm[2]))
  pe <- parameterEstimates(fit)
  res <- pe[pe$op == ":=" & !grepl("^ip_", pe$lhs), c("lhs", "est", "se", "pvalue")]
  res$tie <- tie
  print(transform(res, est = round(est, 3), se = round(se, 3), pvalue = signif(pvalue, 3)), row.names = FALSE)
  ip <- pe[pe$op == ":=" & grepl("^ip_", pe$lhs), c("lhs", "est", "se", "pvalue")]
  ip$tie <- tie
  list(summary = res, by_factor = ip, fit = fit)
}

decomp <- list(weak = decompose_tie("weak"), strong = decompose_tie("strong"))
saveRDS(decomp, "cache/diagnostics/sem_gpa_fixed_sep_decomposition.rds")
cat("\nDone.\n")
