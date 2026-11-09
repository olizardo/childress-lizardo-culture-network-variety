#' Structural models with simple-structure CFA activity factors and target-rotated ESEM
#' network blocks.
#'
#' Activity side: three-factor CFA (arts, solitary leisure, residual), zero cross-loadings,
#' as in Scripts/diagnostics/sem_by_tie_script07.R.
#' Network side: target-rotated EFA blocks per tie type (script 07 sortings, 16 items,
#' gender excluded), as in Scripts/diagnostics/sem_set_esem_full.R.
#'
#' Two specifications are fitted: a joint model (weak and strong network ESEM blocks in one
#' sem() call, with activity CFA) and separate by-tie models (one network ESEM block plus
#' activity CFA). Demographics predict network and activity factors; network factors predict
#' activity factors; disturbance covariances are specified among network and among activity
#' factors.
#'
#' Estimation: WLSMV, polychoric, std.lv, fixed.x = TRUE, complete cases.
#' Run from the project root: sys.source("Scripts/diagnostics/sem_cfa_act_esem_net.R", envir = new.env())
#' Writes cache/diagnostics/sem_cfa_act_esem_net.rds.

suppressPackageStartupMessages({
  library(lavaan)
})

# Test statistic for WLSMV fits: "scaled.shifted" (default) or "standard" (unscaled),
# set via environment variable SEM_TEST for fit-index recovery runs.
TEST_OPT <- Sys.getenv("SEM_TEST", "scaled.shifted")

prep <- readRDS("cache/01_prepared_data.rds")
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df

act <- s07$activities
arts <- names(act$arts)
leis <- names(act$leisure)
resid_act <- names(act$residual)
all_act <- c(arts, leis, resid_act)

target_for <- function(fac_items, fac_names) {
  items <- unlist(fac_items)
  m <- matrix(0, nrow = length(items), ncol = length(fac_names),
    dimnames = list(items, fac_names))
  for (k in seq_along(fac_items)) m[fac_items[[k]], fac_names[k]] <- 1
  m
}

tie_setup <- function(tie) {
  s <- s07[[tie]]
  pre <- if (tie == "weak") "W" else "S"
  fac_names <- paste0(pre, c("v", "l", "c"))
  items_list <- lapply(c("variety", "liberal", "conservative"), function(f) {
    items <- names(s[[f]])
    items[!grepl("^Know(men|women)_", items)]
  })
  list(items_list = items_list, fac_names = fac_names,
       target = target_for(items_list, fac_names),
       items = unlist(items_list))
}
weak <- tie_setup("weak")
strong <- tie_setup("strong")

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
  cov_names <- names(dm)
  dat <- cbind(cc[setdiff(need, covars)], dm)
  item_cols <- setdiff(need, covars)
  dat[item_cols] <- lapply(dat[item_cols], function(x) as.numeric(unclass(x)))
  list(dat = dat, cov_names = cov_names, n = nrow(dat))
}

efa_block_syntax <- function(label, fac_names, items) {
  mods <- paste(sprintf("efa(\"%s\")*%s", label, fac_names), collapse = " + ")
  sprintf("%s =~ %s", mods, paste(items, collapse = " + "))
}

structural <- function(net_fac, cov_names) {
  demo_to_net <- paste(sprintf("%s ~ %s", net_fac, paste(cov_names, collapse = " + ")), collapse = "\n")
  demo_to_act <- paste(sprintf("%s ~ %s", c("A", "Lis", "R"), paste(c(net_fac, cov_names), collapse = " + ")), collapse = "\n")
  net_disturb <- paste(combn(net_fac, 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")
  paste(demo_to_net, demo_to_act, net_disturb, act_disturb, sep = "\n")
}

fit_and_report <- function(label, syn, dat, ordered_vars, net_fac, cov_names, target_list) {
  cat(sprintf("\n===== %s, N = %d =====\n", label, nrow(dat)))
  fit <- tryCatch(
    sem(syn, data = dat, ordered = ordered_vars, estimator = "WLSMV",
      std.lv = TRUE, fixed.x = TRUE, test = TEST_OPT,
      rotation = list("target", target = target_list)),
    error = function(e) e)
  if (inherits(fit, "error")) {
    cat(sprintf("FAILED: %s\n", conditionMessage(fit)))
    return(list(failed = TRUE, message = conditionMessage(fit), label = label))
  }
  cat(sprintf("converged=%s npar=%d\n", lavInspect(fit, "converged"), lavInspect(fit, "npar")))
  cat(sprintf("CFI=%.3f TLI=%.3f RMSEA=%.3f [%.3f, %.3f] SRMR=%.3f\n",
    fitMeasures(fit, "cfi.scaled"), fitMeasures(fit, "tli.scaled"),
    fitMeasures(fit, "rmsea.scaled"), fitMeasures(fit, "rmsea.ci.lower.scaled"),
    fitMeasures(fit, "rmsea.ci.upper.scaled"), fitMeasures(fit, "srmr")))
  eig <- eigen(lavInspect(fit, "cov.lv"), symmetric = TRUE, only.values = TRUE)$values
  cat("Latent covariance matrix positive definite:", all(eig > 0),
    sprintf("(min eigenvalue %.4f)\n", min(eig)))
  pe <- parameterEstimates(fit, standardized = TRUE)
  paths <- pe[pe$op == "~" & pe$lhs %in% c("A", "Lis", "R") & pe$rhs %in% net_fac,
              c("lhs", "rhs", "est", "se", "pvalue", "std.all")]
  cat("\nNetwork-to-activity paths:\n")
  print(transform(paths, est = round(est, 3), se = round(se, 3), pvalue = signif(pvalue, 3),
                  std.all = round(std.all, 3)), row.names = FALSE)
  b <- coef(fit); V <- vcov(fit)
  for (o in c("A", "Lis", "R")) {
    idx <- match(sprintf("%s~%s", o, net_fac), names(b))
    if (any(is.na(idx))) next
    bb <- b[idx]
    stat <- as.numeric(t(bb) %*% solve(V[idx, idx]) %*% bb)
    cat(sprintf("Joint Wald for %-4s network block: chi2(3) = %.2f, p = %.4g\n",
                o, stat, pchisq(stat, df = 3, lower.tail = FALSE)))
  }
  nc <- pe[pe$op == "~~" & pe$lhs %in% net_fac & pe$rhs %in% net_fac & pe$lhs != pe$rhs,
           c("lhs", "rhs", "std.all")]
  cat("\nNetwork factor disturbance correlations:\n")
  print(transform(nc[!duplicated(t(apply(nc[, c("lhs", "rhs")], 1, sort))), ],
                  std.all = round(std.all, 3)), row.names = FALSE)
  cat("\nActivity factor disturbance correlations:\n")
  ac <- pe[pe$op == "~~" & pe$lhs %in% c("A", "Lis", "R") & pe$rhs %in% c("A", "Lis", "R") & pe$lhs != pe$rhs,
           c("lhs", "rhs", "std.all")]
  print(transform(ac[!duplicated(t(apply(ac[, c("lhs", "rhs")], 1, sort))), ], std.all = round(std.all, 3)),
        row.names = FALSE)
  list(failed = FALSE, fit = fit, paths = paths, label = label, net_fac = net_fac)
}

out <- list()

need <- unique(c(all_act, weak$items, strong$items, covars))
d <- make_data(need)
syn_joint <- paste(
  act_cfa,
  efa_block_syntax("weak", weak$fac_names, weak$items),
  efa_block_syntax("strong", strong$fac_names, strong$items),
  structural(c(weak$fac_names, strong$fac_names), d$cov_names),
  sep = "\n")
out$joint <- fit_and_report("Joint: CFA activities + ESEM networks (weak + strong)",
  syn_joint, d$dat, c(all_act, weak$items, strong$items),
  c(weak$fac_names, strong$fac_names), d$cov_names,
  list(weak = weak$target, strong = strong$target))

for (tie in list(weak = weak, strong = strong)) {
  tie_name <- if (identical(tie, weak)) "weak" else "strong"
  need <- unique(c(all_act, tie$items, covars))
  d <- make_data(need)
  syn <- paste(
    act_cfa,
    efa_block_syntax(tie_name, tie$fac_names, tie$items),
    structural(tie$fac_names, d$cov_names),
    sep = "\n")
  out[[paste0("sep_", tie_name)]] <- fit_and_report(
    sprintf("Separate: CFA activities + ESEM networks, %s ties", tie_name),
    syn, d$dat, c(all_act, tie$items), tie$fac_names, d$cov_names,
    setNames(list(tie$target), tie_name))
}

saveRDS(out, sprintf("cache/diagnostics/sem_cfa_act_esem_net_%s.rds", TEST_OPT))
cat("\nDone.\n")
