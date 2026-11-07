#' Full set-ESEM structural models: tie EFA blocks + activities EFA block.
#'
#' Joint model: three EFA blocks in one sem() call (weak-tie 16 items/3 factors,
#' strong-tie 16 items/3 factors, activities 20 items/3 factors), each rotated
#' independently with its own pst target matrix (script 07 sortings; activities
#' varimax targets). Demographics predict network and activity factors; network
#' factors predict activity factors. Disturbance covariances among network factors
#' and among activity factors (cross-block covariances are allowed; within-block
#' covariances are rotation-determined and are not specified).
#'
#' Fallback: if the joint model is inadmissible (non-positive-definite latent
#' covariance, as in the earlier joint six-factor attempt), separate by-tie
#' set-ESEM models (tie block + activities block) are fitted instead, mirroring
#' sem_by_tie_esem_activities.R but with the tie CFA block replaced by an EFA block.
#'
#' Estimation: WLSMV, polychoric, std.lv, fixed.x, complete cases on all model
#' variables (N = 1,227). Education as one ordinal variable; covariates follow
#' Scripts/02_fit_models.R.
#'
#' Run from the project root: Rscript Scripts/diagnostics/sem_set_esem_full.R
#' Writes cache/diagnostics/sem_set_esem_full.rds and a log to stdout.

suppressPackageStartupMessages({
  library(lavaan)
})

prep <- readRDS("cache/01_prepared_data.rds")
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df

act <- s07$activities
arts <- names(act$arts)
leis <- names(act$leisure)
resid_act <- names(act$residual)
all_act <- c(arts, leis, resid_act)

# Targets, one matrix per block. Rows = items in the block's item order,
# columns = factors in the block's factor order.
target_for <- function(fac_items, fac_names) {
  items <- unlist(fac_items)
  m <- matrix(0, nrow = length(items), ncol = length(fac_names),
    dimnames = list(items, fac_names))
  for (k in seq_along(fac_items)) m[fac_items[[k]], fac_names[k]] <- 1
  m
}

act_items_list <- list(arts, leis, resid_act)
target_act <- target_for(act_items_list, c("A", "Lis", "R"))

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
  dat <- cbind(cc[need[need %in% c(all_act, weak$items, strong$items)]], dm)
  cols <- c(all_act, weak$items, strong$items)
  dat[cols[cols %in% names(dat)]] <- lapply(cols[cols %in% names(dat)], function(cn) as.numeric(unclass(dat[[cn]])))
  list(dat = dat, cov_names = cov_names, n = nrow(dat))
}

efa_block_syntax <- function(label, fac_names, items) {
  mods <- paste(sprintf("efa(\"%s\")*%s", label, fac_names), collapse = " + ")
  sprintf("%s =~ %s", mods, paste(items, collapse = " + "))
}

structural <- function(net_fac, cov_names, dat) {
  demo_to_net <- paste(sprintf("%s ~ %s", net_fac, paste(cov_names, collapse = " + ")), collapse = "\n")
  demo_to_act <- paste(sprintf("%s ~ %s", c("A", "Lis", "R"), paste(c(net_fac, cov_names), collapse = " + ")), collapse = "\n")
  net_disturb <- paste(combn(net_fac, 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")
  act_disturb <- paste(combn(c("A", "Lis", "R"), 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")
  paste(demo_to_net, demo_to_act, net_disturb, act_disturb, sep = "\n")
}

fit_and_report <- function(label, syn, dat, ordered_vars, net_fac, cov_names, target_list) {
  cat(sprintf("\n===== %s, N = %d =====\n", label, nrow(dat)))
  fit <- tryCatch(
    sem(syn, data = dat, ordered = ordered_vars, estimator = "WLSMV",
      std.lv = TRUE, fixed.x = TRUE,
      rotation = list("target", target = target_list)),
    error = function(e) e)
  if (inherits(fit, "error")) {
    cat(sprintf("FAILED: %s\n", conditionMessage(fit)))
    return(list(failed = TRUE, message = conditionMessage(fit)))
  }
  cat(sprintf("converged=%s npar=%d\n", lavInspect(fit, "converged"), lavInspect(fit, "npar")))
  cat(sprintf("CFI=%.3f TLI=%.3f RMSEA=%.3f [%.3f, %.3f] SRMR=%.3f\n",
    fitMeasures(fit, "cfi.scaled"), fitMeasures(fit, "tli.scaled"),
    fitMeasures(fit, "rmsea.scaled"), fitMeasures(fit, "rmsea.ci.lower.scaled"),
    fitMeasures(fit, "rmsea.ci.upper.scaled"), fitMeasures(fit, "srmr")))
  eig <- eigen(lavInspect(fit, "cov.lv"))$values
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
  cat("\nNetwork factor residual correlations:\n")
  print(transform(nc[!duplicated(t(apply(nc[, c("lhs", "rhs")], 1, sort))), ],
                  std.all = round(std.all, 3)), row.names = FALSE)
  demo <- pe[pe$op == "~" & pe$lhs %in% c("A", "Lis", "R") & pe$rhs %in% cov_names,
             c("lhs", "rhs", "est", "se", "pvalue")]
  cat("\nDemographic paths on activity factors:\n")
  print(transform(demo, est = round(est, 3), se = round(se, 3), pvalue = signif(pvalue, 3)),
        row.names = FALSE)
  cat("\nActivity factor correlations (rotation-determined):\n")
  print(round(as.matrix(lavInspect(fit, "cor.lv"))[c("A", "Lis", "R"), c("A", "Lis", "R")], 3))
  list(failed = FALSE, fit = fit, paths = paths, label = label)
}

out <- list()

# ---------- Joint model (all three EFA blocks, both tie types) ----------
need <- unique(c(all_act, weak$items, strong$items, covars))
d <- make_data(need)
syn_joint <- paste(
  efa_block_syntax("act", c("A", "Lis", "R"), all_act),
  efa_block_syntax("weak", weak$fac_names, weak$items),
  efa_block_syntax("strong", strong$fac_names, strong$items),
  structural(c(weak$fac_names, strong$fac_names), d$cov_names, d$dat),
  sep = "\n")
out$joint <- fit_and_report("Joint set-ESEM (weak + strong + activities)",
  syn_joint, d$dat, c(all_act, weak$items, strong$items),
  c(weak$fac_names, strong$fac_names), d$cov_names,
  list(act = target_act, weak = weak$target, strong = strong$target))

# ---------- Separate by-tie set-ESEM models ----------
for (tie in list(weak, strong)) {
  need <- unique(c(all_act, tie$items, covars))
  d <- make_data(need)
  syn <- paste(
    efa_block_syntax("act", c("A", "Lis", "R"), all_act),
    efa_block_syntax(if (identical(tie, weak)) "weak" else "strong", tie$fac_names, tie$items),
    structural(tie$fac_names, d$cov_names, d$dat),
    sep = "\n")
  lab <- sprintf("Separate set-ESEM, %s ties", if (identical(tie, weak)) "weak" else "strong")
  block_label <- if (identical(tie, weak)) "weak" else "strong"
  target_list <- list(act = target_act)
  target_list[[block_label]] <- tie$target
  out[[if (identical(tie, weak)) "sep_weak" else "sep_strong"]] <-
    fit_and_report(lab, syn, d$dat, c(all_act, tie$items), tie$fac_names, d$cov_names,
      target_list)
}

saveRDS(out, "cache/diagnostics/sem_set_esem_full.rds")
cat("\nDone.\n")
