#' Separate SEMs by tie strength, two-factor activity CFA (arts; leisure = walk, exercise, reading;
#' residual DIY dropped): weak-tie and strong-tie network factors predicting the activity factors, with demographics predicting all factors.
#'
#' Each model contains one tie type only: three network factors (variety, liberal,
#' conservative) from the pooled oblique sorting (cfa_tie_pooled_oblimin_sorting.R), 16 items
#' per tie type after dropping gender items. The weak and strong models are fit separately, so
#' each gives total effects of that tie type and does not condition on the other tie type.
#' Within-tie residual covariances between weak and strong items are not needed here.
#' Estimation: WLSMV, std.lv, fixed.x = TRUE, N = 1,227 complete cases.
#'
#' Run from the project root: Rscript Scripts/diagnostics/sem_by_tie_strength.R

suppressPackageStartupMessages({
  library(lavaan)
  library(psych)
})

prep <- readRDS("cache/01_prepared_data.rds")
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df

act <- s07$activities
arts <- names(act$arts)
leis <- names(act$leisure)
resid_act <- names(act$residual)
all_act <- c(arts, leis)
outcomes <- c("A", "Lis")

weak_v <- names(s07$weak$variety)
weak_l <- names(s07$weak$liberal)
weak_c <- names(s07$weak$conservative)
strong_v <- names(s07$strong$variety)
strong_l <- names(s07$strong$liberal)
strong_c <- names(s07$strong$conservative)
all_items <- c(weak_v, weak_l, weak_c, strong_v, strong_l, strong_c)
is_gender <- grepl("^Know(men|women)_", all_items)

dat_net <- df[complete.cases(df[all_items]), all_items, drop = FALSE]
dat_net[] <- lapply(dat_net, function(x) as.numeric(unclass(x)))
pooled_efa <- fa(polychoric(dat_net)$rho, nfactors = 3, n.obs = nrow(dat_net),
                 rotate = "oblimin", fm = "wls")
label <- c("variety", "liberal", "conservative")[apply(abs(unclass(pooled_efa$loadings)), 1, which.max)]
names(label) <- all_items

no_g <- all_items[!is_gender]
covars <- c("gender_f", "educ_cat", "child_arts", "income", "age2", "race_f", "poli")
need <- unique(c(all_act, no_g, covars, "educ", "child_arts"))
cc <- df[complete.cases(df[intersect(need, names(df))]), , drop = FALSE]
cat("analytic N =", nrow(cc), "\n")
cf <- cc[covars]
cf[] <- lapply(cf, function(x) if (is.numeric(x)) x else factor(x))
mm <- model.matrix(as.formula(paste("~", paste(covars, collapse = " + "))), data = cf)[, -1, drop = FALSE]
dm <- as.data.frame(mm)
names(dm) <- make.names(names(dm))
cov_names <- names(dm)
dat <- cbind(cc[c(all_act, no_g)], dm)
dat[c(all_act, no_g)] <- lapply(dat[c(all_act, no_g)], function(x) as.numeric(unclass(x)))

act_meas <- paste0(
  "A =~ ", paste(arts, collapse = " + "), "\n",
  "Lis =~ ", paste(leis, collapse = " + "), "\n",
  "")
act_disturb <- paste(combn(outcomes, 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")
line <- function(name, items) if (length(items) > 0) paste0(name, " =~ ", paste(items, collapse = " + "), "\n") else ""

fit_tie <- function(tie) {
  pre <- if (tie == "weak") "W" else "S"
  items_tie <- no_g[grepl(paste0("_", tie, "$"), no_g)]
  fac <- function(ideology) items_tie[label[items_tie] == ideology]
  net_fac <- paste0(pre, c("v", "l", "c"))
  net_meas <- paste0(
    line(net_fac[1], fac("variety")),
    line(net_fac[2], fac("liberal")),
    line(net_fac[3], fac("conservative")))
  demo_to_net <- paste(sprintf("%s ~ %s", net_fac, paste(cov_names, collapse = " + ")), collapse = "\n")
  net_disturb <- paste(combn(net_fac, 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")
  demo_to_act <- paste(sprintf("%s ~ %s", outcomes, paste(c(net_fac, cov_names), collapse = " + ")), collapse = "\n")
  syn <- paste(net_meas, act_meas, demo_to_net, demo_to_act, net_disturb, act_disturb, sep = "\n")
  fit <- sem(syn, data = dat, ordered = c(all_act, items_tie), estimator = "WLSMV",
             std.lv = TRUE, fixed.x = TRUE)
  list(fit = fit, net_fac = net_fac, items = items_tie)
}

report_tie <- function(tie) {
  res <- fit_tie(tie)
  f <- res$fit
  cat(sprintf("\n===== %s ties: %d items, npar = %d =====\n", tie, length(res$items), lavInspect(f, "npar")))
  cat(sprintf("converged=%s PD=%s CFI=%.3f TLI=%.3f RMSEA=%.3f SRMR=%.3f\n",
    lavInspect(f, "converged"), all(eigen(lavInspect(f, "cov.lv"))$values > 0),
    fitMeasures(f, "cfi.scaled"), fitMeasures(f, "tli.scaled"),
    fitMeasures(f, "rmsea.scaled"), fitMeasures(f, "srmr")))
  pe <- parameterEstimates(f, standardized = TRUE)
  act_paths <- pe[pe$op == "~" & pe$lhs %in% outcomes & pe$rhs %in% res$net_fac,
                  c("lhs", "rhs", "est", "se", "pvalue", "std.all")]
  cat("\nNetwork-to-activity paths:\n")
  print(transform(act_paths, est = round(est, 3), se = round(se, 3), pvalue = signif(pvalue, 3),
                  std.all = round(std.all, 3)), row.names = FALSE)
  b <- coef(f)
  idx_all <- sapply(outcomes, function(o) match(sprintf("%s~%s", o, res$net_fac), names(b)))
  for (o in colnames(idx_all)) {
    bb <- b[idx_all[, o]]
    V <- vcov(f)[idx_all[, o], idx_all[, o], drop = FALSE]
    stat <- as.numeric(t(bb) %*% solve(V) %*% bb)
    cat(sprintf("Joint Wald for %-4s network block: chi2(3) = %.2f, p = %.4g\n", o, stat,
                pchisq(stat, df = 3, lower.tail = FALSE)))
  }
  cat("\nNetwork factor disturbance correlations:\n")
  nc <- pe[pe$op == "~~" & pe$lhs %in% res$net_fac & pe$rhs %in% res$net_fac & pe$lhs != pe$rhs, c("lhs", "rhs", "std.all")]
  print(transform(nc[!duplicated(t(apply(nc[, c("lhs", "rhs")], 1, sort))), ], std.all = round(std.all, 3)), row.names = FALSE)
  demo <- pe[pe$op == "~" & pe$lhs %in% c(res$net_fac, outcomes) & pe$rhs %in% cov_names,
             c("lhs", "rhs", "est", "se", "pvalue")]
  cat("\nDemographic paths on network factors:\n")
  print(transform(demo[demo$lhs %in% res$net_fac, ], est = round(est, 3), se = round(se, 3),
                  pvalue = signif(pvalue, 3)), row.names = FALSE)
  cat("\nDemographic paths on activity factors:\n")
  print(transform(demo[demo$lhs %in% outcomes, ], est = round(est, 3), se = round(se, 3),
                  pvalue = signif(pvalue, 3)), row.names = FALSE)
  invisible(res)
}

weak_res <- report_tie("weak")
strong_res <- report_tie("strong")
