#' Separate SEMs by tie strength on the script 07 sorting.
#'
#' Each model has three network factors (variety, liberal, conservative) for one tie type,
#' 16 items after dropping the gender items, predicting the three activity factors (arts,
#' solitary leisure, residual). Demographics predict the network and activity factors.
#'
#' Coding differs from Scripts/02_fit_models.R in one respect: education enters as a single
#' ordinal variable, educ_cat coded 1 (HS or less) to 4 (graduate degree), rather than as
#' dummies. Gender (reference Woman), race (reference White), childhood arts (numeric),
#' income (numeric), age2 (linear age in years), and politics (numeric, 1 to 7) follow the
#' regression scripts.
#'
#' Estimation: WLSMV, std.lv, fixed.x = TRUE, complete cases.
#' Run from the project root: Rscript Scripts/diagnostics/sem_by_tie_script07.R

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

line <- function(name, items) paste0(name, " =~ ", paste(items, collapse = " + "), "\n")
act_meas <- paste0(line("A", arts), line("Lis", leis), line("R", resid_act))
act_disturb <- paste(combn(c("A", "Lis", "R"), 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")

df$educ_ord <- as.numeric(df$educ_cat)
df$educ_ord[is.na(df$educ_cat)] <- NA
covars <- c("gender_f", "educ_ord", "child_arts", "income", "age2", "race_f", "poli")

fit_tie <- function(tie, antonym = FALSE) {
  s <- s07[[tie]]
  pre <- if (tie == "weak") "W" else "S"
  net_fac <- paste0(pre, c("v", "l", "c"))
  fac_items <- lapply(c("variety", "liberal", "conservative"), function(f) {
    items <- names(s[[f]])
    items[!grepl("^Know(men|women)_", items)]
  })
  net_items <- unlist(fac_items)
  net_meas <- paste0(line(net_fac[1], fac_items[[1]]), line(net_fac[2], fac_items[[2]]),
                     line(net_fac[3], fac_items[[3]]))

  need <- unique(c(all_act, net_items, covars))
  cc <- df[complete.cases(df[need]), need, drop = FALSE]
  cf <- cc[covars]
  cf$educ_ord <- cf$educ_ord
  cf[] <- lapply(cf, function(x) if (is.numeric(x)) x else factor(x))
  mm <- model.matrix(as.formula(paste("~", paste(covars, collapse = " + "))), data = cf)[, -1, drop = FALSE]
  dm <- as.data.frame(mm)
  names(dm) <- make.names(names(dm))
  cov_names <- names(dm)

  dat <- cbind(cc[c(all_act, net_items)], dm)
  dat[c(all_act, net_items)] <- lapply(dat[c(all_act, net_items)], function(x) as.numeric(unclass(x)))

  demo_to_net <- paste(sprintf("%s ~ %s", net_fac, paste(cov_names, collapse = " + ")), collapse = "\n")
  demo_to_act <- paste(sprintf("%s ~ %s", c("A", "Lis", "R"), paste(c(net_fac, cov_names), collapse = " + ")), collapse = "\n")
  net_disturb <- paste(combn(net_fac, 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")
  ant_syn <- if (antonym) paste(sprintf("Know%s_%s ~~ Know%s_%s", c("VeryLib", "LittleChurch", "Cityppl"), tie,
                                       c("VeryCons", "LotsaChurch", "Ruralppl"), tie), collapse = "\n") else ""
  syn <- paste(net_meas, act_meas, demo_to_net, demo_to_act, net_disturb, act_disturb, ant_syn, sep = "\n")
  fit <- sem(syn, data = dat, ordered = c(all_act, net_items), estimator = "WLSMV",
             std.lv = TRUE, fixed.x = TRUE)
  list(fit = fit, net_fac = net_fac, cov_names = cov_names, n = nrow(dat), syntax = syn)
}

report_tie <- function(tie, antonym = FALSE) {
  res <- fit_tie(tie, antonym)
  f <- res$fit
  cat(sprintf("\n===== %s ties, N = %d, antonym residuals = %s =====\n", tie, res$n, antonym))
  cat(sprintf("converged=%s npar=%d\n", lavInspect(f, "converged"), lavInspect(f, "npar")))
  cat(sprintf("CFI=%.3f TLI=%.3f RMSEA=%.3f [%.3f, %.3f] SRMR=%.3f\n",
    fitMeasures(f, "cfi.scaled"), fitMeasures(f, "tli.scaled"),
    fitMeasures(f, "rmsea.scaled"), fitMeasures(f, "rmsea.ci.lower.scaled"),
    fitMeasures(f, "rmsea.ci.upper.scaled"), fitMeasures(f, "srmr")))
  cat("Latent covariance matrix positive definite:", all(eigen(lavInspect(f, "cov.lv"))$values > 0), "\n")
  pe <- parameterEstimates(f, standardized = TRUE)
  paths <- pe[pe$op == "~" & pe$lhs %in% c("A", "Lis", "R") & pe$rhs %in% res$net_fac,
              c("lhs", "rhs", "est", "se", "pvalue", "std.all")]
  cat("\nNetwork-to-activity paths:\n")
  print(transform(paths, est = round(est, 3), se = round(se, 3), pvalue = signif(pvalue, 3),
                  std.all = round(std.all, 3)), row.names = FALSE)
  b <- coef(f)
  V <- vcov(f)
  for (o in c("A", "Lis", "R")) {
    idx <- match(sprintf("%s~%s", o, res$net_fac), names(b))
    bb <- b[idx]
    stat <- as.numeric(t(bb) %*% solve(V[idx, idx]) %*% bb)
    cat(sprintf("Joint Wald for %-4s network block: chi2(3) = %.2f, p = %.4g\n",
                o, stat, pchisq(stat, df = 3, lower.tail = FALSE)))
  }
  nc <- pe[pe$op == "~~" & pe$lhs %in% res$net_fac & pe$rhs %in% res$net_fac & pe$lhs != pe$rhs,
           c("lhs", "rhs", "std.all")]
  cat("\nNetwork factor residual correlations:\n")
  print(transform(nc[!duplicated(t(apply(nc[, c("lhs", "rhs")], 1, sort))), ], std.all = round(std.all, 3)),
        row.names = FALSE)
  demo <- pe[pe$op == "~" & pe$lhs %in% c(res$net_fac, "A", "Lis", "R") & pe$rhs %in% res$cov_names,
             c("lhs", "rhs", "est", "se", "pvalue")]
  cat("\nDemographic paths on network factors:\n")
  print(transform(demo[demo$lhs %in% res$net_fac, ], est = round(est, 3), se = round(se, 3),
                  pvalue = signif(pvalue, 3)), row.names = FALSE)
  cat("\nDemographic paths on activity factors:\n")
  print(transform(demo[demo$lhs %in% c("A", "Lis", "R"), ], est = round(est, 3), se = round(se, 3),
                  pvalue = signif(pvalue, 3)), row.names = FALSE)
  invisible(res)
}

res_list <- list()
for (ant in c(FALSE, TRUE)) for (tie in c("weak", "strong")) res_list[[paste(tie, ant)]] <- report_tie(tie, ant)
saveRDS(res_list, "cache/diagnostics/sem_by_tie_script07.rds")
