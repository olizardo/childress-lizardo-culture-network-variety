#' Full SEM: six network factors (weak and strong variety, liberal, conservative) predicting
#' the three activity factors, with demographics predicting network and activity factors.
#'
#' Measurement: script 07 per-battery sorting, 16 no-gender items per tie type, with no
#' cross-tie residuals. The six network factors are correlated through free disturbance
#' covariances, since the demographics predict them.
#' Covariates are coded as in Scripts/02_fit_models.R: gender_f (reference Woman), educ_cat
#' (reference HS or less), child_arts (numeric), income (numeric), age2 (linear age in years),
#' race_f (reference White), and poli (numeric, 1 to 7).
#' Estimation: WLSMV, std.lv, fixed.x = TRUE, complete cases.
#'
#' Run from the project root: Rscript Scripts/diagnostics/sem_full_six_factor.R

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

tie_items <- function(tie) {
  items <- unlist(lapply(s07[[tie]], names), use.names = FALSE)
  items[!grepl("^Know(men|women)_", items)]
}
net_items <- c(tie_items("weak"), tie_items("strong"))
net_fac <- c("Wv", "Wl", "Wc", "Sv", "Sl", "Sc")
fac_items <- function(tie, ideology) {
  items <- names(s07[[tie]][[ideology]])
  items[!grepl("^Know(men|women)_", items)]
}
line <- function(name, items) paste0(name, " =~ ", paste(items, collapse = " + "), "\n")
net_meas <- paste0(
  line("Wv", fac_items("weak", "variety")),
  line("Wl", fac_items("weak", "liberal")),
  line("Wc", fac_items("weak", "conservative")),
  line("Sv", fac_items("strong", "variety")),
  line("Sl", fac_items("strong", "liberal")),
  line("Sc", fac_items("strong", "conservative")))

act_meas <- paste0(
  line("A", arts), line("Lis", leis), line("R", resid_act))
act_disturb <- paste(combn(c("A", "Lis", "R"), 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")
net_disturb <- paste(combn(net_fac, 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")

covars <- c("gender_f", "educ_cat", "child_arts", "income", "age2", "race_f", "poli")
need <- unique(c(all_act, net_items, covars))
cc <- df[complete.cases(df[need]), need, drop = FALSE]
cat("analytic N =", nrow(cc), "\n")

cf <- cc[covars]
cf[] <- lapply(cf, function(x) if (is.numeric(x)) x else factor(x))
mm <- model.matrix(as.formula(paste("~", paste(covars, collapse = " + "))), data = cf)[, -1, drop = FALSE]
dm <- as.data.frame(mm)
names(dm) <- make.names(names(dm))
cov_names <- names(dm)

dat <- cbind(cc[c(all_act, net_items)], dm)
dat[c(all_act, net_items)] <- lapply(dat[c(all_act, net_items)], function(x) as.numeric(unclass(x)))

demo_to_net <- paste(sprintf("%s ~ %s", net_fac, paste(cov_names, collapse = " + ")), collapse = "\n")
demo_to_act <- paste(sprintf("%s ~ %s", c("A", "Lis", "R"), paste(c(net_fac, cov_names), collapse = " + ")), collapse = "\n")
syn <- paste(net_meas, act_meas, demo_to_net, demo_to_act, net_disturb, act_disturb, sep = "\n")

fit <- sem(syn, data = dat, ordered = c(all_act, net_items), estimator = "WLSMV",
           std.lv = TRUE, fixed.x = TRUE)

cat(sprintf("\nconverged=%s npar=%d\n", lavInspect(fit, "converged"), lavInspect(fit, "npar")))
cat(sprintf("CFI=%.3f TLI=%.3f RMSEA=%.3f [%.3f, %.3f] SRMR=%.3f\n",
  fitMeasures(fit, "cfi.scaled"), fitMeasures(fit, "tli.scaled"),
  fitMeasures(fit, "rmsea.scaled"), fitMeasures(fit, "rmsea.ci.lower.scaled"),
  fitMeasures(fit, "rmsea.ci.upper.scaled"), fitMeasures(fit, "srmr")))
cat("Latent covariance matrix positive definite:", all(eigen(lavInspect(fit, "cov.lv"))$values > 0), "\n")

pe <- parameterEstimates(fit, standardized = TRUE)
act_paths <- pe[pe$op == "~" & pe$lhs %in% c("A", "Lis", "R") & pe$rhs %in% net_fac,
                c("lhs", "rhs", "est", "se", "pvalue", "std.all")]
cat("\nNetwork-to-activity paths:\n")
print(transform(act_paths, est = round(est, 3), se = round(se, 3), pvalue = signif(pvalue, 3),
                std.all = round(std.all, 3)), row.names = FALSE)

b <- coef(fit)
V <- vcov(fit)
for (o in c("A", "Lis", "R")) {
  idx <- match(sprintf("%s~%s", o, net_fac), names(b))
  bb <- b[idx]
  stat <- as.numeric(t(bb) %*% solve(V[idx, idx]) %*% bb)
  cat(sprintf("Joint Wald for %-4s six-factor network block: chi2(6) = %.2f, p = %.4g\n",
              o, stat, pchisq(stat, df = 6, lower.tail = FALSE)))
}

nc <- pe[pe$op == "~~" & pe$lhs %in% net_fac & pe$rhs %in% net_fac & pe$lhs != pe$rhs,
         c("lhs", "rhs", "std.all")]
cat("\nNetwork factor residual correlations:\n")
print(transform(nc[!duplicated(t(apply(nc[, c("lhs", "rhs")], 1, sort))), ], std.all = round(std.all, 3)),
      row.names = FALSE)

demo <- pe[pe$op == "~" & pe$lhs %in% c(net_fac, "A", "Lis", "R") & pe$rhs %in% cov_names,
           c("lhs", "rhs", "est", "se", "pvalue")]
cat("\nDemographic paths on network factors:\n")
print(transform(demo[demo$lhs %in% net_fac, ], est = round(est, 3), se = round(se, 3),
                pvalue = signif(pvalue, 3)), row.names = FALSE)
cat("\nDemographic paths on activity factors:\n")
print(transform(demo[demo$lhs %in% c("A", "Lis", "R"), ], est = round(est, 3), se = round(se, 3),
                pvalue = signif(pvalue, 3)), row.names = FALSE)

saveRDS(list(fit = fit, syntax = syn, cov_names = cov_names, n = nrow(dat)),
        "cache/diagnostics/sem_full_six_factor.rds")
