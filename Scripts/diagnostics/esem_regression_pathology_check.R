#' PATHOLOGY CHECK: regressions onto EFA-block factors in lavaan 0.7-3.
#'
#' Finding (November 7, 2026): when covariates regress onto EFA-block factors
#' (set-ESEM with fixed.x = TRUE), the estimated covariate paths can contradict
#' the item-level associations outright. Minimal reproduction on the weak-tie
#' battery with poli as the only covariate:
#'   - Observed polyserial corr(poli, KnowVeryCons_weak) = +.43 and
#'     corr(poli, KnowVeryLib_weak) = -.43.
#'   - The ESEM-with-regressions solution implies corr(poli, VeryLib item) = +.60
#'     and corr(poli, VeryCons item) = -.59: signs flipped, residuals up to 1.03.
#'   - The otherwise identical CFA model implies the correct signs
#'     (max residual .22).
#' Mechanism (diagnosis): the rotation criterion in lavaan is evaluated on the
#' model-implied latent covariance of the block, which for endogenous factors
#' depends on the regression coefficients; the optimizer trades rotation
#' simplicity against regression fit and lands in a distorted compromise. Under
#' Mplus conventions covariates also correlate freely with factors, which lavaan
#' does not do under fixed.x = TRUE.
#'
#' Consequence: the demographic-to-network paths in the set-ESEM structural models
#' (sem_set_esem_full.R) are NOT trustworthy. Demographic-to-activity paths in the
#' same models retained plausible signs, and the network-to-activity paths agreed
#' in direction and significance with the CFA-based baseline across specifications,
#' but any model with covariate paths onto EFA blocks should be treated with
#' suspicion in this lavaan version.
#'
#' Run from the project root: Rscript Scripts/diagnostics/esem_regression_pathology_check.R
#' Writes cache/diagnostics/esem_regression_pathology_check.rds and a log to stdout.

suppressPackageStartupMessages({
  library(lavaan)
  library(polycor)
})

prep <- readRDS("cache/01_prepared_data.rds")
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df
s <- s07$weak
items_list <- lapply(c("variety", "liberal", "conservative"), function(f) {
  it <- names(s[[f]]); it[!grepl("^Know(men|women)_", it)]
})
items <- unlist(items_list)
target <- matrix(0, nrow = length(items), ncol = 3, dimnames = list(items, c("Wv", "Wl", "Wc")))
for (k in 1:3) target[items_list[[k]], k] <- 1
dat <- df[complete.cases(df[c(items, "poli")]), c(items, "poli")]
dat[items] <- lapply(dat[items], function(x) as.numeric(unclass(x)))

esem_syn <- sprintf("efa(\"t\")*Wv + efa(\"t\")*Wl + efa(\"t\")*Wc =~ %s",
  paste(items, collapse = " + "))
reg <- "Wv ~ poli\nWl ~ poli\nWc ~ poli"
cfa_syn <- paste(sprintf("%s =~ %s", c("Wv", "Wl", "Wc"),
  vapply(items_list, paste, character(1), collapse = " + ")), collapse = "\n")

fe <- sem(paste(esem_syn, reg, sep = "\n"), data = dat, ordered = items,
  estimator = "WLSMV", std.lv = TRUE, fixed.x = TRUE,
  rotation = list("target", target = target))
fc <- sem(paste(cfa_syn, reg, sep = "\n"), data = dat, ordered = items,
  estimator = "WLSMV", std.lv = TRUE, fixed.x = TRUE)

obs_ps <- sapply(items, function(it) polyserial(dat[[it]], dat$poli))
cat("Observed polyserial corr(poli, item):\n")
print(round(obs_ps, 2))

implied_corr <- function(fit, lbl) {
  pe <- parameterEstimates(fit, standardized = TRUE)
  beta <- pe[pe$op == "~" & pe$rhs == "poli", c("lhs", "std.all")]
  lam <- inspect(fit, "std")$lambda
  out <- sapply(items, function(it)
    sum(beta$std.all[match(colnames(lam), beta$lhs)] * lam[it, ], na.rm = TRUE))
  cat(sprintf("\n--- implied corr(poli, item), %s-with-regressions ---\n", lbl))
  print(round(out, 2))
  cat(sprintf("max |implied - observed| = %.3f\n", max(abs(out - obs_ps))))
  out
}
i_e <- implied_corr(fe, "ESEM")
i_c <- implied_corr(fc, "CFA")

cat("\nESEM regression estimates (est, std.all, p):\n")
pe <- parameterEstimates(fe, standardized = TRUE)
print(pe[pe$op == "~" & pe$rhs == "poli", c("lhs", "est", "std.all", "pvalue")])
cat("\nCFA regression estimates (est, std.all, p):\n")
pe <- parameterEstimates(fc, standardized = TRUE)
print(pe[pe$op == "~" & pe$rhs == "poli", c("lhs", "est", "std.all", "pvalue")])

saveRDS(list(observed_polyserial = obs_ps, implied_esem = i_e, implied_cfa = i_c,
  esem_fit = fe, cfa_fit = fc),
  "cache/diagnostics/esem_regression_pathology_check.rds")
cat("\nDone.\n")
