#' Test whether fixed.x = FALSE repairs the regression pathology onto EFA blocks.
#'
#' Variants, all on the minimal weak-tie model (16 items, 3 EFA factors, poli only):
#'   (1) fixed.x = FALSE, regressions only (no explicit covariate-factor covariances);
#'   (2) fixed.x = FALSE, regressions PLUS explicit poli ~~ factor covariances
#'       (closer to the Mplus convention of freely correlated covariates);
#'   (3) baseline: fixed.x = TRUE (known pathological).
#' For each, implied item-poli correlations (sum of std.all beta * std.all lambda
#' across factors) are compared with the observed polyserial correlations.
#'
#' Run from the project root: Rscript Scripts/diagnostics/esem_fixedx_false_test.R
#' Writes cache/diagnostics/esem_fixedx_false_test.rds and a log to stdout.

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
covs <- "Wv ~~ poli\nWl ~~ poli\nWc ~~ poli"

obs_ps <- sapply(items, function(it) polyserial(dat[[it]], dat$poli))
cat("Observed polyserial corr(poli, item):\n")
print(round(obs_ps, 2))

variants <- list(
  fxTRUE            = list(syn = paste(esem_syn, reg, sep = "\n"), fx = TRUE),
  fxFALSE_regress   = list(syn = paste(esem_syn, reg, sep = "\n"), fx = FALSE),
  fxFALSE_regr_covs = list(syn = paste(esem_syn, reg, covs, sep = "\n"), fx = FALSE)
)

out <- list()
for (nm in names(variants)) {
  v <- variants[[nm]]
  cat(sprintf("\n===== variant: %s =====\n", nm))
  fit <- tryCatch(sem(v$syn, data = dat, ordered = items, estimator = "WLSMV",
    std.lv = TRUE, fixed.x = v$fx, rotation = list("target", target = target)),
    error = function(e) e)
  if (inherits(fit, "error")) {
    cat(sprintf("FAILED: %s\n", conditionMessage(fit)))
    out[[nm]] <- list(error = conditionMessage(fit))
    next
  }
  pe <- parameterEstimates(fit, standardized = TRUE)
  beta <- pe[pe$op == "~" & pe$rhs == "poli", c("lhs", "est", "std.all", "pvalue")]
  cat("Regression estimates:\n")
  print(beta)
  lam <- inspect(fit, "std")$lambda
  imp <- sapply(items, function(it)
    sum(beta$std.all[match(colnames(lam), beta$lhs)] * lam[it, ], na.rm = TRUE))
  cat("Implied vs observed corr(poli, item):\n")
  tbl <- round(cbind(implied = imp, observed = obs_ps, resid = obs_ps - imp), 3)
  print(tbl[order(-abs(tbl[, "resid"])), ][1:6, ])
  cat(sprintf("max |implied - observed| = %.3f\n", max(abs(imp - obs_ps))))
  fm <- fitMeasures(fit, c("chisq.scaled", "df", "cfi.scaled", "rmsea.scaled", "srmr"))
  cat(sprintf("fit: chi2=%s df=%s CFI=%s RMSEA=%s SRMR=%.3f\n",
    ifelse(is.na(fm[1]), "NA", sprintf("%.1f", fm[1])),
    ifelse(is.na(fm[2]), "NA", fm[2]),
    ifelse(is.na(fm[3]), "NA", sprintf("%.3f", fm[3])),
    ifelse(is.na(fm[4]), "NA", sprintf("%.3f", fm[4])), fm[5]))
  out[[nm]] <- list(fit = fit, beta = beta, implied = imp)
}

saveRDS(list(out = out, observed_polyserial = obs_ps),
  "cache/diagnostics/esem_fixedx_false_test.rds")
cat("\nDone.\n")
