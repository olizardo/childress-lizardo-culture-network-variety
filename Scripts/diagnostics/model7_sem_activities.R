# Latent-variable SEM: model 7 (no gender) network factors predicting the two activity CFA factors,
# compared with regression and Bartlett score-based estimates on identical covariates.
# Run from project root: Rscript Scripts/diagnostics/model7_sem_activities.R
suppressPackageStartupMessages({library(lavaan); library(estimatr)})
prep <- readRDS("cache/01_prepared_data.rds")
df <- prep$df
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
act <- s07$activities
arts <- names(act$arts); leis <- names(act$leisure); all17 <- c(arts, leis)
covars <- c("gender_f","educ_cat","child_arts","income","age2","race_f","poli")
w <- list(v = names(s07$weak$variety), l = names(s07$weak$liberal), c = names(s07$weak$conservative))
s <- list(v = names(s07$strong$variety), l = names(s07$strong$liberal), c = names(s07$strong$conservative))
items <- c(unlist(w), unlist(s))
items32 <- items[!grepl("^Know(men|women)_", items)]
liberal_items <- c(w$l, s$l)[!grepl("^Know(men|women)_", c(w$l, s$l))]
conservative_items <- c(w$c, s$c)[!grepl("^Know(men|women)_", c(w$c, s$c))]
weak_items <- items32[grepl("_weak$", items32)]
pairs <- data.frame(w = weak_items, s = sub("_weak$", "_strong", weak_items), stringsAsFactors = FALSE)
pairs <- pairs[pairs$s %in% items32, ]
meas <- paste0(
  "G =~ ", paste(items32, collapse = " + "), "\n",
  "L =~ ", paste(liberal_items, collapse = " + "), "\n",
  "C =~ ", paste(conservative_items, collapse = " + "), "\n",
  "G ~~ 0*L\nG ~~ 0*C\n",
  paste(sprintf("%s ~~ %s", pairs$w, pairs$s), collapse = "\n"), "\n")
need <- unique(c(all17, items32, covars, "educ", "child_arts"))
cc <- df[complete.cases(df[intersect(need, names(df))]), , drop = FALSE]
cat("analytic N =", nrow(cc), "\n")
# covariate dummies (numeric), same coding as lm_robust factor terms
cf <- cc[covars]
cf[] <- lapply(cf, function(x) if (is.numeric(x)) x else factor(x))
mm <- model.matrix(as.formula(paste("~", paste(covars, collapse = " + "))), data = cf)[, -1, drop = FALSE]
dm <- as.data.frame(mm); names(dm) <- make.names(names(dm))
dat <- cbind(cc[c(all17, items32)], dm)
dat[c(all17, items32)] <- lapply(dat[c(all17, items32)], function(x) as.numeric(unclass(x)))
cov_names <- names(dm)
struct <- paste(
  "A =~ ", paste(arts, collapse = " + "), "\n",
  "Lis =~ ", paste(leis, collapse = " + "), "\n",
  "A ~ G + L + C + ", paste(cov_names, collapse = " + "), "\n",
  "Lis ~ G + L + C + ", paste(cov_names, collapse = " + "), "\n")
sem_syn <- paste(meas, struct, sep = "\n")
sem_fit <- sem(sem_syn, data = dat, ordered = c(all17, items32), estimator = "WLSMV",
               std.lv = TRUE, fixed.x = TRUE)
cat(sprintf("SEM: converged=%s PD=%s CFI=%.3f RMSEA=%.3f SRMR=%.3f\n",
  lavInspect(sem_fit,"converged"), all(eigen(lavInspect(sem_fit,"cov.lv"))$values > 0),
  fitMeasures(sem_fit,"cfi.scaled"), fitMeasures(sem_fit,"rmsea.scaled"), fitMeasures(sem_fit,"srmr")))
pe <- parameterEstimates(sem_fit)
sem_b <- pe[pe$op == "~" & pe$lhs %in% c("A","Lis") & pe$rhs %in% c("G","L","C"), c("lhs","rhs","est","se","pvalue")]
cat("\nSEM structural coefficients (latent outcomes):\n"); print(transform(sem_b, est = round(est,3), se = round(se,3), pvalue = signif(pvalue,3)), row.names = FALSE)
# Regression and Bartlett comparisons on the same design
meas_fit <- cfa(meas, data = dat, ordered = items32, estimator = "WLSMV", std.lv = TRUE)
ns_reg <- as.data.frame(lavPredict(meas_fit, type = "lv", method = "regression"))
ns_bar <- as.data.frame(lavPredict(meas_fit, type = "lv", method = "Bartlett"))
names(ns_reg) <- names(ns_bar) <- c("G","L","C")
act_fit <- cfa(paste(sprintf("A =~ %s", paste(arts, collapse=" + ")), sprintf("Lis =~ %s", paste(leis, collapse=" + ")), sep="\n"),
               data = dat, ordered = all17, estimator = "WLSMV", std.lv = TRUE)
as_reg <- as.data.frame(lavPredict(act_fit, type = "lv", method = "regression"))
as_bar <- as.data.frame(lavPredict(act_fit, type = "lv", method = "Bartlett"))
names(as_reg) <- names(as_bar) <- c("A","Lis")
reg_coef <- function(y, Xdf) {
  d2 <- cbind(Xdf, dm)
  d2$y <- y
  ok <- complete.cases(d2)
  m <- lm_robust(as.formula(paste("y ~ G + L + C +", paste(cov_names, collapse = " + "))), data = d2[ok, ], se_type = "HC1")
  c(b = m$coefficients[c("G","L","C")], se = m$std.error[c("G","L","C")], p = m$p.value[c("G","L","C")], n = sum(ok))
}
res <- list()
for (y in c("A","Lis")) {
  for (src in c("regression","Bartlett")) {
    Xs <- if (src == "regression") ns_reg else ns_bar
    Ys <- if (src == "regression") as_reg[[y]] else as_bar[[y]]
    res[[paste(y, src)]] <- reg_coef(Ys, Xs)
  }
  sb <- sem_b[sem_b$lhs == y, ]
  res[[paste(y, "SEM")]] <- c(b = sb$est, se = sb$se, p = sb$pvalue, n = nrow(dat))
}
tab <- do.call(rbind, lapply(names(res), function(k) {
  r <- res[[k]]
  data.frame(model = k, G = sprintf("%.2f (%.2f)", r["b1"] %||% r[1], r[4]), stringsAsFactors = FALSE)
}))
cat("\nCoefficients (b, SE, p) under each scoring method:\n")
for (k in names(res)) {
  r <- res[[k]]
  cat(sprintf("%-16s G: %6.3f (%.3f) p=%.3g | L: %6.3f (%.3f) p=%.3g | C: %6.3f (%.3f) p=%.3g | n=%s\n", k,
    r[1], r[4], r[7], r[2], r[5], r[8], r[3], r[6], r[9], r[10]))
}
