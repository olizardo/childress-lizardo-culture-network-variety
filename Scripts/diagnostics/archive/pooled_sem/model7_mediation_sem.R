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
if (Sys.getenv("FREED") == "1") meas <- paste0(meas, "L =~ KnowWhiteppl_weak\nC =~ KnowLittleChurch_strong\nL =~ KnowVeryCons_strong\n")
need <- unique(c(all17, items32, covars, "educ", "child_arts"))
cc <- df[complete.cases(df[intersect(need, names(df))]), , drop = FALSE]
cat("analytic N =", nrow(cc), " freed =", Sys.getenv("FREED"), "\n")
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

# ---- labeled marker-scaled SEM with demographics -> network factors ----
targets <- c(grep("^educ_cat", cov_names, value = TRUE), grep("^race_f", cov_names, value = TRUE), "poli")
lab <- setNames(paste0("c", seq_along(cov_names)), cov_names)
net_lines <- sapply(c("G","L","C"), function(M)
  paste0(M, " ~ ", paste(sprintf("a%s%s*%s", M, lab[cov_names], cov_names), collapse = " + ")))
out_lines <- c(
  paste0("A ~ bA_G*G + bA_L*L + bA_C*C + ", paste(sprintf("dA%s*%s", lab[cov_names], cov_names), collapse = " + ")),
  paste0("Lis ~ bLis_G*G + bLis_L*L + bLis_C*C + ", paste(sprintf("dLis%s*%s", lab[cov_names], cov_names), collapse = " + ")))
defs <- character()
for (tg in targets) {
  k <- lab[tg]
  for (o in c("A","Lis")) {
    for (M in c("G","L","C")) defs <- c(defs, sprintf("ind%s%s%s := a%s%s * b%s_%s", o, k, M, M, k, o, M))
    defs <- c(defs, sprintf("dir%s%s := d%s%s", o, k, o, k))
    defs <- c(defs, sprintf("tot%s%s := d%s%s + %s", o, k, o, k,
      paste(sprintf("ind%s%s%s", o, k, c("G","L","C")), collapse = " + ")))
  }
}
act_meas <- paste0("A =~ ", paste(arts, collapse = " + "), "\nLis =~ ", paste(leis, collapse = " + "), "\n")
sem_lab <- paste(meas, act_meas, paste(net_lines, collapse = "\n"), paste(out_lines, collapse = "\n"),
                 paste(defs, collapse = "\n"), sep = "\n")
# marker scaling: std.lv = FALSE, first loading of each latent fixed to 1
fit_lab <- sem(sem_lab, data = dat, ordered = c(all17, items32), estimator = "WLSMV",
               std.lv = FALSE, fixed.x = TRUE)
cat(sprintf("Marker-scaled SEM: converged=%s CFI=%.3f RMSEA=%.3f\n", lavInspect(fit_lab,"converged"),
    fitMeasures(fit_lab,"cfi.scaled"), fitMeasures(fit_lab,"rmsea.scaled")))
pl <- parameterEstimates(fit_lab)
fmt <- function(x) data.frame(est = round(x$est, 3), se = round(x$se, 3), p = signif(x$pvalue, 3))
tot <- pl[pl$op == ":=" & grepl("^tot", pl$lhs), c("lhs","est","se","pvalue")]
ind <- pl[pl$op == ":=" & grepl("^ind", pl$lhs), c("lhs","est","se","pvalue")]
dir <- pl[pl$op == "~" & pl$lhs %in% c("A","Lis") & grepl("^", pl$rhs) & pl$label %in% sprintf("d%s%s", c("A","Lis"), rep(lab[targets], each = 2)), c("lhs","label","est","se","pvalue")]
lab_inv <- setNames(names(lab), lab)
desc <- function(code) {
  k <- sub("^(tot|ind|dA|dLis)(A|Lis)?", "", code)
  sprintf("%s", lab_inv[sub("^(c[0-9]+).*", "\\1", k)])
}
cat("\nTotal effects (direct + indirect):\n")
tot$predictor <- sapply(tot$lhs, function(x) lab_inv[sub("^tot(A|Lis)(c[0-9]+)$", "\\2", x)])
tot$outcome <- sub("^tot(A|Lis)c[0-9]+$", "\\1", tot$lhs)
print(transform(tot[, c("outcome","predictor","est","se","pvalue")], est = round(est,3), se = round(se,3), pvalue = signif(pvalue,3)), row.names = FALSE)
dr <- pl[pl$op == ":=" & grepl("^dir", pl$lhs), c("lhs","est","se","pvalue")]
dr$outcome <- sub("^dir(A|Lis)c[0-9]+$", "\\1", dr$lhs)
dr$predictor <- sapply(dr$lhs, function(x) lab_inv[sub("^dir(A|Lis)(c[0-9]+)$", "\\2", x)])
cat("\nDirect effects:\n")
print(transform(dr[, c("outcome","predictor","est","se","pvalue")], est = round(est,3), se = round(se,3), pvalue = signif(pvalue,3)), row.names = FALSE)
cat("\nIndirect effects through each network factor:\n")
ind$outcome <- sub("^ind(A|Lis)c[0-9]+[GLC]$", "\\1", ind$lhs)
ind$predictor <- sapply(ind$lhs, function(x) lab_inv[sub("^ind(A|Lis)(c[0-9]+)[GLC]$", "\\2", x)])
ind$through <- sub("^ind(A|Lis)c[0-9]+", "", ind$lhs)
print(transform(ind[, c("outcome","predictor","through","est","se","pvalue")], est = round(est,3), se = round(se,3), pvalue = signif(pvalue,3)), row.names = FALSE)
