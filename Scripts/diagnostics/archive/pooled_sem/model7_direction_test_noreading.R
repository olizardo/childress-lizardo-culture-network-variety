# Latent-variable SEM: model 7 (no gender) network factors predicting the two activity CFA factors,
# compared with regression and Bartlett score-based estimates on identical covariates.
# Run from project root: Rscript Scripts/diagnostics/model7_sem_activities.R
suppressPackageStartupMessages({library(lavaan); library(estimatr)})
prep <- readRDS("cache/01_prepared_data.rds")
df <- prep$df
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
act <- s07$activities
arts <- names(act$arts); leis <- setdiff(names(act$leisure), "read_novel_poem_or_play"); all17 <- c(arts, leis)
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


act_meas <- paste0("A =~ ", paste(arts, collapse = " + "), "\nLis =~ ", paste(leis, collapse = " + "), "\n")
cov_str <- paste(cov_names, collapse = " + ")
net_on_act <- paste0(act_meas,
  "A ~ G + L + C + ", cov_str, "\n",
  "Lis ~ G + L + C + ", cov_str, "\n",
  "G ~ ", cov_str, "\nL ~ ", cov_str, "\nC ~ ", cov_str, "\n")
act_on_net <- paste0(act_meas,
  "G ~ A + Lis + ", cov_str, "\nL ~ A + Lis + ", cov_str, "\nC ~ A + Lis + ", cov_str, "\n",
  "A ~ ", cov_str, "\nLis ~ ", cov_str, "\n")
fit_one <- function(struct, label) {
  syn <- paste(meas, struct, sep = "\n")
  f <- sem(syn, data = dat, ordered = c(all17, items32), estimator = "WLSMV", std.lv = FALSE, fixed.x = TRUE)
  fm <- fitMeasures(f, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.scaled","rmsea.scaled","srmr"))
  data.frame(model = label, converged = lavInspect(f, "converged"), chisq = round(fm[1],1), df = fm[2],
    p = signif(fm[3],3), CFI = round(fm[4],3), RMSEA = round(fm[5],3), SRMR = round(fm[6],3), free_params = max(lavInspect(f,"npar")))
}
res <- rbind(fit_one(net_on_act, "networks -> activities"), fit_one(act_on_net, "activities -> networks"))
print(res, row.names = FALSE)
cat("\nChi-square difference (same df; non-nested, descriptive only):", round(res$chisq[1] - res$chisq[2], 1), "\n")
