# Latent SEM: three-factor activities CFA (arts, leisure, residual DIY) outcomes regressed on
# the no-gender bifactor network model (G, L, C with weak-strong residual covariances), with
# demographics predicting both network and activity factors.
# Run from project root: Rscript Scripts/diagnostics/sem_three_factor_activities.R
suppressPackageStartupMessages(library(lavaan))
prep <- readRDS("cache/01_prepared_data.rds")
df <- prep$df
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
act <- s07$activities
arts <- names(act$arts)
leis <- names(act$leisure)
resid <- names(act$residual)
all_act <- c(arts, leis, resid)
covars <- c("gender_f", "educ_cat", "child_arts", "income", "age2", "race_f", "poli")
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
need <- unique(c(all_act, items32, covars, "educ", "child_arts"))
cc <- df[complete.cases(df[intersect(need, names(df))]), , drop = FALSE]
cat("analytic N =", nrow(cc), "\n")
cf <- cc[covars]
cf[] <- lapply(cf, function(x) if (is.numeric(x)) x else factor(x))
mm <- model.matrix(as.formula(paste("~", paste(covars, collapse = " + "))), data = cf)[, -1, drop = FALSE]
dm <- as.data.frame(mm)
names(dm) <- make.names(names(dm))
dat <- cbind(cc[c(all_act, items32)], dm)
dat[c(all_act, items32)] <- lapply(dat[c(all_act, items32)], function(x) as.numeric(unclass(x)))
cov_names <- names(dm)
struct <- paste(
  "A =~ ", paste(arts, collapse = " + "), "\n",
  "Lis =~ ", paste(leis, collapse = " + "), "\n",
  "R =~ ", paste(resid, collapse = " + "), "\n",
  "G ~ ", paste(cov_names, collapse = " + "), "\n",
  "L ~ ", paste(cov_names, collapse = " + "), "\n",
  "C ~ ", paste(cov_names, collapse = " + "), "\n",
  "A ~ G + L + C + ", paste(cov_names, collapse = " + "), "\n",
  "Lis ~ G + L + C + ", paste(cov_names, collapse = " + "), "\n",
  "R ~ G + L + C + ", paste(cov_names, collapse = " + "), "\n")
sem_syn <- paste(meas, struct, sep = "\n")
sem_fit <- sem(sem_syn, data = dat, ordered = c(all_act, items32), estimator = "WLSMV",
               std.lv = TRUE, fixed.x = TRUE)
cat(sprintf("SEM: converged=%s PD=%s CFI=%.3f TLI=%.3f RMSEA=%.3f SRMR=%.3f\n",
  lavInspect(sem_fit, "converged"), all(eigen(lavInspect(sem_fit, "cov.lv"))$values > 0),
  fitMeasures(sem_fit, "cfi.scaled"), fitMeasures(sem_fit, "tli.scaled"),
  fitMeasures(sem_fit, "rmsea.scaled"), fitMeasures(sem_fit, "srmr")))
pe <- parameterEstimates(sem_fit, standardized = TRUE)
keep_struct <- pe$op == "~" & pe$lhs %in% c("A", "Lis", "R", "G", "L", "C")
sb <- pe[keep_struct & pe$rhs %in% c("G", "L", "C"), c("lhs", "rhs", "est", "se", "pvalue", "std.all")]
cat("\nNetwork-to-activity paths (latent outcomes):\n")
print(transform(sb, est = round(est, 3), se = round(se, 3), pvalue = signif(pvalue, 3), std.all = round(std.all, 3)), row.names = FALSE)
dem <- pe[keep_struct & pe$rhs %in% cov_names & pe$lhs %in% c("A", "Lis", "R", "G", "L", "C"), c("lhs", "rhs", "est", "se", "pvalue")]
cat("\nDemographic paths (all factors):\n")
print(transform(dem, est = round(est, 3), se = round(se, 3), pvalue = signif(pvalue, 3)), row.names = FALSE)
lat_cor <- pe[pe$op == "~~" & pe$lhs %in% c("A", "Lis", "R") & pe$rhs %in% c("A", "Lis", "R") & pe$lhs != pe$rhs, c("lhs", "rhs", "est", "std.all")]
cat("\nResidual-factor covariances among activity factors (residual disturbances):\n")
print(transform(lat_cor, est = round(est, 3), std.all = round(std.all, 3)), row.names = FALSE)
