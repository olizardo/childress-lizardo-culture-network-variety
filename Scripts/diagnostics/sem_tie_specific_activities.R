#' Latent SEM: tie-specific network factors predicting the three-factor activity CFA
#'
#' Network measurement: six tie-specific factors (weak and strong variety, liberal, conservative)
#' from the pooled oblique sorting (cfa_tie_pooled_oblimin_sorting.R), 32 no-gender items,
#' with the 16 matched weak-strong residual covariances.
#' Activity measurement: three-factor activities CFA (arts; leisure = walk, exercise, reading;
#' residual = gardening, home auto repair, fast food).
#' Structure: all six network factors and all three activity factors regressed on the
#' demographic covariates; activity factors also regressed on the six network factors.
#' Network factors are allowed to correlate freely (exogenous), as in the tie-specific CFA.
#' Estimation: WLSMV, std.lv, fixed.x = TRUE.
#'
#' Run from the project root: Rscript Scripts/diagnostics/sem_tie_specific_activities.R

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
all_act <- c(arts, leis, resid_act)

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
weak_no_g <- no_g[grepl("_weak$", no_g)]
pairs_w <- weak_no_g
pairs_s <- sub("_weak$", "_strong", weak_no_g)
resid_syntax <- paste(sprintf("%s ~~ %s", pairs_w, pairs_s), collapse = "\n")
line <- function(name, items) if (length(items) > 0) paste0(name, " =~ ", paste(items, collapse = " + "), "\n") else ""
fac_items <- function(tie, factor_name) {
  items <- no_g[grepl(paste0("_", tie, "$"), no_g)]
  items[label[items] == factor_name]
}
net_meas <- paste0(
  line("Wv", fac_items("weak", "variety")),
  line("Wl", fac_items("weak", "liberal")),
  line("Wc", fac_items("weak", "conservative")),
  line("Sv", fac_items("strong", "variety")),
  line("Sl", fac_items("strong", "liberal")),
  line("Sc", fac_items("strong", "conservative")))
net_fac <- c("Wv", "Wl", "Wc", "Sv", "Sl", "Sc")

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
  "R =~ ", paste(resid_act, collapse = " + "), "\n")
demo_to_net <- paste(sprintf("%s ~ %s", net_fac, paste(cov_names, collapse = " + ")), collapse = "\n")
demo_to_act <- paste(sprintf("%s ~ %s", c("A", "Lis", "R"), paste(c(net_fac, cov_names), collapse = " + ")), collapse = "\n")
# network factors become endogenous once regressed on demographics, so their disturbances
# (and the activity disturbances) are correlated explicitly, mirroring the CFA covariances
net_disturb <- paste(combn(net_fac, 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")
act_disturb <- paste(combn(c("A", "Lis", "R"), 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")
syn <- paste(net_meas, act_meas, resid_syntax, demo_to_net, demo_to_act, net_disturb, act_disturb, sep = "\n")

fit <- sem(syn, data = dat, ordered = c(all_act, no_g), estimator = "WLSMV",
           std.lv = TRUE, fixed.x = TRUE)
cat(sprintf("\nSEM: converged=%s PD=%s CFI=%.3f TLI=%.3f RMSEA=%.3f SRMR=%.3f\n",
  lavInspect(fit, "converged"), all(eigen(lavInspect(fit, "cov.lv"))$values > 0),
  fitMeasures(fit, "cfi.scaled"), fitMeasures(fit, "tli.scaled"),
  fitMeasures(fit, "rmsea.scaled"), fitMeasures(fit, "srmr")))

pe <- parameterEstimates(fit, standardized = TRUE)
ns <- function(x) signif(x, 3)
act_paths <- pe[pe$op == "~" & pe$lhs %in% c("A", "Lis", "R") & pe$rhs %in% net_fac,
                c("lhs", "rhs", "est", "se", "pvalue", "std.all")]
cat("\nNetwork-to-activity paths (tie-specific factors):\n")
print(transform(act_paths, est = round(est, 3), se = round(se, 3), pvalue = ns(pvalue), std.all = round(std.all, 3)), row.names = FALSE)

demo_paths <- pe[pe$op == "~" & (pe$lhs %in% net_fac | pe$lhs %in% c("A", "Lis", "R")) & pe$rhs %in% cov_names,
                 c("lhs", "rhs", "est", "se", "pvalue", "std.all")]
cat("\nDemographic paths on all factors (sig. at p < .05 flagged):\n")
demo_paths$sig <- ifelse(demo_paths$pvalue < .05, "*", "")
print(transform(demo_paths, est = round(est, 3), se = round(se, 3), pvalue = ns(pvalue), std.all = round(std.all, 3)), row.names = FALSE)

net_cor <- pe[pe$op == "~~" & pe$lhs %in% net_fac & pe$rhs %in% net_fac & pe$lhs != pe$rhs, c("lhs", "rhs", "std.all")]
net_cor <- net_cor[!duplicated(t(apply(net_cor[, c("lhs", "rhs")], 1, sort))), ]
cat("\nNetwork factor disturbance correlations:\n")
print(transform(net_cor, std.all = round(std.all, 3)), row.names = FALSE)

# joint Wald test of the six network paths for each activity outcome (collinearity makes
# individual paths unstable, so the block test is the stable summary)
wald_block <- function(outcome) {
  b_all <- coef(fit)
  idx <- match(sprintf("%s~%s", outcome, net_fac), names(b_all))
  stopifnot(!anyNA(idx))
  b <- b_all[idx]
  V <- vcov(fit)[idx, idx, drop = FALSE]
  stat <- as.numeric(t(b) %*% solve(V) %*% b)
  list(stat = stat, p.value = pchisq(stat, df = length(idx), lower.tail = FALSE))
}
cat("\nJoint Wald tests of the six network paths per activity outcome:\n")
for (o in c("A", "Lis", "R")) {
  w <- wald_block(o)
  cat(sprintf("%-4s chi2(6) = %.2f, p = %.4g\n", o, w$stat, w$p.value))
}
act_cor <- pe[pe$op == "~~" & pe$lhs %in% c("A", "Lis", "R") & pe$rhs %in% c("A", "Lis", "R") & pe$lhs < pe$rhs, c("lhs", "rhs", "std.all")]
cat("\nActivity-factor disturbance covariances:\n")
print(transform(act_cor, std.all = round(std.all, 3)), row.names = FALSE)
