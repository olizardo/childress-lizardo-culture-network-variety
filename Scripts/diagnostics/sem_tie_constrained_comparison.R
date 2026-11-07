#' Constrained versions of the tie-specific SEM for interpretable network effects
#'
#' The unconstrained tie-specific SEM (sem_tie_specific_activities.R) estimates six network
#' paths per activity outcome, which are collinear. Nested constraints reduce the number of
#' structural paths and each one is a substantive hypothesis:
#'   free      six separate paths per outcome (Wv, Wl, Wc, Sv, Sl, Sc)
#'   ideology  weak and strong effects equal within ideology (v, l, c): 3 paths
#'   tie       effects equal across ideology within tie type (variety, liberal and conservative
#'             share one path per tie): 2 paths
#'   pooled    one common network path per outcome: 1 path
#'   variety   only the two variety paths (Wv, Sv), composition paths removed: 2 paths
#' Each constrained model is compared with the free model (and the pooled model with the
#' ideology and tie models) by scaled chi-square difference tests (lavTestLRT, WLSMV).
#' Measurement model, sorting, residuals, sample, and covariates match sem_tie_specific_activities.R.
#'
#' Run from the project root: Rscript Scripts/diagnostics/sem_tie_constrained_comparison.R

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
net_disturb <- paste(combn(net_fac, 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")
act_disturb <- paste(combn(c("A", "Lis", "R"), 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")
outcomes <- c("A", "Lis", "R")

# label scheme: named vector net factor -> label suffix (NULL drops the path)
schemes <- list(
  free     = setNames(net_fac, net_fac),
  ideology = c(Wv = "v", Sv = "v", Wl = "l", Sl = "l", Wc = "c", Sc = "c"),
  tie      = c(Wv = "w", Wl = "w", Wc = "w", Sv = "s", Sl = "s", Sc = "s"),
  pooled   = setNames(rep("p", 6), net_fac),
  variety  = c(Wv = "v", Sv = "v")
)

scheme_syntax <- function(scheme) {
  struct <- vapply(outcomes, function(o) {
    labs <- schemes[[scheme]]
    terms <- paste0(paste0(o, "_", labs[names(labs)], "*", names(labs)), collapse = " + ")
    sprintf("%s ~ %s + %s", o, terms, paste(cov_names, collapse = " + "))
  }, character(1))
  paste(
    net_meas, act_meas, resid_syntax, demo_to_net,
    paste(struct, collapse = "\n"),
    net_disturb, act_disturb, sep = "\n")
}

fits <- lapply(names(schemes), function(s) {
  cat("fitting", s, "\n")
  sem(scheme_syntax(s), data = dat, ordered = c(all_act, no_g), estimator = "WLSMV",
      std.lv = TRUE, fixed.x = TRUE)
})
names(fits) <- names(schemes)
cat("\nFree parameters by scheme:\n")
print(sapply(fits, lavInspect, "npar"))

fit_row <- function(nm, f) data.frame(scheme = nm, conv = lavInspect(f, "converged"),
  pd = all(eigen(lavInspect(f, "cov.lv"))$values > 0),
  chisq_scaled = round(fitMeasures(f, "chisq.scaled"), 1), df = fitMeasures(f, "df"),
  CFI = round(fitMeasures(f, "cfi.scaled"), 3), RMSEA = round(fitMeasures(f, "rmsea.scaled"), 3),
  SRMR = round(fitMeasures(f, "srmr"), 3))
cat("\nFit by scheme:\n")
print(do.call(rbind, Map(fit_row, names(fits), fits)), row.names = FALSE)

nest_row <- function(big, small) {
  tst <- lavTestLRT(fits[[big]], fits[[small]])
  data.frame(comparison = paste(small, "within", big), df_diff = tst[2, "Df diff"],
             scaled_chisq_diff = round(tst[2, "Chisq diff"], 1), p = signif(tst[2, "Pr(>Chisq)"], 3))
}
cat("\nNested tests (constrained model vs. the model it nests in):\n")
print(rbind(nest_row("free", "ideology"), nest_row("free", "tie"), nest_row("free", "pooled"),
            nest_row("ideology", "pooled"), nest_row("tie", "pooled"), nest_row("free", "variety")),
      row.names = FALSE)

structural_rows <- function(f) {
  pe <- parameterEstimates(f, standardized = TRUE)
  pe[pe$op == "~" & pe$lhs %in% outcomes & pe$rhs %in% net_fac | pe$op == "~" & pe$lhs %in% outcomes & grepl("^(A|Lis|R)_", pe$label) & pe$rhs %in% net_fac,
     c("lhs", "rhs", "label", "est", "se", "pvalue", "std.all")]
}
for (s in c("ideology", "tie", "pooled", "variety")) {
  cat("\nStructural paths under", s, "constraint:\n")
  sr <- structural_rows(fits[[s]])
  print(transform(sr, est = round(est, 3), se = round(se, 3), pvalue = signif(pvalue, 3),
                  std.all = round(std.all, 3)), row.names = FALSE)
}
