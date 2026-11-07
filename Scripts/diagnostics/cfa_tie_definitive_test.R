#' Definitive test of tie-specific vs. pooled network structures, with battery method factors
#'
#' Item sorting: the pooled oblique (oblimin, polychoric WLS) 3-factor EFA from
#' cfa_tie_pooled_oblimin_sorting.R, fixed across all samples (N = 1,258 full sample).
#' Residuals: all 16 matched weak-strong item pairs (same question), a priori, not selected
#' from misfit. Gender items dropped from the CFAs (32 items).
#'
#' Four pre-specified models, all WLSMV with std.lv:
#'   M1 pooled V, L, C
#'   M2 tie-specific six factors (weak and strong V, L, C)
#'   M3 pooled V, L, C plus two battery method factors (all weak items; all strong items),
#'      method factors orthogonal to substantive factors
#'   M4 tie-specific six factors plus the same two battery method factors
#' Key nested tests, each by a scaled chi-square difference (lavTestLRT):
#'   M2 vs M1  tie distinction, no method factors
#'   M4 vs M3  tie distinction, with method factors (the definitive test)
#'   M3 vs M1  battery method, pooled factors
#'   M4 vs M2  battery method, tie-specific factors
#' Each model is fit on the full sample and on two random halves (seed 4217) as a replication.
#'
#' Run from the project root: Rscript Scripts/diagnostics/cfa_tie_definitive_test.R

suppressPackageStartupMessages({
  library(lavaan)
  library(psych)
})

prep <- readRDS("cache/01_prepared_data.rds")
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df

weak_v <- names(s07$weak$variety)
weak_l <- names(s07$weak$liberal)
weak_c <- names(s07$weak$conservative)
strong_v <- names(s07$strong$variety)
strong_l <- names(s07$strong$liberal)
strong_c <- names(s07$strong$conservative)
all_items <- c(weak_v, weak_l, weak_c, strong_v, strong_l, strong_c)
is_gender <- grepl("^Know(men|women)_", all_items)

dat_all <- df[complete.cases(df[all_items]), all_items, drop = FALSE]
dat_all[] <- lapply(dat_all, function(x) as.numeric(unclass(x)))

pooled_efa <- fa(polychoric(dat_all)$rho, nfactors = 3, n.obs = nrow(dat_all),
                 rotate = "oblimin", fm = "wls")
L_pool <- unclass(pooled_efa$loadings)
label <- c("variety", "liberal", "conservative")[apply(abs(L_pool), 1, which.max)]
names(label) <- all_items

no_g <- all_items[!is_gender]
weak_no_g <- no_g[grepl("_weak$", no_g)]
strong_no_g <- no_g[grepl("_strong$", no_g)]
pairs_w <- weak_no_g
pairs_s <- sub("_weak$", "_strong", weak_no_g)
stopifnot(all(pairs_s %in% strong_no_g))
resid_syntax <- paste(sprintf("%s ~~ %s", pairs_w, pairs_s), collapse = "\n")

line <- function(name, items) if (length(items) > 0) paste0(name, " =~ ", paste(items, collapse = " + "), "\n") else ""
fac_items <- function(tie, factor_name) {
  items <- no_g[grepl(paste0("_", tie, "$"), no_g)]
  items[label[items] == factor_name]
}
pooled_syn <- paste0(
  line("V", no_g[label[no_g] == "variety"]),
  line("L", no_g[label[no_g] == "liberal"]),
  line("C", no_g[label[no_g] == "conservative"]))
tie_syn <- paste0(
  line("Wv", fac_items("weak", "variety")),
  line("Wl", fac_items("weak", "liberal")),
  line("Wc", fac_items("weak", "conservative")),
  line("Sv", fac_items("strong", "variety")),
  line("Sl", fac_items("strong", "liberal")),
  line("Sc", fac_items("strong", "conservative")))
method_syn <- paste0(
  "Mw =~ ", paste(weak_no_g, collapse = " + "), "\n",
  "Ms =~ ", paste(strong_no_g, collapse = " + "), "\n",
  "Mw ~~ Ms\n")
pooled_orth <- "Mw ~~ 0*V\nMw ~~ 0*L\nMw ~~ 0*C\nMs ~~ 0*V\nMs ~~ 0*L\nMs ~~ 0*C\n"
tie_orth <- "Mw ~~ 0*Wv\nMw ~~ 0*Wl\nMw ~~ 0*Wc\nMw ~~ 0*Sv\nMw ~~ 0*Sl\nMw ~~ 0*Sc\nMs ~~ 0*Wv\nMs ~~ 0*Wl\nMs ~~ 0*Wc\nMs ~~ 0*Sv\nMs ~~ 0*Sl\nMs ~~ 0*Sc\n"

models <- list(
  M1 = paste0(pooled_syn, resid_syntax),
  M2 = paste0(tie_syn, resid_syntax),
  M3 = paste0(pooled_syn, method_syn, pooled_orth, resid_syntax),
  M4 = paste0(tie_syn, method_syn, tie_orth, resid_syntax))

fit_set <- function(dat) {
  lapply(models, function(syn) cfa(syn, data = dat, ordered = no_g, estimator = "WLSMV", std.lv = TRUE))
}
fit_row <- function(nm, f) data.frame(model = nm, conv = lavInspect(f, "converged"),
  pd = all(eigen(lavInspect(f, "cov.lv"))$values > 0),
  chisq_scaled = round(fitMeasures(f, "chisq.scaled"), 1), df = fitMeasures(f, "df"),
  CFI = round(fitMeasures(f, "cfi.scaled"), 3), RMSEA = round(fitMeasures(f, "rmsea.scaled"), 3),
  SRMR = round(fitMeasures(f, "srmr"), 3))
nest_row <- function(big, small, fits) {
  tst <- lavTestLRT(fits[[big]], fits[[small]])
  data.frame(comparison = paste(big, "vs", small), df_diff = tst[2, "Df diff"],
             scaled_chisq_diff = round(tst[2, "Chisq diff"], 1), p = signif(tst[2, "Pr(>Chisq)"], 3))
}
run_sample <- function(label_txt, dat) {
  cat("\n=====", label_txt, " N =", nrow(dat), "=====\n")
  fits <- fit_set(dat)
  print(do.call(rbind, Map(fit_row, names(fits), fits)), row.names = FALSE)
  nests <- rbind(nest_row("M2", "M1", fits), nest_row("M4", "M3", fits),
                 nest_row("M3", "M1", fits), nest_row("M4", "M2", fits))
  cat("\nNested tests:\n")
  print(nests, row.names = FALSE)
  pe <- parameterEstimates(fits$M4, standardized = TRUE)
  cr <- pe[pe$op == "~~" & pe$lhs %in% c("Wv","Wl","Wc") & pe$rhs == sub("W", "S", pe$lhs), c("lhs","rhs","std.all")]
  cat("\nWeak-strong latent correlations, M4:\n")
  print(transform(cr, std.all = round(std.all, 3)), row.names = FALSE)
  mload <- pe[pe$op == "=~" & pe$lhs %in% c("Mw","Ms"), "std.all"]
  cat("Method-factor standardized loadings range, M4:", round(range(mload), 2), "\n")
  invisible(fits)
}

dat_full <- dat_all[no_g]
run_sample("full sample", dat_full)

set.seed(4217)
idx_a <- sample(seq_len(nrow(dat_full)), size = floor(nrow(dat_full) / 2))
run_sample("random half A", dat_full[idx_a, , drop = FALSE])
run_sample("random half B", dat_full[-idx_a, , drop = FALSE])
