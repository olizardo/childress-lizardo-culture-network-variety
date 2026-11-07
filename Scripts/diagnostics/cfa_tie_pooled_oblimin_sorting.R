#' Tie-structure CFAs under a single pooled oblique item sorting
#'
#' The liberal/conservative/variety assignment is taken from one 3-factor pooled EFA
#' (polychoric, oblimin, WLS) on all 36 network items, each item assigned to its highest
#' absolute loading (factor 1 variety, 2 liberal, 3 conservative, following the labelling in
#' model7_pooled_sorting.R). The same assignment is used for weak and strong items.
#' Gender items enter the EFA but are dropped from the CFAs (no-gender condition), along with
#' their residual pairs.
#'
#' Models (all WLSMV, std.lv, model 7 weak-strong residual covariances):
#'   (a) pooled: one variety, one liberal, one conservative factor across ties
#'   (b) tie-specific: six factors (weak and strong V, L, C), unconstrained
#'   (c) tie-specific with each weak-strong factor correlation fixed at 1 (nested in b)
#'
#' Run from the project root: Rscript Scripts/diagnostics/cfa_tie_pooled_oblimin_sorting.R

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

cat("Pooled oblimin sorting (all 36 items):\n")
print(data.frame(item = all_items, label = unname(label), round(L_pool, 2),
                 row.names = NULL), row.names = FALSE)

no_g <- all_items[!is_gender]
cat("\nNo-gender item counts by tie and factor:\n")
print(table(tie = ifelse(grepl("_weak$", no_g), "weak", "strong"), factor = label[no_g]))

dat <- dat_all[no_g]
weak_no_g <- no_g[grepl("_weak$", no_g)]
pairs_w <- weak_no_g
pairs_s <- sub("_weak$", "_strong", weak_no_g)
stopifnot(all(pairs_s %in% no_g))
resid_syntax <- paste(sprintf("%s ~~ %s", pairs_w, pairs_s), collapse = "\n")

fac_items <- function(tie, factor_name) {
  items <- no_g[grepl(paste0("_", tie, "$"), no_g)]
  items[label[items] == factor_name]
}
line <- function(name, items) if (length(items) > 0) paste0(name, " =~ ", paste(items, collapse = " + "), "\n") else ""

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
collapsed_syn <- paste0(tie_syn, "Wv ~~ 1*Sv\nWl ~~ 1*Sl\nWc ~~ 1*Sc\n")

fit_a <- cfa(paste0(pooled_syn, resid_syntax), data = dat, ordered = no_g, estimator = "WLSMV", std.lv = TRUE)
fit_b <- cfa(paste0(tie_syn, resid_syntax), data = dat, ordered = no_g, estimator = "WLSMV", std.lv = TRUE)
fit_c <- cfa(paste0(collapsed_syn, resid_syntax), data = dat, ordered = no_g, estimator = "WLSMV", std.lv = TRUE)

row <- function(nm, f) data.frame(model = nm, conv = lavInspect(f, "converged"),
  chisq_scaled = round(fitMeasures(f, "chisq.scaled"), 1), df = fitMeasures(f, "df"),
  CFI = round(fitMeasures(f, "cfi.scaled"), 3), RMSEA = round(fitMeasures(f, "rmsea.scaled"), 3),
  SRMR = round(fitMeasures(f, "srmr"), 3))
cat("\nN =", nrow(dat), " items =", length(no_g), " residual pairs =", length(pairs_w), "\n\n")
print(rbind(row("(a) pooled V, L, C + residuals", fit_a),
            row("(b) tie-specific 6-factor + residuals", fit_b),
            row("(c) tie-specific, weak-strong r = 1", fit_c)), row.names = FALSE)

cat("\nNested test (c) within (b):\n")
print(lavTestLRT(fit_c, fit_b))

pe_b <- parameterEstimates(fit_b, standardized = TRUE)
cat("\nWeak-strong latent correlations, (b):\n")
print(transform(pe_b[pe_b$op == "~~" & pe_b$lhs %in% c("Wv","Wl","Wc") & pe_b$rhs == sub("W", "S", pe_b$lhs), c("lhs","rhs","std.all")],
                std.all = round(std.all, 3)), row.names = FALSE)
cat("\nLatent covariance matrix of (b) positive definite:",
    all(eigen(lavInspect(fit_b, "cov.lv"))$values > 0), "\n")
