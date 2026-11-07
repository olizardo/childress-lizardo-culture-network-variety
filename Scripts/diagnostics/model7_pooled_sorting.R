#' Refit bifactor model 7 under two liberal/conservative item sortings
#'
#' Sortings compared:
#'   script07: the per-battery EFA assignment stored in cache/07_reliability_cfa.rds
#'   pooled:   a 3-factor pooled EFA (polychoric, oblimin, WLS) on all 36 items,
#'             each item assigned to whichever of factors 2 (liberal) or
#'             3 (conservative) it loads on highest; factor 1 is variety.
#'
#' Model 7: general factor G on all items; L and C on the liberal and conservative
#' items; G orthogonal to L and C; L ~~ C free; one residual covariance per
#' weak-strong item pair. Gender items are dropped in the no-gender condition,
#' along with their residual pairs.
#'
#' Run from the project root: Rscript Scripts/diagnostics/model7_pooled_sorting.R

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

dat_all <- df[complete.cases(df[all_items]), all_items, drop = FALSE]
dat_all[] <- lapply(dat_all, function(x) as.numeric(unclass(x)))

pooled_efa <- fa(polychoric(dat_all)$rho, nfactors = 3, n.obs = nrow(dat_all),
                 rotate = "oblimin", fm = "wls")
L_pool <- unclass(pooled_efa$loadings)
pooled_label <- c("variety", "liberal", "conservative")[apply(abs(L_pool), 1, which.max)]
names(pooled_label) <- all_items

script07_label <- setNames(
  c(rep("variety", length(weak_v)), rep("liberal", length(weak_l)), rep("conservative", length(weak_c)),
    rep("variety", length(strong_v)), rep("liberal", length(strong_l)), rep("conservative", length(strong_c))),
  all_items)

is_gender <- grepl("^Know(men|women)_", all_items)

build_model7 <- function(label, drop_gender) {
  items <- if (drop_gender) all_items[!is_gender] else all_items
  lab <- label[items]
  liberal_items <- items[lab == "liberal"]
  conservative_items <- items[lab == "conservative"]
  weak_items <- items[grepl("_weak$", items)]
  pairs <- data.frame(w = weak_items, s = sub("_weak$", "_strong", weak_items), stringsAsFactors = FALSE)
  pairs <- pairs[pairs$s %in% items, , drop = FALSE]
  syntax <- paste0(
    "G =~ ", paste(items, collapse = " + "), "\n",
    "L =~ ", paste(liberal_items, collapse = " + "), "\n",
    "C =~ ", paste(conservative_items, collapse = " + "), "\n",
    "G ~~ 0*L\nG ~~ 0*C\n",
    paste(sprintf("%s ~~ %s", pairs$w, pairs$s), collapse = "\n"), "\n")
  list(syntax = syntax, items = items, n_liberal = length(liberal_items),
       n_conservative = length(conservative_items), n_pairs = nrow(pairs))
}

fit_one <- function(label_name, label, drop_gender) {
  spec <- build_model7(label, drop_gender)
  dat <- dat_all[spec$items]
  fit <- cfa(spec$syntax, data = dat, ordered = spec$items, estimator = "WLSMV", std.lv = TRUE)
  pe <- parameterEstimates(fit, standardized = TRUE)
  lc <- pe[pe$op == "~~" & pe$lhs == "L" & pe$rhs == "C", ]
  data.frame(
    sorting = label_name,
    condition = if (drop_gender) "no gender" else "all items",
    n_items = length(spec$items),
    n_liberal = spec$n_liberal,
    n_conservative = spec$n_conservative,
    n_resid_pairs = spec$n_pairs,
    converged = lavInspect(fit, "converged"),
    PD = all(eigen(lavInspect(fit, "cov.lv"))$values > 0),
    CFI = round(fitMeasures(fit, "cfi.scaled"), 3),
    RMSEA = round(fitMeasures(fit, "rmsea.scaled"), 3),
    LC_r = round(lc$std.all, 2),
    LC_se = round(lc$se, 2),
    stringsAsFactors = FALSE
  )
}

results <- do.call(rbind, list(
  fit_one("script07", script07_label, FALSE),
  fit_one("pooled", pooled_label, FALSE),
  fit_one("script07", script07_label, TRUE),
  fit_one("pooled", pooled_label, TRUE)
))

print(results, row.names = FALSE)

changed <- names(pooled_label)[pooled_label != script07_label[names(pooled_label)]]
cat("\nItems that change assignment:", paste(changed, collapse = ", "), "\n")
