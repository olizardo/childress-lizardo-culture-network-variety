#' Weak-tie CFA with modifications selected on one random half and tested on the other
#'
#' Baseline: three-factor (variety, liberal, conservative) WLSMV CFA on the 18 weak-tie items,
#' using the script 07 per-battery sorting (the working assignment in the Results Draft),
#' with V, L, C factors freely correlated. Two conditions: all 18 items, and the 16 items
#' without the gender items (Knowmen_/Knowwomen_).
#'
#' Modifications: residual covariances and cross-loadings with modification index >= 10,
#' selected on one random half (A or B), then refit on the other half. The holdout scaled
#' chi-square difference (lavTestLRT) and fit indices are reported for the selected
#' specification, and the selection is then reversed so each half serves once as holdout.
#' The selection is one pass over the modification indices, so the specification is data-driven
#' and the holdout test is the check against overfitting.
#'
#' Run from the project root: Rscript Scripts/diagnostics/weak_tie_modified_cfa.R

suppressPackageStartupMessages({
  library(lavaan)
})

prep <- readRDS("cache/01_prepared_data.rds")
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df

fac_map <- c(variety = "V", liberal = "L", conservative = "C")
weak_items <- unlist(lapply(s07$weak, names), use.names = FALSE)
item_fac <- setNames(rep(NA_character_, length(weak_items)), weak_items)
for (f in names(s07$weak)) item_fac[names(s07$weak[[f]])] <- fac_map[[f]]

is_gender <- grepl("^Know(men|women)_", weak_items)
dat_all <- df[weak_items]
dat_all[] <- lapply(dat_all, function(x) as.numeric(unclass(x)))
dat_all <- dat_all[complete.cases(dat_all), ]

build_syntax <- function(items, resid = NULL, cross = NULL) {
  fac_lines <- vapply(c("V", "L", "C"), function(f) {
    paste0(f, " =~ ", paste(items[item_fac[items] == f], collapse = " + "))
  }, character(1))
  lines <- c(
    fac_lines,
    if (!is.null(cross) && nrow(cross) > 0) sprintf("%s =~ %s", cross$lhs, cross$rhs),
    if (!is.null(resid) && nrow(resid) > 0) sprintf("%s ~~ %s", resid$lhs, resid$rhs)
  )
  paste(lines, collapse = "\n")
}

fit_cfa <- function(syn, d) {
  cfa(syn, data = d, ordered = colnames(d), estimator = "WLSMV", std.lv = TRUE)
}

select_mods <- function(d, items, mi_min = 10, epc_min = 0.30, max_resid = 6, max_cross = 4) {
  base <- fit_cfa(build_syntax(items), d[items])
  mi <- modindices(base, sort. = TRUE, minimum.value = mi_min)
  mi <- mi[mi$op %in% c("=~", "~~") & mi$lhs != mi$rhs & abs(mi$sepc.all) >= epc_min, ]
  resid <- mi[mi$op == "~~" & mi$lhs %in% items & mi$rhs %in% items, c("lhs", "rhs"), drop = FALSE]
  cross <- mi[mi$op == "=~" & mi$rhs %in% items & mi$lhs %in% c("V", "L", "C"), c("lhs", "rhs"), drop = FALSE]
  cross <- cross[item_fac[cross$rhs] != cross$lhs, , drop = FALSE]
  list(resid = head(resid, max_resid), cross = head(cross, max_cross))
}

summarize_fit <- function(f, label, base = NULL) {
  pe <- parameterEstimates(f, standardized = TRUE)
  lc <- pe[pe$op == "~~" & pe$lhs == "L" & pe$rhs == "C", ]
  row <- data.frame(
    model = label,
    converged = lavInspect(f, "converged"),
    PD = all(eigen(lavInspect(f, "cov.lv"))$values > 0),
    CFI = round(fitMeasures(f, "cfi.scaled"), 3),
    RMSEA = round(fitMeasures(f, "rmsea.scaled"), 3),
    SRMR = round(fitMeasures(f, "srmr"), 3),
    LC_r = round(lc$std.all, 2),
    LC_se = round(lc$se, 2),
    stringsAsFactors = FALSE
  )
  row$df_diff <- NA_real_
  row$scaled_chisq_diff <- NA_real_
  row$p <- NA_real_
  if (!is.null(base)) {
    tst <- lavTestLRT(f, base)
    row$df_diff <- tst[2, "Df diff"]
    row$scaled_chisq_diff <- round(tst[2, "Chisq diff"], 1)
    row$p <- signif(tst[2, "Pr(>Chisq)"], 3)
  }
  row
}

run_condition <- function(cond, items, idx_a) {
  d_all <- dat_all[items]
  halves <- list(A = idx_a, B = setdiff(seq_len(nrow(d_all)), idx_a))
  out <- list()
  for (sel in c("A", "B")) {
    hold <- setdiff(c("A", "B"), sel)
    d_sel <- d_all[halves[[sel]], , drop = FALSE]
    d_hold <- d_all[halves[[hold]], , drop = FALSE]
    spec <- select_mods(d_sel, items)
    base_hold <- fit_cfa(build_syntax(items), d_hold)
    mod_hold <- fit_cfa(build_syntax(items, spec$resid, spec$cross), d_hold)
    label_pref <- sprintf("%s | selected on %s, tested on %s", cond, sel, hold)
    out[[length(out) + 1]] <- cbind(
      summarize_fit(base_hold, paste(label_pref, "| baseline")),
      n_resid = 0L, n_cross = 0L, n_items = length(items), N = nrow(d_hold))
    out[[length(out) + 1]] <- cbind(
      summarize_fit(mod_hold, paste(label_pref, "| modified"), base = base_hold),
      n_resid = nrow(spec$resid), n_cross = nrow(spec$cross), n_items = length(items), N = nrow(d_hold))
    spec_list <- list(resid = spec$resid, cross = spec$cross)
    cat("\n", label_pref, "\nResidual covariances:\n", sep = "")
    print(spec$resid, row.names = FALSE)
    cat("Cross-loadings:\n")
    print(spec$cross, row.names = FALSE)
  }
  out
}

set.seed(4217)
idx_a <- sample(seq_len(nrow(dat_all)), size = floor(nrow(dat_all) / 2))

items_all <- weak_items
items_ng <- weak_items[!is_gender]

res <- c(
  run_condition("all items", items_all, idx_a),
  run_condition("no gender", items_ng, idx_a)
)
res <- do.call(rbind, res)
rownames(res) <- NULL

cat("\nHoldout results (scaled chi-square difference is modified vs baseline on the holdout half):\n")
print(res, row.names = FALSE)

full_fits <- list(
  all = list(base = fit_cfa(build_syntax(items_all), dat_all[items_all])),
  ng = list(base = fit_cfa(build_syntax(items_ng), dat_all[items_ng]))
)
full_spec <- list(all = select_mods(dat_all, items_all), ng = select_mods(dat_all, items_ng))
full_fits$all$mod <- fit_cfa(build_syntax(items_all, full_spec$all$resid, full_spec$all$cross), dat_all[items_all])
full_fits$ng$mod <- fit_cfa(build_syntax(items_ng, full_spec$ng$resid, full_spec$ng$cross), dat_all[items_ng])
full_tbl <- rbind(
  summarize_fit(full_fits$all$base, "full sample, all items | baseline"),
  summarize_fit(full_fits$all$mod, "full sample, all items | modified", base = full_fits$all$base),
  summarize_fit(full_fits$ng$base, "full sample, no gender | baseline"),
  summarize_fit(full_fits$ng$mod, "full sample, no gender | modified", base = full_fits$ng$base)
)
cat("\nFull-sample refits (selection in-sample; optimistic, shown for reference):\n")
print(full_tbl, row.names = FALSE)

saveRDS(list(holdout = res, full = full_tbl, full_spec = full_spec), "cache/diagnostics/weak_tie_modified_cfa.rds")
