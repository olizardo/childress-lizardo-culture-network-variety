#' Model 7 with secondary loadings freed, flagged from the POOLED 36-item oblimin EFA
#'
#' Same freeing rule as model7_crossloadings.R (secondary loading > .30 on the other
#' composition factor), but the flags come from one 3-factor oblimin EFA on all 36
#' items. Each item's weak-tie and strong-tie rows are read separately.
#' Compares the item list and fit against the per-battery version saved in
#' cache/diagnostics/model7_crossloadings.rds.
#' Run from the project root: Rscript Scripts/diagnostics/model7_crossloadings_pooled.R
suppressPackageStartupMessages({library(lavaan); library(psych)})

prep <- readRDS("cache/01_prepared_data.rds")
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df
threshold <- 0.30
anchors <- list(variety = c("KnowHawaiiPI", "KnowAmIndianppl", "KnowMENAppl"),
                liberal = c("KnowVeryLib", "KnowLGBTQ"),
                conservative = c("KnowVeryCons", "KnowLotsaChurch"))

all36 <- c(rownames(unclass(prep$weak_fit$fa$loadings)), rownames(unclass(prep$strong_fit$fa$loadings)))
d36 <- df[all36]; d36[] <- lapply(d36, function(x) as.numeric(unclass(x)))
d36 <- d36[complete.cases(d36), ]
f36 <- fa(polychoric(d36)$rho, nfactors = 3, n.obs = nrow(d36), rotate = "oblimin", fm = "wls")
Lp <- unclass(f36$loadings)
cols <- sapply(anchors, function(a) {
  rows <- as.vector(outer(a, c("_weak", "_strong"), paste0))
  which.max(colMeans(abs(Lp[rows, , drop = FALSE])))
})
Lp <- Lp[, as.integer(cols), drop = FALSE]; colnames(Lp) <- names(anchors)
cat("Pooled oblimin loadings for the flagged items (liberal/conservative columns):\n")

cross_list <- function(struct, suffix) {
  out <- data.frame(item = character(), factor = character(), secondary = character(), loading = numeric(), stringsAsFactors = FALSE)
  for (f in c("liberal", "conservative")) {
    other <- setdiff(c("liberal", "conservative"), f)
    for (it in names(struct[[f]])) {
      if (!it %in% rownames(Lp)) next
      val <- Lp[it, other]
      if (abs(val) > threshold) out <- rbind(out, data.frame(item = it, factor = f, secondary = other, loading = round(val, 2), stringsAsFactors = FALSE))
    }
  }
  out
}
cross_pooled <- rbind(cross_list(s07$weak, "_weak"), cross_list(s07$strong, "_strong"))
cat("\nPooled-EFA cross-loadings > .30 (before gender exclusion):\n")
print(cross_pooled, row.names = FALSE)

build <- function(drop_gender) {
  is_g <- function(x) grepl("^Know(men|women)_", x)
  wv <- names(s07$weak$variety); wl <- names(s07$weak$liberal); wc <- names(s07$weak$conservative)
  sv <- names(s07$strong$variety); sl <- names(s07$strong$liberal); sc <- names(s07$strong$conservative)
  items <- c(wv, wl, wc, sv, sl, sc)
  if (drop_gender) items <- items[!is_g(items)]
  liberal <- c(wl, sl); conservative <- c(wc, sc)
  liberal <- liberal[liberal %in% items]; conservative <- conservative[conservative %in% items]
  weak <- items[grepl("_weak$", items)]
  pairs <- data.frame(w = weak, s = sub("_weak$", "_strong", weak), stringsAsFactors = FALSE)
  pairs <- pairs[pairs$s %in% items, ]
  base_syn <- paste0(
    "G =~ ", paste(items, collapse = " + "), "\n",
    "L =~ ", paste(liberal, collapse = " + "), "\n",
    "C =~ ", paste(conservative, collapse = " + "), "\n",
    "G ~~ 0*L\nG ~~ 0*C\n",
    paste(sprintf("%s ~~ %s", pairs$w, pairs$s), collapse = "\n"), "\n")
  c_cross <- cross_pooled[cross_pooled$item %in% items, ]
  free_lines <- sprintf("%s =~ %s", ifelse(c_cross$secondary == "liberal", "L", "C"), c_cross$item)
  list(base = base_syn, freed = paste0(base_syn, paste(free_lines, collapse = "\n"), "\n"),
       items = items, cross = c_cross)
}

fit_model <- function(syn, items, label) {
  d <- df[items]; d[] <- lapply(d, function(x) as.numeric(unclass(x)))
  d <- d[complete.cases(d), ]
  f <- cfa(syn, data = d, ordered = items, estimator = "WLSMV", std.lv = TRUE)
  pe <- parameterEstimates(f, standardized = TRUE)
  lc <- pe[pe$op == "~~" & pe$lhs == "L" & pe$rhs == "C", ]
  data.frame(model = label, N = nrow(d), converged = lavInspect(f, "converged"),
    PD = all(eigen(lavInspect(f, "cov.lv"))$values > 0),
    CFI = round(fitMeasures(f, "cfi.scaled"), 3), RMSEA = round(fitMeasures(f, "rmsea.scaled"), 3),
    SRMR = round(fitMeasures(f, "srmr"), 3), LC_r = round(lc$std.all, 2), LC_se = round(lc$se, 2),
    stringsAsFactors = FALSE)
}

results <- list()
flag_sets <- list()
for (cond in c("no gender", "all items")) {
  b <- build(cond == "no gender")
  flag_sets[[cond]] <- b$cross$item
  cat("\n", cond, ": pooled-flagged loadings freed:\n", sep = "")
  print(b$cross, row.names = FALSE)
  results[[length(results) + 1]] <- fit_model(b$base, b$items, paste(cond, "- model 7 baseline"))
  results[[length(results) + 1]] <- fit_model(b$freed, b$items, paste(cond, "- pooled-flag freed"))
}
res_pooled <- do.call(rbind, results)
cat("\nPooled-flag fit:\n"); print(res_pooled, row.names = FALSE)

res_battery <- readRDS("cache/diagnostics/model7_crossloadings.rds")
cat("\nPer-battery flag fit (from model7_crossloadings.rds):\n")
print(res_battery[grepl("freed", res_battery$model), ], row.names = FALSE)

cat("\nItem lists (freed) by source:\n")
for (cond in names(flag_sets)) cat(cond, "| pooled:", paste(flag_sets[[cond]], collapse = ", "), "\n")
saveRDS(list(pooled = res_pooled, flags = flag_sets), "cache/diagnostics/model7_crossloadings_pooled.rds")
