#' Model 7 with secondary loadings freed for items with oblimin EFA cross-loadings > .30
#'
#' 1. Per-battery 3-factor oblimin EFA (polychoric, WLS) on the 18 weak and 18 strong items.
#' 2. Factor columns are labeled variety / liberal / conservative from script 01 anchors.
#' 3. For each item assigned to liberal or conservative (script 07 structure), if its
#'    loading on the other composition factor exceeds .30, that secondary loading is freed.
#'    Variety loadings are not freed because G already loads on all items in model 7.
#' 4. Model 7 baseline and the freed model are fit on the same complete cases,
#'    in the no-gender and all-items conditions.
#' Run from the project root: Rscript Scripts/diagnostics/model7_crossloadings.R
suppressPackageStartupMessages({library(lavaan); library(psych)})

prep <- readRDS("cache/01_prepared_data.rds")
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df
threshold <- 0.30
anchors <- list(variety = c("KnowHawaiiPI", "KnowAmIndianppl", "KnowMENAppl"),
                liberal = c("KnowVeryLib", "KnowLGBTQ"),
                conservative = c("KnowVeryCons", "KnowLotsaChurch"))

battery_efa <- function(items, suffix) {
  d <- df[items]; d[] <- lapply(d, function(x) as.numeric(unclass(x)))
  f <- fa(polychoric(d)$rho, nfactors = 3, n.obs = nrow(d), rotate = "oblimin", fm = "wls")
  L <- unclass(f$loadings)
  col <- sapply(anchors, function(a) which.max(colMeans(abs(L[paste0(a, suffix), , drop = FALSE]))))
  m <- L[, as.integer(col), drop = FALSE]; colnames(m) <- names(anchors)
  rownames(m) <- sub("_(weak|strong)$", "", rownames(m))
  m
}
w_items <- unlist(lapply(s07$weak, names)); s_items <- unlist(lapply(s07$strong, names))
Lw <- battery_efa(rownames(unclass(prep$weak_fit$fa$loadings)), "_weak")
Ls <- battery_efa(rownames(unclass(prep$strong_fit$fa$loadings)), "_strong")

cross_list <- function(struct, Lm, suffix) {
  out <- data.frame(item = character(), factor = character(), secondary = character(), loading = numeric(), stringsAsFactors = FALSE)
  for (f in c("liberal", "conservative")) {
    other <- setdiff(c("liberal", "conservative"), f)
    for (it in names(struct[[f]])) {
      base <- sub("_(weak|strong)$", "", it)
      if (!base %in% rownames(Lm)) next
      val <- Lm[base, other]
      if (abs(val) > threshold) out <- rbind(out, data.frame(item = it, factor = f, secondary = other, loading = round(val, 2), stringsAsFactors = FALSE))
    }
  }
  out
}
cross <- rbind(cross_list(s07$weak, Lw, "_weak"), cross_list(s07$strong, Ls, "_strong"))
cat("Cross-loadings > .30 among liberal/conservative-assigned items (before gender exclusion):\n")
print(cross, row.names = FALSE)

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
  c_cross <- cross[cross$item %in% items, ]
  free_lines <- character()
  for (i in seq_len(nrow(c_cross))) {
    fac <- if (c_cross$secondary[i] == "liberal") "L" else "C"
    free_lines <- c(free_lines, sprintf("%s =~ %s", fac, c_cross$item[i]))
  }
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
for (cond in c("no gender", "all items")) {
  b <- build(cond == "no gender")
  results[[length(results) + 1]] <- fit_model(b$base, b$items, paste(cond, "- model 7 baseline"))
  results[[length(results) + 1]] <- fit_model(b$freed, b$items, paste(cond, "- freed secondary loadings"))
  cat("\n", cond, ": cross-loadings freed:\n", sep = "")
  print(b$cross, row.names = FALSE)
}
res <- do.call(rbind, results)
cat("\nFit and liberal-conservative correlation:\n")
print(res, row.names = FALSE)
saveRDS(res, "cache/diagnostics/model7_crossloadings.rds")
