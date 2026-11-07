#' Activities CFA with secondary loadings freed for items with oblimin EFA cross-loadings > .30
#'
#' 1. 3-factor oblimin EFA (polychoric, WLS) on the 20 activity items.
#' 2. Columns labeled arts / leisure / residual using anchors from script 07
#'    (arts: museum, art gallery; leisure: walk, exercise; residual: gardening, home repair).
#' 3. For each item, any loading on a non-primary factor with |loading| > .30 is freed
#'    in the script 07 three-factor CFA (simple structure otherwise).
#' Compares fit and factor correlations against the baseline CFA.
#' Run from the project root: Rscript Scripts/diagnostics/activities_crossloadings.R
suppressPackageStartupMessages({library(lavaan); library(psych)})

prep <- readRDS("cache/01_prepared_data.rds")
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
act <- s07$activities
threshold <- 0.30
fac_names <- c("arts", "leisure", "residual")
anchors <- list(arts = c("museum", "art_gallery"),
                leisure = c("go_for_walk", "exercise_or_yoga"),
                residual = c("gardening", "home_auto_repair"))
items_by_factor <- lapply(act, names)
all20 <- unlist(items_by_factor, use.names = FALSE)
d <- prep$df[all20]; d[] <- lapply(d, function(x) as.numeric(unclass(x)))
d <- d[complete.cases(d), ]
efa <- fa(polychoric(d)$rho, nfactors = 3, n.obs = nrow(d), rotate = "oblimin", fm = "wls")
L <- unclass(efa$loadings)
cols <- sapply(anchors, function(a) which.max(colMeans(abs(L[a, , drop = FALSE]))))
L <- L[, as.integer(cols), drop = FALSE]; colnames(L) <- fac_names
cat("Oblimin EFA loadings (columns labeled by anchors), N =", nrow(d), "\n")
print(round(L, 2), cutoff = 0.30)
cat("EFA interfactor correlations:\n"); print(round(efa$Phi[as.integer(cols), as.integer(cols)], 2))

cross <- data.frame(item = character(), factor = character(), secondary = character(), loading = numeric(), stringsAsFactors = FALSE)
for (f in fac_names) {
  for (it in items_by_factor[[f]]) {
    for (other in setdiff(fac_names, f)) {
      val <- L[it, other]
      if (abs(val) > threshold) cross <- rbind(cross, data.frame(item = it, factor = f, secondary = other, loading = round(val, 2), stringsAsFactors = FALSE))
    }
  }
}
cat("\nCross-loadings > .30 to free:\n"); print(cross, row.names = FALSE)

base_syn <- paste(sapply(fac_names, function(f)
  sprintf("%s =~ %s", f, paste(items_by_factor[[f]], collapse = " + "))), collapse = "\n")
free_lines <- sprintf("%s =~ %s", cross$secondary, cross$item)
freed_syn <- paste0(base_syn, "\n", paste(free_lines, collapse = "\n"))

fit_one <- function(syn, label) {
  f <- cfa(syn, data = d, ordered = all20, estimator = "WLSMV", std.lv = TRUE)
  pe <- parameterEstimates(f, standardized = TRUE)
  cors <- pe[pe$op == "~~" & pe$lhs != pe$rhs & pe$lhs %in% fac_names & pe$rhs %in% fac_names, ]
  cors <- cors[!duplicated(t(apply(cors[, c("lhs","rhs")], 1, sort))), ]
  data.frame(model = label, N = nrow(d), converged = lavInspect(f, "converged"),
    PD = all(eigen(lavInspect(f, "cov.lv"))$values > 0),
    CFI = round(fitMeasures(f, "cfi.scaled"), 3), RMSEA = round(fitMeasures(f, "rmsea.scaled"), 3),
    SRMR = round(fitMeasures(f, "srmr"), 3),
    r_arts_leisure = round(cors$std.all[cors$lhs == "arts" & cors$rhs == "leisure"], 2),
    r_arts_residual = round(cors$std.all[cors$lhs == "arts" & cors$rhs == "residual"], 2),
    r_leisure_residual = round(cors$std.all[cors$lhs == "leisure" & cors$rhs == "residual"], 2),
    stringsAsFactors = FALSE)
}
res <- rbind(fit_one(base_syn, "baseline 3-factor CFA"), fit_one(freed_syn, "cross-loadings freed"))
cat("\nFit and factor correlations:\n"); print(res, row.names = FALSE)
saveRDS(list(results = res, cross = cross), "cache/diagnostics/activities_crossloadings.rds")
