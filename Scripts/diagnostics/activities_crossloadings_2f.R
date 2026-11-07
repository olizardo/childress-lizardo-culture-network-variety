#' Activities two-factor (arts, leisure; 17 items) CFA with cross-loadings freed
#' for items with oblimin EFA loadings > .30 on the other factor.
#' The 3-factor activity EFA does not yield a separable residual factor (see
#' activities_crossloadings.R), so this uses the two-factor alternative.
#' Run from the project root: Rscript Scripts/diagnostics/activities_crossloadings_2f.R
suppressPackageStartupMessages({library(lavaan); library(psych)})

prep <- readRDS("cache/01_prepared_data.rds")
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
act <- s07$activities
arts <- names(act$arts); leis <- names(act$leisure)
items <- c(arts, leis)
threshold <- 0.30
d <- prep$df[items]; d[] <- lapply(d, function(x) as.numeric(unclass(x)))
d <- d[complete.cases(d), ]
efa <- fa(polychoric(d)$rho, nfactors = 2, n.obs = nrow(d), rotate = "oblimin", fm = "wls")
L <- unclass(efa$loadings)
cols <- c(arts = unname(which.max(colMeans(abs(L[c("museum","art_gallery"), , drop = FALSE])))),
          leisure = unname(which.max(colMeans(abs(L[c("go_for_walk","exercise_or_yoga"), , drop = FALSE])))))
stopifnot(cols[["arts"]] != cols[["leisure"]])
L <- L[, unname(cols), drop = FALSE]; colnames(L) <- names(cols)
cat("2-factor oblimin EFA, N =", nrow(d), "\n"); print(round(L, 2), cutoff = 0.30)
cat("EFA interfactor r:", round(efa$Phi[unname(cols)[1], unname(cols)[2]], 2), "\n")

cross <- data.frame(item = character(), factor = character(), secondary = character(), loading = numeric(), stringsAsFactors = FALSE)
for (f in c("arts","leisure")) for (it in names(act[[f]])) {
  other <- setdiff(c("arts","leisure"), f)
  v <- L[it, other]
  if (abs(v) > threshold) cross <- rbind(cross, data.frame(item = it, factor = f, secondary = other, loading = round(v, 2), stringsAsFactors = FALSE))
}
cat("\nCross-loadings > .30 to free:\n"); print(cross, row.names = FALSE)

base_syn <- sprintf("arts =~ %s\nleisure =~ %s", paste(arts, collapse = " + "), paste(leis, collapse = " + "))
freed_syn <- paste0(base_syn, "\n", paste(sprintf("%s =~ %s", cross$secondary, cross$item), collapse = "\n"))
fit_one <- function(syn, label) {
  f <- cfa(syn, data = d, ordered = items, estimator = "WLSMV", std.lv = TRUE)
  pe <- parameterEstimates(f, standardized = TRUE)
  r <- pe$std.all[pe$op == "~~" & pe$lhs == "arts" & pe$rhs == "leisure"]
  data.frame(model = label, N = nrow(d), converged = lavInspect(f, "converged"),
    PD = all(eigen(lavInspect(f, "cov.lv"))$values > 0),
    CFI = round(fitMeasures(f, "cfi.scaled"), 3), RMSEA = round(fitMeasures(f, "rmsea.scaled"), 3),
    SRMR = round(fitMeasures(f, "srmr"), 3), r_arts_leisure = round(r, 2), stringsAsFactors = FALSE)
}
res <- rbind(fit_one(base_syn, "2-factor baseline"), fit_one(freed_syn, "2-factor, cross-loadings freed"))
cat("\nFit and factor correlation:\n"); print(res, row.names = FALSE)
saveRDS(list(results = res, cross = cross), "cache/diagnostics/activities_crossloadings_2f.rds")
