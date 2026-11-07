suppressPackageStartupMessages(library(lavaan))
setwd("/home/omarlizardo/projects/CULTURE/childress-lizardo-culture-network-variety")
prep <- readRDS("cache/01_prepared_data.rds")
str07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df
wv <- names(str07$weak$variety); wl <- names(str07$weak$liberal); wc <- names(str07$weak$conservative)
sv <- names(str07$strong$variety); sl <- names(str07$strong$liberal); sc <- names(str07$strong$conservative)
all_items <- c(wv, wl, wc, sv, sl, sc)
dat <- df[complete.cases(df[all_items]), all_items, drop = FALSE]
dat[] <- lapply(dat, function(x) as.numeric(unclass(x)))
weak_items <- c(wv, wl, wc)
strong_items <- c(sv, sl, sc)
pairs <- data.frame(w = weak_items, s = sub("_weak$", "_strong", weak_items), stringsAsFactors = FALSE)
stopifnot(all(pairs$s %in% strong_items))
resid_syntax <- paste(sprintf("%s ~~ %s", pairs$w, pairs$s), collapse = "\n")
base <- paste0(
  "G =~ ", paste(all_items, collapse = " + "), "\n",
  "L =~ ", paste(c(wl, sl), collapse = " + "), "\n",
  "C =~ ", paste(c(wc, sc), collapse = " + "), "\n",
  "G ~~ 0*L\nG ~~ 0*C\n")
fit_base <- cfa(base, data = dat, ordered = all_items, estimator = "WLSMV", std.lv = TRUE)
fit_res  <- cfa(paste0(base, resid_syntax), data = dat, ordered = all_items, estimator = "WLSMV", std.lv = TRUE)
cat("N =", nrow(dat), " residual pairs:", nrow(pairs), "\n")
for (nm in c("base", "resid")) {
  f <- if (nm == "base") fit_base else fit_res
  cat(sprintf("%-6s converged=%s PD=%s CFI=%.3f RMSEA=%.3f\n", nm, lavInspect(f, "converged"),
    all(eigen(lavInspect(f, "cov.lv"))$values > 0),
    fitMeasures(f, "cfi.scaled"), fitMeasures(f, "rmsea.scaled")))
}
cat("\nScaled difference test (residual pairs vs base):\n")
print(anova(fit_base, fit_res))
pe <- parameterEstimates(fit_res, standardized = TRUE)
cat("\nL-C latent correlation (std):", round(pe$std.all[pe$op == "~~" & pe$lhs == "L" & pe$rhs == "C"], 3), "\n")
rc <- pe[pe$op == "~~" & pe$lhs %in% weak_items & pe$rhs %in% strong_items & pe$lhs != pe$rhs, ]
cat("Residual correlations (std), weak vs strong same group:\n")
print(round(range(rc$std.all), 2))
cat("\nResidual correlations above 1 or below 0:\n")
print(rc[rc$std.all > 1 | rc$std.all < 0, c("lhs", "rhs", "est", "std.all", "se")], row.names = FALSE, digits = 3)
cat("\nAll residual correlations:\n")
print(rc[, c("lhs", "rhs", "std.all")], row.names = FALSE, digits = 2)
cat("\nHeywood/variance check (theta, std.all of observed items > 1 or < 0):\n")
th <- pe[pe$op == "~~" & pe$lhs == pe$rhs & pe$lhs %in% all_items, c("lhs", "est", "std.all")]
print(th[th$std.all < 0.01 | th$std.all > 1, ], row.names = FALSE, digits = 3)
