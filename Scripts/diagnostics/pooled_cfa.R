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
cat("N =", nrow(dat), " items =", length(all_items), "\n")
sub3 <- paste0(
  "V =~ ", paste(c(wv, sv), collapse = " + "), "\n",
  "L =~ ", paste(c(wl, sl), collapse = " + "), "\n",
  "C =~ ", paste(c(wc, sc), collapse = " + "))
method <- paste0(
  "Mw =~ ", paste(c(wv, wl, wc), collapse = " + "), "\n",
  "Ms =~ ", paste(c(sv, sl, sc), collapse = " + "), "\n",
  "Mw ~~ 0*V\nMw ~~ 0*L\nMw ~~ 0*C\nMs ~~ 0*V\nMs ~~ 0*L\nMs ~~ 0*C\n")
fit_sub <- cfa(sub3, data = dat, ordered = all_items, estimator = "WLSMV", std.lv = TRUE)
fit_meth <- cfa(paste0(sub3, "\n", method), data = dat, ordered = all_items, estimator = "WLSMV", std.lv = TRUE)
cat("\n--- Pooled 3-factor (no method factors) ---\n")
cat("converged:", lavInspect(fit_sub, "converged"), " CFI:", round(fitMeasures(fit_sub, "cfi.scaled"), 3),
    " RMSEA:", round(fitMeasures(fit_sub, "rmsea.scaled"), 3), "\n")
cat("\n--- Pooled 3-factor + battery method factors ---\n")
cat("converged:", lavInspect(fit_meth, "converged"), " CFI:", round(fitMeasures(fit_meth, "cfi.scaled"), 3),
    " RMSEA:", round(fitMeasures(fit_meth, "rmsea.scaled"), 3), "\n")
cat("PD cov.lv:", all(eigen(lavInspect(fit_meth, "cov.lv"))$values > 0), "\n")
print(anova(fit_sub, fit_meth))
pe <- parameterEstimates(fit_meth, standardized = TRUE)
cat("\nLatent correlations (substantive):\n")
print(pe[pe$op == "~~" & pe$lhs %in% c("V","L","C") & pe$rhs %in% c("V","L","C") & pe$lhs != pe$rhs,
         c("lhs","rhs","std.all","se")], row.names = FALSE, digits = 3)
cat("\nMethod factor variances and correlations:\n")
print(pe[pe$op == "~~" & pe$lhs %in% c("Mw","Ms") & pe$rhs %in% c("Mw","Ms"), c("lhs","rhs","std.all","se")], row.names = FALSE, digits = 3)
cat("\nMethod factor loadings (std):\n")
ld <- pe[pe$op == "=~" & pe$lhs %in% c("Mw","Ms"), c("lhs","rhs","std.all")]
print(aggregate(std.all ~ lhs, data = ld, FUN = function(x) round(range(x), 2)))
