suppressPackageStartupMessages(library(lavaan))
setwd("/home/omarlizardo/projects/CULTURE/childress-lizardo-culture-network-variety")
prep <- readRDS("cache/01_prepared_data.rds")
str07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df
drop_gender <- function(x) x[!grepl("^Know(men|women)_", x)]
wv <- drop_gender(names(str07$weak$variety)); wl <- drop_gender(names(str07$weak$liberal)); wc <- drop_gender(names(str07$weak$conservative))
sv <- drop_gender(names(str07$strong$variety)); sl <- drop_gender(names(str07$strong$liberal)); sc <- drop_gender(names(str07$strong$conservative))
all_items <- c(wv, wl, wc, sv, sl, sc)
dat <- df[complete.cases(df[all_items]), all_items, drop = FALSE]
dat[] <- lapply(dat, function(x) as.numeric(unclass(x)))
weak_items <- c(wv, wl, wc)
pairs_w <- weak_items; pairs_s <- sub("_weak$", "_strong", weak_items)
stopifnot(all(pairs_s %in% c(sv, sl, sc)))
resid_syntax <- paste(sprintf("%s ~~ %s", pairs_w, pairs_s), collapse = "\n")
base <- paste0(
  "G =~ ", paste(all_items, collapse = " + "), "\n",
  "L =~ ", paste(c(wl, sl), collapse = " + "), "\n",
  "C =~ ", paste(c(wc, sc), collapse = " + "), "\n",
  "G ~~ 0*L\nG ~~ 0*C\n")
pooled3 <- paste0(
  "V =~ ", paste(c(wv, sv), collapse = " + "), "\n",
  "L =~ ", paste(c(wl, sl), collapse = " + "), "\n",
  "C =~ ", paste(c(wc, sc), collapse = " + "))
fits <- list(
  pooled3_no_gender = cfa(pooled3, data = dat, ordered = all_items, estimator = "WLSMV", std.lv = TRUE),
  bifactor_no_gender = cfa(base, data = dat, ordered = all_items, estimator = "WLSMV", std.lv = TRUE),
  bifactor_resid_no_gender = cfa(paste0(base, resid_syntax), data = dat, ordered = all_items, estimator = "WLSMV", std.lv = TRUE)
)
cat("N =", nrow(dat), " items =", length(all_items), " residual pairs =", length(pairs_w), "\n")
for (nm in names(fits)) {
  f <- fits[[nm]]
  cat(sprintf("%-26s conv=%s PD=%s CFI=%.3f RMSEA=%.3f\n", nm, lavInspect(f, "converged"),
    all(eigen(lavInspect(f, "cov.lv"))$values > 0), fitMeasures(f, "cfi.scaled"), fitMeasures(f, "rmsea.scaled")))
}
pe <- parameterEstimates(fits$bifactor_resid_no_gender, standardized = TRUE)
cat("\nL-C latent correlation (bifactor, residuals):",
    round(pe$std.all[pe$op == "~~" & pe$lhs == "L" & pe$rhs == "C"], 3), "\n")
rc <- pe[pe$op == "~~" & pe$lhs %in% pairs_w & pe$rhs %in% pairs_s, ]
cat("Residual correlations range:", round(range(rc$std.all), 2), "\n")
cat("Heywood-type loadings or variances outside (0,1]:",
    any(pe$std.all[pe$op == "=~"] > 1) || any(pe$std.all[pe$op == "~~" & pe$lhs == pe$rhs] < 0), "\n")
