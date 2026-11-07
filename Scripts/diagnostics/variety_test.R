suppressPackageStartupMessages(library(lavaan))
setwd("/home/omarlizardo/projects/CULTURE/childress-lizardo-culture-network-variety")
prep <- readRDS("cache/01_prepared_data.rds")
str07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df
wv_names <- names(str07$weak$variety); sv_names <- names(str07$strong$variety)
items <- c(wv_names, sv_names)
dat <- df[complete.cases(df[items]), items, drop = FALSE]
dat[] <- lapply(dat, function(x) as.numeric(unclass(x)))
mod_free <- paste0("w =~ ", paste(wv_names, collapse = " + "), "\n",
                   "s =~ ", paste(sv_names, collapse = " + "))
ord <- c(wv_names, sv_names)
f_free <- cfa(mod_free, data = dat, ordered = ord, estimator = "WLSMV", std.lv = TRUE)
# Constrain the latent correlation to 1 by estimating a single factor across both batteries
mod_one <- paste0("v =~ ", paste(c(wv_names, sv_names), collapse = " + "))
f_1f <- cfa(mod_one, data = dat, ordered = ord, estimator = "WLSMV", std.lv = TRUE)
cat("N =", nrow(dat), "\n")
cat("Latent correlation (free):\n")
print(parameterEstimates(f_free, standardized = TRUE)[parameterEstimates(f_free)$op == "~~" &
  parameterEstimates(f_free)$lhs == "w" & parameterEstimates(f_free)$rhs == "s", c("est", "std.all", "se")])
cat("Converged:", lavInspect(f_free, "converged"), "  PD cov.lv:", all(eigen(lavInspect(f_free, "cov.lv"))$values > 0), "\n")
print(anova(f_1f, f_free))
cat("CFI free:", round(fitMeasures(f_free, "cfi.scaled"), 3), " CFI one-factor:", round(fitMeasures(f_1f, "cfi.scaled"), 3), "\n")
