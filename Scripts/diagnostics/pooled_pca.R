suppressPackageStartupMessages({library(psych)})
setwd("/home/omarlizardo/projects/CULTURE/childress-lizardo-culture-network-variety")
prep <- readRDS("cache/01_prepared_data.rds")
df <- prep$df
weak <- prep$network_weak_items; strong <- prep$network_strong_items
items <- c(weak, strong)
d <- as.data.frame(df[items]); d[] <- lapply(d, function(x) as.numeric(unclass(x)))
ok <- complete.cases(d)
cat("N complete:", sum(ok), "\n")
R <- cor(d[ok, ])
fa3 <- principal(d[ok, ], nfactors = 3, rotate = "varimax", scores = TRUE)
print(round(unclass(fa3$loadings), 2)[order(rownames(unclass(fa3$loadings))), ], digits = 2)
cat("\nVariance explained:", round(fa3$Vaccounted["Cumulative Var", 3], 3), "\n")
# Compare pooled factor scores with the batteries' PCA scores
sc <- as.data.frame(fa3$scores)
cat("\nCorrelation of pooled factor scores with battery PCA scores:\n")
print(round(cor(sc, df[ok, c("weak_variety","weak_lib_comp","weak_cons_comp","strong_variety","strong_lib_comp","strong_cons_comp")]), 2))
cat("\nMax within-pooled-score correlation:", round(max(abs(cor(sc)[upper.tri(cor(sc))])), 3), "\n")
