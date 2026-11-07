#!/usr/bin/env Rscript
# Verification: refit Table 1/2 sequences with PCA scores vs CFA (WLSMV) scores
# on the same respondents. Run from the project root.

suppressPackageStartupMessages({
  library(lavaan)
  library(estimatr)
  library(car)
})

prep <- readRDS("cache/01_prepared_data.rds")
str07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df

covar_terms <- "gender_f + educ_cat + child_arts + income + age2 + race_f + poli"
variety_terms <- "weak_variety + strong_variety"
composition_terms <- "weak_lib_comp + weak_cons_comp + strong_lib_comp + strong_cons_comp"

# Fit one ordinal CFA on the item structure from script 07 and return
# lavaan factor scores for the rows that are complete on those items.
cfa_scores <- function(items_by_factor, data) {
  model_string <- paste(
    sapply(names(items_by_factor), function(fid)
      sprintf("%s =~ %s", fid, paste(names(items_by_factor[[fid]]), collapse = " + "))),
    collapse = "\n")
  all_items <- unlist(lapply(items_by_factor, names))
  sub <- data[complete.cases(data[all_items]), all_items, drop = FALSE]
  sub[] <- lapply(sub, function(x) as.numeric(unclass(x)))
  fit <- cfa(model_string, data = sub, ordered = all_items,
             estimator = "WLSMV", std.lv = TRUE)
  sc <- as.data.frame(lavPredict(fit))
  list(scores = sc, rows = as.integer(rownames(sub)), fit = fit)
}

act_cfa  <- cfa_scores(str07$activities, df)
weak_cfa <- cfa_scores(str07$weak, df)
strong_cfa <- cfa_scores(str07$strong, df)

# Common analytic sample: complete on every variable in either specification
model_vars_pca <- c(
  "arts_participation", "solitary_leisure", "residual_leisure",
  "weak_variety", "strong_variety", "weak_lib_comp", "weak_cons_comp",
  "strong_lib_comp", "strong_cons_comp",
  "gender_f", "educ_cat", "child_arts", "income", "age2", "race_f", "poli"
)
common_rows <- Reduce(intersect, list(act_cfa$rows, weak_cfa$rows, strong_cfa$rows))

cfa_df <- df[common_rows, ]
cfa_df$arts_participation <- act_cfa$scores$arts[match(common_rows, act_cfa$rows)]
cfa_df$solitary_leisure   <- act_cfa$scores$leisure[match(common_rows, act_cfa$rows)]
cfa_df$residual_leisure   <- act_cfa$scores$residual[match(common_rows, act_cfa$rows)]
cfa_df$weak_variety       <- weak_cfa$scores$variety[match(common_rows, weak_cfa$rows)]
cfa_df$weak_lib_comp      <- weak_cfa$scores$liberal[match(common_rows, weak_cfa$rows)]
cfa_df$weak_cons_comp     <- weak_cfa$scores$conservative[match(common_rows, weak_cfa$rows)]
cfa_df$strong_variety     <- strong_cfa$scores$variety[match(common_rows, strong_cfa$rows)]
cfa_df$strong_lib_comp    <- strong_cfa$scores$liberal[match(common_rows, strong_cfa$rows)]
cfa_df$strong_cons_comp   <- strong_cfa$scores$conservative[match(common_rows, strong_cfa$rows)]


pca_df <- df[common_rows, ]
net <- c("weak_variety","weak_lib_comp","weak_cons_comp","strong_variety","strong_lib_comp","strong_cons_comp")
cat("\nPredictor correlations, PCA arm:\n"); print(round(cor(pca_df[net]), 2))
cat("\nPredictor correlations, CFA arm:\n"); print(round(cor(cfa_df[net]), 2))

ok <- complete.cases(pca_df[c("strong_variety","weak_variety","solitary_leisure","arts_participation")])
cat("\nN complete for PCA bivariates:", sum(ok), "\n")
cat("PCA-scored predictors vs PCA leisure:\n")
print(round(c(strong = cor(pca_df$strong_variety[ok], pca_df$solitary_leisure[ok]),
              weak = cor(pca_df$weak_variety[ok], pca_df$solitary_leisure[ok])), 3))
cat("CFA-scored predictors vs CFA leisure:\n")
print(round(c(strong = cor(cfa_df$strong_variety[ok], cfa_df$solitary_leisure[ok]),
              weak = cor(cfa_df$weak_variety[ok], cfa_df$solitary_leisure[ok])), 3))
cat("CFA predictors vs PCA leisure:\n")
print(round(c(strong = cor(cfa_df$strong_variety[ok], pca_df$solitary_leisure[ok]),
              weak = cor(cfa_df$weak_variety[ok], pca_df$solitary_leisure[ok])), 3))
cat("PCA predictors vs CFA leisure:\n")
print(round(c(strong = cor(pca_df$strong_variety[ok], cfa_df$solitary_leisure[ok]),
              weak = cor(pca_df$weak_variety[ok], cfa_df$solitary_leisure[ok])), 3))
cat("Summary of NA counts in PCA leisure:", sum(is.na(pca_df$solitary_leisure)), "\n")
