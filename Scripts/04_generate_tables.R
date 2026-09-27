#!/usr/bin/env Rscript
# Scripts/04_generate_tables.R
#
# Generates publication-ready markdown tables to cache/:
#   Table 1  - Public Arts Participation regression models (M1-M4)
#   Table 2  - Solitary Leisure regression models (M1-M4)
#   Table A1 - Descriptive statistics
#   Table A2 - Activity/leisure item factor loadings
#   Table A3 - Weak- and strong-tie item factor loadings
#   Table A5 - Network variety factor validation (correlation with group counts)
#
# NOTE: Table A4 (correlation matrix of continuous/ordinal predictors) has
# been replaced by a correlation heat map (Plots/figA1_predictor_correlation_heatmap.png)
# per project style, generated in Scripts/03_generate_figures.R.

suppressPackageStartupMessages({
  library(dplyr)
  library(estimatr)
})

dir.create("cache", showWarnings = FALSE, recursive = TRUE)

prep <- readRDS("cache/01_prepared_data.rds")
mods <- readRDS("cache/02_models.rds")
df <- prep$df

stars <- function(p) {
  ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
}

fmt_cell <- function(b, se, p) sprintf("%.3f%s (%.3f)", b, stars(p), se)

# ------------------------------------------------------------------------
# Tables 1 and 2: regression model sequences
# ------------------------------------------------------------------------
build_reg_table <- function(models, row_order, row_labels) {
  cells <- lapply(models, function(m) {
    s <- summary(m)$coefficients
    out <- setNames(rep(NA_character_, length(row_order)), row_order)
    for (v in row_order) {
      if (v %in% rownames(s)) out[v] <- fmt_cell(s[v, 1], s[v, 2], s[v, 4])
    }
    out
  })
  tab <- as.data.frame(cells, check.names = FALSE)
  rownames(tab) <- row_labels
  n_rows <- sapply(models, function(m) length(residuals(m)))
  r2_rows <- sapply(models, function(m) summary(m)$r.squared)
  tab <- rbind(tab, N = sprintf("%d", n_rows))
  tab <- rbind(tab, `R-squared` = sprintf("%.3f", r2_rows))
  tab
}

row_order_1 <- c(
  "(Intercept)", "weak_variety", "strong_variety",
  "weak_lib_comp", "weak_cons_comp", "strong_lib_comp", "strong_cons_comp",
  "gender_fMan", "gender_fNon-binary/Trans",
  "educ_catSome college", "educ_catCollege degree", "educ_catGraduate degree",
  "child_arts", "income", "age2",
  "race_fblack", "race_flatine", "race_fAAPI", "race_fmixed/other", "poli"
)
row_labels_1 <- c(
  "Intercept", "Weak-Tie Variety", "Strong-Tie Variety",
  "Weak-Tie Liberal Composition", "Weak-Tie Conservative Composition",
  "Strong-Tie Liberal Composition", "Strong-Tie Conservative Composition",
  "Gender: Man (ref. Woman)", "Gender: Non-binary/Trans",
  "Education: Some College", "Education: College Degree", "Education: Graduate Degree",
  "Childhood Arts Exposure", "Household Income", "Age",
  "Race: Black (ref. White)", "Race: Hispanic/Latine", "Race: AAPI", "Race: Mixed/Other",
  "Political Ideology (Liberal-Conservative)"
)

table1 <- build_reg_table(mods$arts_models, row_order_1, row_labels_1)
table2 <- build_reg_table(mods$leisure_models, row_order_1, row_labels_1)

write_md_table <- function(tab, path, col_names = c("M1", "M2", "M3", "M4")) {
  df_out <- data.frame(Predictor = rownames(tab), tab, check.names = FALSE)
  colnames(df_out) <- c("Predictor", col_names)
  df_out[is.na(df_out)] <- ""
  lines <- c(
    paste0("| ", paste(colnames(df_out), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df_out)), collapse = "|"), "|"),
    apply(df_out, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  )
  writeLines(lines, path)
}

write_md_table(table1, "cache/table1_arts_participation.md")
write_md_table(table2, "cache/table2_solitary_leisure.md")

# ------------------------------------------------------------------------
# Table A1: Descriptive statistics
# ------------------------------------------------------------------------
desc_continuous <- function(x) sprintf("%.2f (%.2f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))

cont_vars <- c(
  "arts_participation" = "Public Arts Participation (factor score)",
  "solitary_leisure" = "Solitary Leisure (factor score)",
  "weak_variety" = "Weak-Tie Variety (factor score)",
  "strong_variety" = "Strong-Tie Variety (factor score)",
  "weak_lib_comp" = "Weak-Tie Liberal Composition (factor score)",
  "weak_cons_comp" = "Weak-Tie Conservative Composition (factor score)",
  "strong_lib_comp" = "Strong-Tie Liberal Composition (factor score)",
  "strong_cons_comp" = "Strong-Tie Conservative Composition (factor score)",
  "age2" = "Age (years)",
  "child_arts" = "Childhood Arts Exposure (1-7)",
  "income" = "Household Income (1-13)",
  "poli" = "Political Ideology (1-7, liberal-conservative)"
)

table_a1_cont <- data.frame(
  Variable = unname(cont_vars),
  `Mean (SD)` = sapply(names(cont_vars), function(v) desc_continuous(df[[v]])),
  N = sapply(names(cont_vars), function(v) sum(!is.na(df[[v]]))),
  check.names = FALSE
)

cat_summary <- function(var, labels_fn = as_factor_pct) NULL
pct_table <- function(x, name) {
  tab <- table(x, useNA = "no")
  pct <- round(100 * tab / sum(tab), 1)
  data.frame(Variable = name, Category = names(tab), `N (%)` = sprintf("%d (%.1f%%)", tab, pct), check.names = FALSE)
}

table_a1_cat <- bind_rows(
  pct_table(haven::as_factor(df$gender), "Gender Identity"),
  pct_table(df$educ_cat, "Educational Attainment"),
  pct_table(haven::as_factor(df$race3), "Race/Ethnicity")
)

writeLines(
  c(
    "**Table A1a. Descriptive Statistics: Continuous and Ordinal Variables**", "",
    "| Variable | Mean (SD) | N |", "|---|---|---|",
    apply(table_a1_cont, 1, function(r) sprintf("| %s | %s | %s |", r[1], r[2], r[3]))
  ),
  "cache/tableA1_descriptives_continuous.md"
)
writeLines(
  c(
    "**Table A1b. Descriptive Statistics: Categorical Variables**", "",
    "| Variable | Category | N (%) |", "|---|---|---|",
    apply(table_a1_cat, 1, function(r) sprintf("| %s | %s | %s |", r[1], r[2], r[3]))
  ),
  "cache/tableA1_descriptives_categorical.md"
)

# ------------------------------------------------------------------------
# Table A2: Activity item factor loadings
# ------------------------------------------------------------------------
act_loadings <- unclass(prep$act_fa$loadings)
colnames(act_loadings) <- names(prep$act_factor_id)[match(seq_len(ncol(act_loadings)), prep$act_factor_id)]
act_loadings <- act_loadings[, c("arts", "leisure", "residual")]
colnames(act_loadings) <- c("Public Arts Participation", "Solitary Leisure", "Residual")

table_a2 <- data.frame(Item = prep$activity_items, round(act_loadings, 2), check.names = FALSE)
writeLines(
  c(
    "**Table A2. Factor Loadings for Culture and Leisure Activity Items**", "",
    paste0("| Item | ", paste(colnames(table_a2)[-1], collapse = " | "), " |"),
    paste0("|---|", paste(rep("---", ncol(table_a2) - 1), collapse = "|"), "|"),
    apply(table_a2, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  ),
  "cache/tableA2_activity_loadings.md"
)

# ------------------------------------------------------------------------
# Table A3: Weak- and strong-tie item factor loadings
# ------------------------------------------------------------------------
build_loading_table <- function(fit, items, suffix) {
  loadings <- unclass(fit$fa$loadings)
  colnames(loadings) <- names(fit$idx)[match(seq_len(ncol(loadings)), fit$idx)]
  loadings <- loadings[, c("variety", "liberal", "conservative")]
  colnames(loadings) <- c("Variety", "Liberal Composition", "Conservative Composition")
  data.frame(Item = gsub(suffix, "", items), round(loadings, 2), check.names = FALSE)
}

table_a3_weak   <- build_loading_table(prep$weak_fit, prep$network_weak_items, "_weak")
table_a3_strong <- build_loading_table(prep$strong_fit, prep$network_strong_items, "_strong")

write_loading_md <- function(tab, title, path) {
  writeLines(
    c(
      title, "",
      paste0("| Item | ", paste(colnames(tab)[-1], collapse = " | "), " |"),
      paste0("|---|", paste(rep("---", ncol(tab) - 1), collapse = "|"), "|"),
      apply(tab, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
    ),
    path
  )
}
write_loading_md(table_a3_weak, "**Table A3a. Factor Loadings for Weak-Tie Network Items**", "cache/tableA3_weak_tie_loadings.md")
write_loading_md(table_a3_strong, "**Table A3b. Factor Loadings for Strong-Tie Network Items**", "cache/tableA3_strong_tie_loadings.md")

# ------------------------------------------------------------------------
# Table A5: Network variety factor validation (correlation with group counts)
# ------------------------------------------------------------------------
val_vars <- c(
  "weak_variety", "weak_lib_comp", "weak_cons_comp",
  "strong_variety", "strong_lib_comp", "strong_cons_comp"
)
val_counts <- c(
  weak = "weak_variety_count1", weak2 = "weak_variety_count2",
  strong = "strong_variety_count1", strong2 = "strong_variety_count2"
)

cor_row <- function(v) {
  c(
    `>=1 (weak)` = round(cor(df[[v]], df$weak_variety_count1, use = "complete.obs"), 3),
    `>=2 (weak)` = round(cor(df[[v]], df$weak_variety_count2, use = "complete.obs"), 3),
    `>=1 (strong)` = round(cor(df[[v]], df$strong_variety_count1, use = "complete.obs"), 3),
    `>=2 (strong)` = round(cor(df[[v]], df$strong_variety_count2, use = "complete.obs"), 3)
  )
}
table_a5 <- data.frame(
  `Factor Score` = c(
    "Weak-Tie Variety", "Weak-Tie Liberal Composition", "Weak-Tie Conservative Composition",
    "Strong-Tie Variety", "Strong-Tie Liberal Composition", "Strong-Tie Conservative Composition"
  ),
  t(sapply(val_vars, cor_row)),
  check.names = FALSE
)
writeLines(
  c(
    "**Table A5. Correlation of Network Factor Scores with Counts of the Number of Groups Known**", "",
    paste0("| ", paste(colnames(table_a5), collapse = " | "), " |"),
    paste0("|---|", paste(rep("---", ncol(table_a5) - 1), collapse = "|"), "|"),
    apply(table_a5, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  ),
  "cache/tableA5_variety_validation.md"
)

message("Done. Tables saved to cache/. (Table A4 replaced by Plots/figA1_predictor_correlation_heatmap.png)")
