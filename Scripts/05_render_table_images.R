#!/usr/bin/env Rscript
# Scripts/05_render_table_images.R
#
# Renders publication tables as flat PNG images (matching the plain,
# booktabs-style screenshots already embedded in the live Google Doc:
# monospaced-alignment numeric columns, thin top/header/bottom rules,
# no cell shading) so they can be swapped in via the surgical OpenXML
# image-replacement pipeline in Scripts/sync_manuscript.py.
#
# Outputs to Plots/ (kept alongside the ggplot figures since these are
# themselves just images, not native Word tables in the source document).

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(haven)
  library(estimatr)
})

dir.create("Plots", showWarnings = FALSE, recursive = TRUE)

prep <- readRDS("cache/01_prepared_data.rds")
mods <- readRDS("cache/02_models.rds")
df <- prep$df
mdat <- mods$mdat

stars <- function(p) ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
fmt_b  <- function(b, p) sprintf("%.3f%s", b, stars(p))
fmt_se <- function(se) sprintf("(%.3f)", se)

# ------------------------------------------------------------------------
# Generic flat-table renderer: draws row labels left-aligned, numeric
# columns right-aligned, with a top rule, a rule under the header, and a
# bottom rule -- no gridlines, no shading.
# ------------------------------------------------------------------------
render_table_png <- function(header, rows, path, title = NULL, note = NULL,
                              col_x = NULL, label_x = 0, width = 7, height = NULL,
                              base_size = 11, label_hjust = 0) {
  n_col <- length(header)
  if (is.null(col_x)) col_x <- seq(1, n_col - 1) / (n_col - 1) * 0.78 + 0.20
  n_row <- length(rows)
  if (is.null(height)) height <- 0.9 + 0.28 * n_row + (if (!is.null(title)) 0.35 else 0) + (if (!is.null(note)) 0.3 else 0)

  y_header <- n_row + 1.3
  y_top    <- n_row + 1.8
  y_rule2  <- n_row + 0.8
  y_bottom <- 0.2
  y_note   <- -0.4
  # Only reserve vertical space for a title row when a title is actually
  # supplied, so tables without one (the caption already lives in the
  # Google Doc, not the image) don't render with a blank gap up top.
  y_title  <- y_top + 0.5
  y_max    <- if (!is.null(title)) y_title + 0.3 else y_top + 0.3

  txt <- list()
  txt[[length(txt) + 1]] <- data.frame(x = label_x, y = y_header, label = header[1], hjust = label_hjust, bold = TRUE)
  for (j in 2:n_col) txt[[length(txt) + 1]] <- data.frame(x = col_x[j - 1], y = y_header, label = header[j], hjust = 0.5, bold = TRUE)

  for (i in seq_len(n_row)) {
    r <- rows[[i]]
    y <- n_row - i + 0.8
    txt[[length(txt) + 1]] <- data.frame(x = label_x, y = y, label = r[1], hjust = label_hjust, bold = isTRUE(attr(r, "bold")))
    for (j in 2:n_col) {
      txt[[length(txt) + 1]] <- data.frame(x = col_x[j - 1], y = y, label = ifelse(is.na(r[j]), "", r[j]), hjust = 0.5, bold = FALSE)
    }
  }
  txt_df <- bind_rows(txt)

  p <- ggplot() +
    geom_text(data = txt_df, aes(x = x, y = y, label = label, hjust = hjust, fontface = ifelse(bold, "bold", "plain")),
               size = base_size / 2.9, family = "serif") +
    geom_segment(aes(x = -0.02, xend = 1.05, y = y_top, yend = y_top), linewidth = 0.6) +
    geom_segment(aes(x = -0.02, xend = 1.05, y = y_rule2, yend = y_rule2), linewidth = 0.4) +
    geom_segment(aes(x = -0.02, xend = 1.05, y = y_bottom, yend = y_bottom), linewidth = 0.6) +
    xlim(-0.05, 1.1) +
    ylim(y_note - 0.3, y_max) +
    theme_void()

  if (!is.null(title)) {
    p <- p + annotate("text", x = label_x, y = y_title, label = title, hjust = label_hjust, size = base_size / 2.7, family = "serif")
  }
  if (!is.null(note)) {
    p <- p + annotate("text", x = -0.02, y = y_note, label = note, hjust = 0, size = base_size / 3.4, family = "serif")
  }

  ggsave(path, p, width = width, height = height, dpi = 300, bg = "white")
}

# ------------------------------------------------------------------------
# Table 1 / Table 2: regression models
# ------------------------------------------------------------------------
build_reg_rows <- function(models) {
  s <- lapply(models, function(m) summary(m)$coefficients)
  add_row <- function(label, term, bold = FALSE) {
    vals <- sapply(s, function(x) if (term %in% rownames(x)) fmt_b(x[term, 1], x[term, 4]) else NA)
    se_vals <- sapply(s, function(x) if (term %in% rownames(x)) fmt_se(x[term, 2]) else NA)
    list(structure(c(label, vals), bold = bold), structure(c("", se_vals), bold = FALSE))
  }
  rows <- list()
  rows <- c(rows, list(structure(c("Network Variety", NA, NA, NA, NA), bold = TRUE)))
  rows <- c(rows, add_row("Weak Tie Variety", "weak_variety"))
  rows <- c(rows, add_row("Strong Tie Variety", "strong_variety"))
  rows <- c(rows, list(structure(c("", NA, NA, NA, NA), bold = FALSE)))
  rows <- c(rows, list(structure(c("Network Composition", NA, NA, NA, NA), bold = TRUE)))
  rows <- c(rows, add_row("Weak Tie Liberal", "weak_lib_comp"))
  rows <- c(rows, add_row("Strong Tie Liberal", "strong_lib_comp"))
  rows <- c(rows, add_row("Weak Tie Conservative", "weak_cons_comp"))
  rows <- c(rows, add_row("Strong Tie Conservative", "strong_cons_comp"))
  rows <- c(rows, list(structure(c("", NA, NA, NA, NA), bold = FALSE)))
  rows <- c(rows, list(structure(c("Controls", NA, NA, NA, NA), bold = TRUE)))
  rows <- c(rows, add_row("Education", "educ_catCollege degree"))
  rows <- c(rows, add_row("Childhood Arts", "child_arts"))
  rows <- c(rows, add_row("Income", "income"))
  rows <- c(rows, add_row("Age", "age2"))
  rows <- c(rows, add_row("Woman (Man = Ref)", "gender_fMan"))
  rows <- c(rows, add_row("Black (White = Ref)", "race_fblack"))
  rows <- c(rows, add_row("Hispanic/Latine", "race_flatine"))
  rows <- c(rows, add_row("AAPI", "race_fAAPI"))
  rows <- c(rows, add_row("Political Ideology", "poli"))
  rows <- c(rows, add_row("Constant", "(Intercept)"))
  n_rows <- sapply(models, function(m) length(residuals(m)))
  rows <- c(rows, list(structure(c("N", sprintf("%d", n_rows)), bold = FALSE)))
  rows
}

render_table_png(
  header = c("", "(1)", "(2)", "(3)", "(4)"),
  rows = build_reg_rows(mods$arts_models),
  path = "Plots/table1_arts_participation.png",
  note = "Note: Robust (HC1) standard errors in parentheses. * p < .05, ** p < .01, *** p < .001",
  label_x = -0.02, label_hjust = 0, width = 6.8, height = 9.8
)

render_table_png(
  header = c("", "(1)", "(2)", "(3)", "(4)"),
  rows = build_reg_rows(mods$leisure_models),
  path = "Plots/table2_solitary_leisure.png",
  note = "Note: Robust (HC1) standard errors in parentheses. * p < .05, ** p < .01, *** p < .001",
  label_x = -0.02, label_hjust = 0, width = 6.8, height = 9.8
)

# ------------------------------------------------------------------------
# Table 3: residual/DIY-practical leisure factor (discriminant-validity
# check) -- same four-model hierarchical sequence and row layout as
# Tables 1-2, so the composition-terms block can be visually compared
# across all three outcomes.
# ------------------------------------------------------------------------
render_table_png(
  header = c("", "(1)", "(2)", "(3)", "(4)"),
  rows = build_reg_rows(mods$residual_models),
  path = "Plots/table3_residual_leisure.png",
  note = "Note: Robust (HC1) standard errors in parentheses. * p < .05, ** p < .01, *** p < .001",
  label_x = -0.02, label_hjust = 0, width = 6.8, height = 9.8
)

# ------------------------------------------------------------------------
# Table A1: Descriptive statistics (complete-case modeling sample)
# ------------------------------------------------------------------------
cont_vars <- c(
  "arts_participation" = "Arts Participation", "solitary_leisure" = "Solitary Leisure",
  "residual_leisure" = "DIY Practical", "weak_variety" = "Weak Tie Variety",
  "strong_variety" = "Strong Tie Variety", "weak_lib_comp" = "Weak Ties Liberal",
  "strong_lib_comp" = "Strong Ties Liberal", "weak_cons_comp" = "Weak Ties Conservative",
  "strong_cons_comp" = "Strong Ties Conservative", "educ" = "Education",
  "child_arts" = "Childhood Arts", "income" = "Income", "poli" = "Political Ideology", "age2" = "Age"
)
cont_rows <- lapply(names(cont_vars), function(v) {
  x <- mdat[[v]]
  structure(c(cont_vars[v], sprintf("%.2f", mean(x)), sprintf("(%.2f)", sd(x)),
              sprintf("%.2f", min(x)), sprintf("%.2f", max(x)), "", sprintf("%d", length(x))), bold = FALSE)
})

cat_specs <- list(
  list(var = "gender_f", label_fn = as.character),
  list(var = "race_f", label_fn = as.character)
)
cat_rows <- list()
for (spec in cat_specs) {
  tab <- table(mdat[[spec$var]])
  pct <- round(100 * tab / sum(tab), 1)
  cat_rows <- c(cat_rows, list(structure(c("", NA, NA, NA, NA, NA, NA), bold = FALSE)))
  for (lv in names(tab)) {
    cat_rows <- c(cat_rows, list(structure(c(lv, "", "", "", "", sprintf("%.1f", pct[lv]), sprintf("%d", tab[lv])), bold = FALSE)))
  }
}

render_table_png(
  header = c("", "Mean", "SD", "Min", "Max", "PCT", "N"),
  rows = c(cont_rows, cat_rows),
  path = "Plots/tableA1_descriptives.png",
  note = sprintf("Note: Complete-case modeling sample, N = %d.", nrow(mdat)),
  col_x = c(0.44, 0.55, 0.64, 0.73, 0.83, 0.94),
  label_x = -0.02, label_hjust = 0, width = 7.8, height = 8.5
)

# ------------------------------------------------------------------------
# Table A2: Activity item factor loadings (single table, 3 factor columns)
# ------------------------------------------------------------------------
act_loadings <- unclass(prep$act_fa$loadings)
colnames(act_loadings) <- names(prep$act_factor_id)[match(seq_len(ncol(act_loadings)), prep$act_factor_id)]
act_loadings <- act_loadings[, c("arts", "leisure", "residual")]
act_item_labels <- c(
  museum = "Museum", art_gallery = "Art Gallery", symphony_orchestra_opera = "Symphony/Opera",
  gardening = "Gardening", carnival_fair_amusement_park = "Carnival/Fair", music_concert_festival = "Music Concert",
  play_or_musical = "Play/Musical", library = "Library", fancy_restaurant = "Fancy Restaurant",
  fast_food = "Fast Food", go_for_walk = "Walk", exercise_or_yoga = "Gym/Yoga",
  dance_performance = "Dance/Ballet", movie_theater = "Movie Theater", read_novel_poem_or_play = "Read Fiction",
  attend_sports = "Attend Sports", home_auto_repair = "Home/Auto Repair", hiking_camping_boating = "Hiking/Camping",
  historic_site = "Historic Site", go_to_festival = "Cultural Festival"
)
rows_a2 <- lapply(prep$activity_items, function(it) {
  structure(c(act_item_labels[it], sprintf("%.3f", act_loadings[it, ])), bold = FALSE)
})
render_table_png(
  header = c("", "Arts", "Leisure", "Residual"),
  rows = rows_a2,
  path = "Plots/tableA2_activity_loadings.png",
  label_x = -0.02, label_hjust = 0, width = 6.0, height = 7.8
)

# ------------------------------------------------------------------------
# Table A3: Weak- and strong-tie item factor loadings (combined table)
# ------------------------------------------------------------------------
tie_item_labels <- c(
  KnowLGBTQ = "LGBTQ", KnowLotsaChurch = "Lots of Church", KnowLittleChurch = "No Church",
  KnowVeryLib = "Very Liberal", KnowVeryCons = "Very Conservative", KnowAsianppl = "Asian",
  KnowHispanixppl = "Hispanic", KnowBlackppl = "Black", KnowWhiteppl = "White",
  Know2ndHome = "Owns 2nd Home", KnowBornElsewhere = "Born Elsewhere", KnowMENAppl = "MENA",
  KnowHawaiiPI = "Hawaiian/PI", KnowAmIndianppl = "American Indian", KnowCityppl = "Live in City",
  KnowRuralppl = "Live in Rural", Knowwomen = "Women", Knowmen = "Men"
)
get_tie_loadings <- function(fit, items, suffix) {
  loadings <- unclass(fit$fa$loadings)
  colnames(loadings) <- names(fit$idx)[match(seq_len(ncol(loadings)), fit$idx)]
  loadings <- loadings[, c("variety", "liberal", "conservative")]
  rownames(loadings) <- gsub(suffix, "", items)
  loadings
}
weak_l   <- get_tie_loadings(prep$weak_fit, prep$network_weak_items, "_weak")
strong_l <- get_tie_loadings(prep$strong_fit, prep$network_strong_items, "_strong")
item_key <- gsub("_weak$", "", prep$network_weak_items)

rows_a3 <- lapply(item_key, function(it) {
  structure(c(
    tie_item_labels[it],
    sprintf("%.3f", weak_l[it, ]), sprintf("%.3f", strong_l[it, ])
  ), bold = FALSE)
})
render_table_png(
  header = c("", "Weak: Var.", "Weak: Lib.", "Weak: Cons.", "Strong: Var.", "Strong: Lib.", "Strong: Cons."),
  rows = rows_a3,
  path = "Plots/tableA3_tie_loadings.png",
  col_x = c(0.30, 0.42, 0.54, 0.68, 0.80, 0.93),
  label_x = -0.02, label_hjust = 0, width = 8.5, height = 7.8, base_size = 9.5
)

# ------------------------------------------------------------------------
# Table A5: Network variety factor validation (two stacked panels)
# ------------------------------------------------------------------------
build_validation_panel <- function(count_var, variety_var, lib_var, cons_var, labels) {
  vars <- c(count_var, variety_var, lib_var, cons_var)
  m <- df[vars]
  ct <- sapply(vars, function(v1) sapply(vars, function(v2) cor.test(m[[v1]], m[[v2]])))
  n_v <- length(vars)
  rows <- list()
  for (i in seq_len(n_v)) {
    r_vals <- character(n_v)
    p_vals <- character(n_v)
    for (j in seq_len(n_v)) {
      if (j > i) { r_vals[j] <- NA; p_vals[j] <- NA; next }
      ct <- cor.test(m[[vars[i]]], m[[vars[j]]])
      r_vals[j] <- sprintf("%.3f%s", unname(ct$estimate), stars(ct$p.value))
      p_vals[j] <- if (i == j) "" else sprintf("(%.3f)", ct$p.value)
    }
    rows[[length(rows) + 1]] <- structure(c(sprintf("(%d) %s", i, labels[i]), r_vals), bold = FALSE)
    rows[[length(rows) + 1]] <- structure(c("", p_vals), bold = FALSE)
  }
  rows
}

weak_panel <- build_validation_panel(
  "weak_variety_count1", "weak_variety", "weak_lib_comp", "weak_cons_comp",
  c("Weak Tie Variety Count", "Weak Tie Variety", "Weak Ties Liberal", "Weak Ties Conservative")
)
strong_panel <- build_validation_panel(
  "strong_variety_count1", "strong_variety", "strong_lib_comp", "strong_cons_comp",
  c("Strong Tie Variety Count", "Strong Tie Variety", "Strong Ties Liberal", "Strong Ties Conservative")
)

# Combined single image (weak-tie panel stacked above strong-tie panel)
# so the replacement matches the original document's one-image layout.
rows_a5_combined <- c(
  list(structure(c("Weak Ties", NA, NA, NA, NA), bold = TRUE)),
  weak_panel,
  list(structure(c("", NA, NA, NA, NA), bold = FALSE)),
  list(structure(c("Strong Ties", NA, NA, NA, NA), bold = TRUE)),
  strong_panel
)
render_table_png(
  header = c("", "(1)", "(2)", "(3)", "(4)"),
  rows = rows_a5_combined,
  path = "Plots/tableA5_variety_validation.png",
  note = "*** p < .01, ** p < .05, * p < .1",
  col_x = c(0.62, 0.75, 0.87, 0.98),
  label_x = -0.02, label_hjust = 0, width = 7.6, height = 6.5
)

message("Done. Table images saved to Plots/.")
