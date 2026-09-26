#!/usr/bin/env Rscript
# Scripts/03_generate_figures.R
#
# Generates the three publication figures at 6.5 in width, 300 DPI:
#   Figure 1: Heat map of factor loadings for activities and practices
#   Figure 2: Heat map of factor loadings for weak and strong ties
#   Figure 3: Weak- and strong-tie network variety on arts participation
#             (predicted margins from Table 1, Model 2)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(estimatr)
})

dir.create("Plots", showWarnings = FALSE, recursive = TRUE)

prep <- readRDS("cache/01_prepared_data.rds")
mods <- readRDS("cache/02_models.rds")

# ------------------------------------------------------------------------
# Figure 1: Activity/leisure item loadings heat map
# ------------------------------------------------------------------------
message("[1/3] Building Figure 1 (activity loadings heat map)...")

act_loadings <- unclass(prep$act_fa$loadings)
colnames(act_loadings) <- names(prep$act_factor_id)[match(seq_len(ncol(act_loadings)), prep$act_factor_id)]
act_long <- as.data.frame(act_loadings) |>
  mutate(item = rownames(act_loadings)) |>
  pivot_longer(-item, names_to = "factor", values_to = "loading") |>
  mutate(
    item = factor(item, levels = rev(prep$activity_items)),
    factor = factor(factor,
      levels = c("arts", "leisure", "residual"),
      labels = c("Public Arts\nParticipation", "Solitary\nLeisure", "Residual")
    )
  )

fig1 <- ggplot(act_long, aes(x = factor, y = item, fill = loading)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", loading)), size = 2.6) +
  scale_fill_gradient2(low = "#D55E00", mid = "white", high = "#0072B2", midpoint = 0, name = "Loading") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(face = "bold"))

ggsave("Plots/fig1_activity_loadings_heatmap.png", fig1, width = 6.5, height = 7.5, dpi = 300)

# ------------------------------------------------------------------------
# Figure 2: Weak- and strong-tie loadings heat map
# ------------------------------------------------------------------------
message("[2/3] Building Figure 2 (network tie loadings heat map)...")

build_tie_long <- function(fit, items, suffix, tie_label) {
  loadings <- unclass(fit$fa$loadings)
  colnames(loadings) <- names(fit$idx)[match(seq_len(ncol(loadings)), fit$idx)]
  as.data.frame(loadings) |>
    mutate(item = gsub(suffix, "", rownames(loadings))) |>
    pivot_longer(-item, names_to = "factor", values_to = "loading") |>
    mutate(tie = tie_label)
}

tie_long <- bind_rows(
  build_tie_long(prep$weak_fit, prep$network_weak_items, "_weak", "Weak Ties"),
  build_tie_long(prep$strong_fit, prep$network_strong_items, "_strong", "Strong Ties")
) |>
  mutate(
    item = gsub("^Know", "", item),
    factor = factor(factor,
      levels = c("variety", "liberal", "conservative"),
      labels = c("Variety", "Liberal\nComposition", "Conservative\nComposition")
    ),
    tie = factor(tie, levels = c("Weak Ties", "Strong Ties"))
  )

fig2 <- ggplot(tie_long, aes(x = factor, y = item, fill = loading)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", loading)), size = 2.4) +
  scale_fill_gradient2(low = "#D55E00", mid = "white", high = "#0072B2", midpoint = 0, name = "Loading") +
  facet_wrap(~tie) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

ggsave("Plots/fig2_network_loadings_heatmap.png", fig2, width = 6.5, height = 6.5, dpi = 300)

# ------------------------------------------------------------------------
# Figure 3: Predicted arts participation by weak/strong-tie variety
# ------------------------------------------------------------------------
message("[3/3] Building Figure 3 (predicted margins plot)...")

m2 <- mods$arts_models$m2
mdat <- mods$mdat

grid_range <- seq(-2.5, 2.5, length.out = 100)

make_pred_grid <- function(varying_var, other_var) {
  base <- mdat[1, , drop = FALSE]
  base$gender_f   <- factor("Woman", levels = levels(mdat$gender_f))
  base$educ_cat   <- factor("College degree", levels = levels(mdat$educ_cat))
  base$race_f     <- factor("white", levels = levels(mdat$race_f))
  base$child_arts <- mean(mdat$child_arts)
  base$income     <- mean(mdat$income)
  base$age2       <- mean(mdat$age2)
  base$poli       <- mean(mdat$poli)

  newdat <- base[rep(1, length(grid_range)), ]
  newdat[[varying_var]] <- grid_range
  newdat[[other_var]] <- 0
  pred <- predict(m2, newdata = newdat, se.fit = TRUE)
  data.frame(
    x = grid_range,
    fit = pred$fit,
    lo = pred$fit - 1.96 * pred$se.fit,
    hi = pred$fit + 1.96 * pred$se.fit,
    tie = varying_var
  )
}

fig3_dat <- bind_rows(
  make_pred_grid("strong_variety", "weak_variety"),
  make_pred_grid("weak_variety", "strong_variety")
) |>
  mutate(tie = factor(tie, levels = c("strong_variety", "weak_variety"),
                       labels = c("Strong-Tie Variety", "Weak-Tie Variety")))

fig3 <- ggplot(fig3_dat, aes(x = x, y = fit, linetype = tie)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  scale_linetype_manual(values = c("Strong-Tie Variety" = "solid", "Weak-Tie Variety" = "dashed"), name = NULL) +
  labs(
    x = "Network Variety Factor Score",
    y = "Predicted Public Arts Participation Score"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("Plots/fig3_variety_margins_plot.png", fig3, width = 6.5, height = 5, dpi = 300)

# ------------------------------------------------------------------------
# Figure A1: Correlation heat map of continuous/ordinal predictors
# (replaces the Table A4 correlation matrix with a heat map in the style
# of Plots/varcortrue.png from the childress-lizardo-aesthetic-politics
# project: symmetric tile matrix, diagonal omitted, purple-white-blue
# diverging fill, angled x-axis labels.)
# ------------------------------------------------------------------------
message("[4/4] Building Figure A1 (predictor correlation heat map)...")

COLOR_BLUE   <- "#0077BB"
COLOR_PURPLE <- "#882255"

corr_vars <- c(
  "age2", "income", "educ", "child_arts", "poli",
  "weak_variety", "weak_lib_comp", "weak_cons_comp",
  "strong_variety", "strong_lib_comp", "strong_cons_comp"
)
corr_labels <- c(
  age2 = "Age", income = "Income", educ = "Education", child_arts = "Childhood Arts Exposure",
  poli = "Political Ideology", weak_variety = "Weak-Tie Variety", weak_lib_comp = "Weak-Tie Liberal Comp.",
  weak_cons_comp = "Weak-Tie Conservative Comp.", strong_variety = "Strong-Tie Variety",
  strong_lib_comp = "Strong-Tie Liberal Comp.", strong_cons_comp = "Strong-Tie Conservative Comp."
)

corr_data <- prep$df[corr_vars]
cor_mat <- cor(corr_data, use = "pairwise.complete.obs")
colnames(cor_mat) <- corr_labels[colnames(cor_mat)]
rownames(cor_mat) <- corr_labels[rownames(cor_mat)]
diag(cor_mat) <- NA

cor_df <- as.data.frame(as.table(cor_mat))
colnames(cor_df) <- c("Var1", "Var2", "Correlation")
cor_df$Var1 <- factor(cor_df$Var1, levels = rev(corr_labels))
cor_df$Var2 <- factor(cor_df$Var2, levels = corr_labels)
cor_df$Label <- ifelse(is.na(cor_df$Correlation), "", sprintf("%.2f", cor_df$Correlation))

fig_a1 <- ggplot(cor_df, aes(x = Var2, y = Var1, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.35) +
  geom_text(aes(label = Label, color = abs(Correlation) > 0.35 & !is.na(Correlation)),
            size = 2.6, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "gray15")) +
  scale_fill_gradient2(
    low = COLOR_PURPLE, mid = "white", high = COLOR_BLUE, midpoint = 0,
    limits = c(-1, 1), na.value = "gray96", name = "Pearson r"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold", size = 8.5),
    axis.text.y = element_text(face = "bold", size = 8.5),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )
# No title/subtitle: the caption already lives in the Google Doc paragraph
# above this figure, so a baked-in ggplot title would just duplicate it.

ggsave("Plots/figA1_predictor_correlation_heatmap.png", fig_a1, width = 8.0, height = 6.1, dpi = 300)

message("Done. Figures saved to Plots/.")
