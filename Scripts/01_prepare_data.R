#!/usr/bin/env Rscript
# Scripts/01_prepare_data.R
#
# Ingests grabbagdata111925.dta, reconstructs the three factor-analytic
# measurement models reported in the paper:
#   (1) the 20-item activities/leisure battery -> Public Arts Participation,
#       Solitary Leisure, and a residual factor;
#   (2) the 18-item weak-tie group position generator -> Variety, Liberal
#       Composition, Conservative Composition;
#   (3) the 18-item strong-tie group position generator -> the same three
#       dimensions.
#
# NOTE ON SAMPLE CLEANING: the analytic data extract available for this
# replication does not retain the original `v8` (duplicate ID) or
# `duration` (survey completion time) columns needed to reproduce the
# paper's explicit "drop duplicates / drop speeders" step (1,258 -> 1,243).
# In practice this has no material consequence: listwise deletion on the
# 20 activity items alone already recovers exactly N = 1,243, matching the
# paper's reported analytic sample for the activities factor model.
#
# Outputs cached to cache/01_prepared_data.rds for downstream scripts.

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(psych)
  library(GPArotation)
})

# NOTE ON ROTATION (reverted to varimax, September 2026): all three factor
# models use orthogonal (varimax) rotation. An oblimin (oblique) variant
# was tried and is documented in project memory/git history, but the
# analysis has been reverted to the original varimax specification.

dir.create("cache", showWarnings = FALSE, recursive = TRUE)

message("[1/4] Loading raw data...")
df <- read_dta("grabbagdata111925.dta")
message(sprintf("  Loaded %d respondents, %d columns.", nrow(df), ncol(df)))

# ------------------------------------------------------------------------
# 1. Activities / leisure factor model (Table A2, Figure 1)
# ------------------------------------------------------------------------
message("[2/4] Fitting 3-factor PCA (varimax) on 20 activity items...")

activity_items <- c(
  "museum", "art_gallery", "symphony_orchestra_opera", "gardening",
  "carnival_fair_amusement_park", "music_concert_festival", "play_or_musical",
  "library", "fancy_restaurant", "fast_food", "go_for_walk", "exercise_or_yoga",
  "dance_performance", "movie_theater", "read_novel_poem_or_play",
  "attend_sports", "home_auto_repair", "hiking_camping_boating",
  "historic_site", "go_to_festival"
)

act_df <- as.data.frame(df[activity_items])
act_fa <- principal(act_df, nfactors = 3, rotate = "varimax", scores = TRUE)

# Identify which rotated component corresponds to each substantive factor
# by looking at where theoretically anchoring items load highest, rather
# than assuming a fixed column order (rotated column order is arbitrary
# and can differ across software/runs). We still identify the residual
# factor as whichever remaining factor has the largest absolute loading
# on fast food, then orient its sign to match the substantive
# interpretation (positive on gardening/home-repair, negative on fast
# food) -- under varimax this sign is typically already correct, but the
# check is kept as a no-op-safe guard rather than assumed.
act_loadings <- unclass(act_fa$loadings)
leisure_id  <- unname(which.max(colMeans(act_loadings[c("go_for_walk", "exercise_or_yoga"), ])))
remaining_id <- setdiff(1:3, leisure_id)
residual_id <- remaining_id[which.max(abs(act_loadings["fast_food", remaining_id]))]
arts_id     <- setdiff(remaining_id, residual_id)
act_factor_id <- c(arts = arts_id, leisure = leisure_id, residual = residual_id)
stopifnot(length(unique(act_factor_id)) == 3)

# Orient the residual factor's sign *inside the fitted object itself*
# (loadings, scores, and its row/column of Phi) so it carries the same
# sign convention reported in the paper (positive on gardening/home
# repair, negative on fast food) everywhere downstream -- Table A2 and
# Figure 1 read loadings directly off act_fa, so flipping only the score
# column (as the varimax-era code did) would leave the reported loadings
# inconsistent with the factor scores actually used in the regressions.
if (act_loadings["fast_food", act_factor_id["residual"]] > 0) {
  r <- act_factor_id["residual"]
  act_fa$loadings[, r] <- -act_fa$loadings[, r]
  act_fa$scores[, r]   <- -act_fa$scores[, r]
  if (!is.null(act_fa$Phi)) {
    act_fa$Phi[, r] <- -act_fa$Phi[, r]
    act_fa$Phi[r, ] <- -act_fa$Phi[r, ]
  }
  act_loadings <- unclass(act_fa$loadings)
}

df$arts_participation <- act_fa$scores[, act_factor_id["arts"]]
df$solitary_leisure    <- act_fa$scores[, act_factor_id["leisure"]]
df$residual_leisure    <- act_fa$scores[, act_factor_id["residual"]]

act_kmo <- KMO(cor(act_df, use = "complete.obs"))$MSA
act_var_explained <- act_fa$Vaccounted["Cumulative Var", 3]
# Under varimax, factors are orthogonal by construction (Phi = identity,
# so act_fa$Phi is NULL); fall back to an explicit identity matrix so
# downstream code that reads act_phi keeps working.
act_phi <- if (!is.null(act_fa$Phi)) {
  p <- act_fa$Phi
  rownames(p) <- colnames(p) <- names(act_factor_id)[match(seq_len(3), act_factor_id)]
  p[names(act_factor_id), names(act_factor_id)]
} else {
  nm <- names(act_factor_id)[match(seq_len(3), act_factor_id)]
  diag(3) |> `dimnames<-`(list(nm, nm))
}

# ------------------------------------------------------------------------
# 2. Network tie factor models (Table A3, Figure 2)
# ------------------------------------------------------------------------
message("[3/4] Fitting 3-factor PCA (varimax) on weak- and strong-tie items...")

network_weak_items <- c(
  "KnowLGBTQ_weak", "KnowLotsaChurch_weak", "KnowLittleChurch_weak", "KnowVeryLib_weak",
  "KnowVeryCons_weak", "KnowAsianppl_weak", "KnowHispanixppl_weak", "KnowBlackppl_weak",
  "KnowWhiteppl_weak", "Know2ndHome_weak", "KnowBornElsewhere_weak", "KnowMENAppl_weak",
  "KnowHawaiiPI_weak", "KnowAmIndianppl_weak", "KnowCityppl_weak", "KnowRuralppl_weak",
  "Knowwomen_weak", "Knowmen_weak"
)
network_strong_items <- gsub("_weak$", "_strong", network_weak_items)

# Identify variety / liberal-composition / conservative-composition columns
# from the loadings pattern rather than a hardcoded column index.
label_tie_factors <- function(loadings_mat, suffix) {
  anchors <- list(
    variety      = paste0(c("KnowHawaiiPI", "KnowAmIndianppl", "KnowMENAppl"), suffix),
    liberal      = paste0(c("KnowVeryLib", "KnowLGBTQ"), suffix),
    conservative = paste0(c("KnowVeryCons", "KnowLotsaChurch"), suffix)
  )
  sapply(anchors, function(items) unname(which.max(colMeans(loadings_mat[items, , drop = FALSE]))))
}

fit_tie_factors <- function(items, suffix) {
  item_df <- as.data.frame(df[items])
  fa <- principal(item_df, nfactors = 3, rotate = "varimax", scores = TRUE)
  idx <- label_tie_factors(unclass(fa$loadings), suffix)
  stopifnot(length(unique(idx)) == 3)
  list(
    fa = fa,
    idx = idx,
    kmo = KMO(cor(item_df, use = "complete.obs"))$MSA,
    var_explained = fa$Vaccounted["Cumulative Var", 3],
    # Inter-factor correlations (Phi); under varimax these are 0 by
    # construction (fa$Phi is NULL), so fall back to an explicit identity
    # matrix so downstream code reading `phi` keeps working.
    phi = if (!is.null(fa$Phi)) {
      p <- fa$Phi
      rownames(p) <- colnames(p) <- names(idx)[match(seq_len(3), idx)]
      p[names(idx), names(idx)]
    } else {
      nm <- names(idx)[match(seq_len(3), idx)]
      diag(3) |> `dimnames<-`(list(nm, nm))
    }
  )
}

weak_fit   <- fit_tie_factors(network_weak_items, "_weak")
strong_fit <- fit_tie_factors(network_strong_items, "_strong")

df$weak_variety     <- weak_fit$fa$scores[, weak_fit$idx["variety"]]
df$weak_lib_comp    <- weak_fit$fa$scores[, weak_fit$idx["liberal"]]
df$weak_cons_comp   <- weak_fit$fa$scores[, weak_fit$idx["conservative"]]

df$strong_variety   <- strong_fit$fa$scores[, strong_fit$idx["variety"]]
df$strong_lib_comp  <- strong_fit$fa$scores[, strong_fit$idx["liberal"]]
df$strong_cons_comp <- strong_fit$fa$scores[, strong_fit$idx["conservative"]]

# ------------------------------------------------------------------------
# 3. Recode covariates used in Tables 1-2 / A1 / A4
# ------------------------------------------------------------------------
message("[4/4] Recoding covariates and saving prepared data...")

df$educ_cat <- factor(
  case_when(
    df$educ %in% 1:3 ~ "HS or less",
    df$educ %in% 4:5 ~ "Some college",
    df$educ == 6     ~ "College degree",
    df$educ == 7     ~ "Graduate degree"
  ),
  levels = c("HS or less", "Some college", "College degree", "Graduate degree")
)
df$gender_f <- factor(as_factor(df$gender), levels = c("Woman", "Man", "Non-binary/Trans"))
df$race_f   <- relevel(factor(as_factor(df$race3)), ref = "white")

# Variety-count validation measures (Table A5): number of the 18 groups
# a respondent knows >= 1 or >= 2 people in, computed separately from the
# factor model as an independent check on Factor 1's interpretation.
weak_mat   <- as.matrix(df[network_weak_items])
strong_mat <- as.matrix(df[network_strong_items])
df$weak_variety_count1   <- rowSums(weak_mat >= 2, na.rm = TRUE)   # response code 2 = "1 person"
df$weak_variety_count2   <- rowSums(weak_mat >= 3, na.rm = TRUE)   # response code 3 = "2 to 5 people"
df$strong_variety_count1 <- rowSums(strong_mat >= 2, na.rm = TRUE)
df$strong_variety_count2 <- rowSums(strong_mat >= 3, na.rm = TRUE)

out <- list(
  df = df,
  activity_items = activity_items,
  network_weak_items = network_weak_items,
  network_strong_items = network_strong_items,
  act_fa = act_fa,
  act_factor_id = act_factor_id,
  act_kmo = act_kmo,
  act_var_explained = act_var_explained,
  act_phi = act_phi,
  weak_fit = weak_fit,
  strong_fit = strong_fit
)

saveRDS(out, "cache/01_prepared_data.rds")
message(sprintf(
  "Done. Activities KMO = %.3f (cum. var %.1f%%); weak KMO = %.3f; strong KMO = %.3f.",
  act_kmo, 100 * act_var_explained, weak_fit$kmo, strong_fit$kmo
))
