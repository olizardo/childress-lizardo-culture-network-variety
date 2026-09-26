#!/usr/bin/env Rscript
# Scripts/00_extract_minimal_data.R
#
# One-off (re-runnable) utility that extracts, from the full raw survey
# export `grabbagdata111925_full.dta` (gitignored, kept only on the
# maintainers' local machines), the minimal set of columns actually read by
# the analytic pipeline (Scripts/01-05), and writes them to
# `grabbagdata111925.dta` -- the file committed to this repository and used
# by every downstream script.
#
# This lets the repository host a reproducibility-sufficient extract of the
# data on GitHub without redistributing the full raw survey export (which
# may contain additional identifying or unused variables).
#
# Usage: Rscript Scripts/00_extract_minimal_data.R

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

full_path <- "grabbagdata111925_full.dta"
minimal_path <- "grabbagdata111925.dta"

if (!file.exists(full_path)) {
  stop(sprintf(
    "'%s' not found. This script must be run on a machine that holds the full raw data export (kept out of git via .gitignore).",
    full_path
  ))
}

message("Loading full raw data export...")
raw <- read_dta(full_path)
message(sprintf("  Loaded %d respondents, %d columns.", nrow(raw), ncol(raw)))

# --- Item batteries used to construct the factor-analytic measurement models ---
activity_items <- c(
  "museum", "art_gallery", "symphony_orchestra_opera", "gardening",
  "carnival_fair_amusement_park", "music_concert_festival", "play_or_musical",
  "library", "fancy_restaurant", "fast_food", "go_for_walk", "exercise_or_yoga",
  "dance_performance", "movie_theater", "read_novel_poem_or_play",
  "attend_sports", "home_auto_repair", "hiking_camping_boating",
  "historic_site", "go_to_festival"
)
network_weak_items <- c(
  "KnowLGBTQ_weak", "KnowLotsaChurch_weak", "KnowLittleChurch_weak", "KnowVeryLib_weak",
  "KnowVeryCons_weak", "KnowAsianppl_weak", "KnowHispanixppl_weak", "KnowBlackppl_weak",
  "KnowWhiteppl_weak", "Know2ndHome_weak", "KnowBornElsewhere_weak", "KnowMENAppl_weak",
  "KnowHawaiiPI_weak", "KnowAmIndianppl_weak", "KnowCityppl_weak", "KnowRuralppl_weak",
  "Knowwomen_weak", "Knowmen_weak"
)
network_strong_items <- gsub("_weak$", "_strong", network_weak_items)

# --- Covariates used in the regression models, descriptives, and correlation heat map ---
covariate_items <- c("educ", "gender", "race3", "child_arts", "income", "age2", "poli")

keep_cols <- c(activity_items, network_weak_items, network_strong_items, covariate_items)

missing_cols <- setdiff(keep_cols, names(raw))
if (length(missing_cols) > 0) {
  stop("Full raw export is missing expected columns: ", paste(missing_cols, collapse = ", "))
}

minimal <- raw %>% select(all_of(keep_cols))
message(sprintf("Extracted minimal set: %d columns (of %d in full export).", ncol(minimal), ncol(raw)))

write_dta(minimal, minimal_path)
message(sprintf("Wrote '%s' (%d rows x %d cols).", minimal_path, nrow(minimal), ncol(minimal)))
