#!/usr/bin/env Rscript
# Scripts/audit_manuscript_stats.R
#
# Recomputes every quantitative claim tracked below directly from the
# modeling cache (cache/01_prepared_data.rds, cache/02_models.rds) and
# diffs it against the number actually printed in the live manuscript,
# so that future changes to the data, factor models, or regression
# specifications get caught automatically instead of requiring another
# manual audit (see AGENTS.md Sections 5-7, which document the manual
# audit this script is meant to replace/automate going forward).
#
# Usage:
#   Rscript Scripts/audit_manuscript_stats.R              # use cached manuscript text if present, else pull
#   Rscript Scripts/audit_manuscript_stats.R --pull        # force a fresh download from Google Drive
#   Rscript Scripts/audit_manuscript_stats.R --local x.docx # audit a local docx instead of Google Drive
#
# Exit status: 1 if any MISMATCH is found (wire this into a pre-push
# hook or CI step); 0 otherwise. A NOT_FOUND result (the manuscript no
# longer states a number this script looks for -- e.g. because a
# sentence was reworded or removed) is reported but does not itself
# fail the run, since a missing claim is not a numeric error.
#
# --- Adding a new check --------------------------------------------
# Append a function to `checks` below with signature
# function(txt_flat, prep, mods) that returns a data.frame with columns
# id, description, reported, actual, tol, status, note (use the
# `num_row()` / `lt_row()` / `not_found_row()` helpers to build rows).
# `txt_flat` is the manuscript text with all whitespace/newlines
# collapsed to single spaces, so regexes don't need to account for
# arbitrary line wraps.
# ---------------------------------------------------------------------

suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(estimatr))

args <- commandArgs(trailingOnly = TRUE)
force_pull <- "--pull" %in% args
local_idx <- which(args == "--local")
local_path <- if (length(local_idx) && length(args) > local_idx[1]) args[local_idx[1] + 1] else NA

DOC_ID <- "1llEbgZ8lZK7pMy3IjZFbLNoek5CK-S47MhNw91-Sa4A"
TEXT_CACHE <- "cache/_manuscript_text_cache.txt"

# ---------------------------------------------------------------------
# 1. Fetch manuscript text (Drive download -> pandoc -> plain text)
# ---------------------------------------------------------------------
convert_and_read <- function(docx_path) {
  tmp_txt <- tempfile(fileext = ".txt")
  status <- system2("pandoc", c("-t", "plain", shQuote(docx_path), "-o", shQuote(tmp_txt)))
  if (status != 0) stop("pandoc conversion failed for ", docx_path)
  txt <- paste(readLines(tmp_txt, warn = FALSE), collapse = "\n")
  unlink(tmp_txt)
  txt
}

get_manuscript_text <- function() {
  if (!is.na(local_path)) {
    message("Reading local docx: ", local_path)
    return(convert_and_read(local_path))
  }
  if (!force_pull && file.exists(TEXT_CACHE)) {
    message("Using cached manuscript text (", TEXT_CACHE, "). Pass --pull to force a fresh download.")
    return(paste(readLines(TEXT_CACHE, warn = FALSE), collapse = "\n"))
  }
  suppressPackageStartupMessages(library(googledrive))
  message("Downloading live manuscript from Google Drive...")
  drive_auth(email = "omarlizardo@gmail.com")
  tmp_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp_docx), add = TRUE)
  drive_download(as_id(DOC_ID), path = tmp_docx, overwrite = TRUE)
  txt <- convert_and_read(tmp_docx)
  dir.create("cache", showWarnings = FALSE, recursive = TRUE)
  writeLines(txt, TEXT_CACHE)
  txt
}

txt <- get_manuscript_text()
txt_flat <- str_replace_all(txt, "[\\s\r\n]+", " ")
message(sprintf(
  "Manuscript text: %d characters (%d after whitespace collapse).",
  nchar(txt), nchar(txt_flat)
))

# ---------------------------------------------------------------------
# 2. Load the ground-truth modeling cache
# ---------------------------------------------------------------------
if (!file.exists("cache/01_prepared_data.rds") || !file.exists("cache/02_models.rds")) {
  stop(
    "cache/01_prepared_data.rds and/or cache/02_models.rds not found. ",
    "Run Scripts/01_prepare_data.R and Scripts/02_fit_models.R first."
  )
}
prep <- readRDS("cache/01_prepared_data.rds")
mods <- readRDS("cache/02_models.rds")

coef_of <- function(model, term) unname(coef(model)[term])
pval_of <- function(model, term) unname(summary(model)$coefficients[term, "Pr(>|t|)"])

# ---------------------------------------------------------------------
# 3. Row-building helpers
# ---------------------------------------------------------------------
num_row <- function(id, description, reported, actual, tol) {
  data.frame(
    id = id, description = description,
    reported = sprintf("%.4g", reported), actual = sprintf("%.4g", actual),
    tol = tol, status = ifelse(abs(reported - actual) <= tol, "OK", "MISMATCH"),
    note = "", stringsAsFactors = FALSE
  )
}

lt_row <- function(id, description, threshold, actual) {
  data.frame(
    id = id, description = description,
    reported = sprintf("< %.4g", threshold), actual = sprintf("%.4g", actual),
    tol = NA_real_, status = ifelse(actual < threshold, "OK", "MISMATCH"),
    note = "reported as an upper-bound threshold (e.g. p < .001)",
    stringsAsFactors = FALSE
  )
}

not_found_row <- function(id, description) {
  data.frame(
    id = id, description = description, reported = NA_character_,
    actual = NA_character_, tol = NA_real_, status = "NOT_FOUND",
    note = "pattern not matched in current manuscript text -- check wording, or the claim may have been removed/reworded",
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------
# 4. Checks registry
# ---------------------------------------------------------------------
checks <- list(

  # -- Activities factor model (Table A2 / Figure 1) -------------------
  function(txt_flat, prep, mods) {
    m <- str_match(txt_flat, "\\bKMO\\s*=\\s*\\.(\\d{3})\\)")
    if (is.na(m[1, 1])) return(not_found_row("act_kmo", "Activities factor model KMO"))
    num_row("act_kmo", "Activities factor model KMO",
            as.numeric(paste0(".", m[1, 2])), prep$act_kmo, tol = 0.0015)
  },
  function(txt_flat, prep, mods) {
    m <- str_match(txt_flat, "explained\\s*([\\d.]+)%\\s*of the variance")
    if (is.na(m[1, 1])) return(not_found_row("act_var_pct", "Activities factor model cumulative variance explained (%)"))
    num_row("act_var_pct", "Activities factor model cumulative variance explained (%)",
            as.numeric(m[1, 2]), 100 * prep$act_var_explained, tol = 0.15)
  },

  # -- Weak-/strong-tie factor models (Table A3 / Figure 2) ------------
  function(txt_flat, prep, mods) {
    m <- str_match(txt_flat, "KMOs\\s*=\\s*\\.(\\d{3})\\s*for weak ties and\\s*\\.(\\d{3})\\s*for strong ties")
    if (is.na(m[1, 1])) return(not_found_row("tie_kmo", "Weak-/strong-tie factor model KMOs"))
    rbind(
      num_row("tie_kmo_weak", "Weak-tie factor model KMO", as.numeric(paste0(".", m[1, 2])), prep$weak_fit$kmo, tol = 0.0015),
      num_row("tie_kmo_strong", "Strong-tie factor model KMO", as.numeric(paste0(".", m[1, 3])), prep$strong_fit$kmo, tol = 0.0015)
    )
  },
  function(txt_flat, prep, mods) {
    m <- str_match(txt_flat, "explaining approximately\\s*(\\d+)%\\s*of the total variance")
    if (is.na(m[1, 1])) return(not_found_row("tie_var_pct", "Weak-/strong-tie factor model variance explained (~%, both solutions)"))
    reported <- as.numeric(m[1, 2])
    rbind(
      num_row("tie_var_pct_weak", "Weak-tie factor model variance explained (%)", reported, 100 * prep$weak_fit$var_explained, tol = 1.5),
      num_row("tie_var_pct_strong", "Strong-tie factor model variance explained (%)", reported, 100 * prep$strong_fit$var_explained, tol = 1.5)
    )
  },

  # -- Inter-factor (Phi) correlations, rotation-agnostic --------------
  # Dual-mode: under oblimin the manuscript reports specific non-zero
  # Phi correlations in prose (matched by the first regex below); under
  # varimax (the standing choice as of the September 2026 revert) the
  # manuscript instead states that the three dimensions are "forced to
  # be uncorrelated by construction" with no numbers to extract, so we
  # fall back to asserting the actual Phi matrices are exactly diagonal
  # (a structural check rather than a text-vs-number diff). This keeps
  # the check meaningful and NOT_FOUND-free regardless of which rotation
  # is currently in force.
  function(txt_flat, prep, mods) {
    m <- str_match(txt_flat,
      "liberal \\(r\\s*=\\s*\\.(\\d{2}) weak,\\s*\\.(\\d{2}) strong\\) and conservative \\(r\\s*=\\s*\\.(\\d{2}) weak,\\s*\\.(\\d{2}) strong\\)")
    if (!is.na(m[1, 1])) {
      return(rbind(
        num_row("phi_variety_liberal_weak", "Phi(variety, liberal), weak ties", as.numeric(paste0(".", m[1, 2])), prep$weak_fit$phi["variety", "liberal"], tol = 0.006),
        num_row("phi_variety_liberal_strong", "Phi(variety, liberal), strong ties", as.numeric(paste0(".", m[1, 3])), prep$strong_fit$phi["variety", "liberal"], tol = 0.006),
        num_row("phi_variety_conservative_weak", "Phi(variety, conservative), weak ties", as.numeric(paste0(".", m[1, 4])), prep$weak_fit$phi["variety", "conservative"], tol = 0.006),
        num_row("phi_variety_conservative_strong", "Phi(variety, conservative), strong ties", as.numeric(paste0(".", m[1, 5])), prep$strong_fit$phi["variety", "conservative"], tol = 0.006)
      ))
    }
    if (str_detect(txt_flat, "forced to be uncorrelated by construction")) {
      off_diag_max <- max(abs(c(
        prep$weak_fit$phi["variety", "liberal"], prep$weak_fit$phi["variety", "conservative"],
        prep$strong_fit$phi["variety", "liberal"], prep$strong_fit$phi["variety", "conservative"]
      )))
      return(num_row("phi_variety", "Phi(variety, liberal/conservative composition), weak & strong ties -- varimax orthogonality check", 0, off_diag_max, tol = 1e-8))
    }
    not_found_row("phi_variety", "Phi(variety, liberal/conservative composition), weak & strong ties")
  },
  function(txt_flat, prep, mods) {
    m <- str_match(txt_flat, "another \\(r\\s*=\\s*\\.(\\d{2}) weak,\\s*\\.(\\d{2})\\s*strong\\) rather than negatively related")
    if (!is.na(m[1, 1])) {
      return(rbind(
        num_row("phi_lib_cons_weak", "Phi(liberal, conservative), weak ties", as.numeric(paste0(".", m[1, 2])), prep$weak_fit$phi["liberal", "conservative"], tol = 0.006),
        num_row("phi_lib_cons_strong", "Phi(liberal, conservative), strong ties", as.numeric(paste0(".", m[1, 3])), prep$strong_fit$phi["liberal", "conservative"], tol = 0.006)
      ))
    }
    if (str_detect(txt_flat, "forced to be uncorrelated by construction")) {
      off_diag_max <- max(abs(c(
        prep$weak_fit$phi["liberal", "conservative"], prep$strong_fit$phi["liberal", "conservative"]
      )))
      return(num_row("phi_lib_cons", "Phi(liberal, conservative composition), weak & strong ties -- varimax orthogonality check", 0, off_diag_max, tol = 1e-8))
    }
    not_found_row("phi_lib_cons", "Phi(liberal, conservative composition), weak & strong ties")
  },

  # -- Table A2 (fka Table A5, renamed September 2026): variety-count
  #    validation correlations -----------------------------------------
  function(txt_flat, prep, mods) {
    m <- str_match(txt_flat,
      "most strongly correlated of the three factors with these observed counts \\(r = \\.(\\d{2}), versus \\.(\\d{2}) and \\.(\\d{2}) for liberal and conservative composition, respectively\\)")
    if (is.na(m[1, 1])) return(not_found_row("tableA2_strong", "Table A2 strong-tie variety/liberal/conservative correlations with raw group counts"))
    d <- prep$df
    idx <- prep$strong_fit$idx
    scores <- as.data.frame(prep$strong_fit$fa$scores)
    colnames(scores) <- names(idx)[match(seq_len(3), idx)]
    rbind(
      num_row("tableA2_strong_variety", "Table A2: strong-tie variety vs. raw count (r)", as.numeric(paste0(".", m[1, 2])), cor(scores$variety, d$strong_variety_count1, use = "complete.obs"), tol = 0.01),
      num_row("tableA2_strong_liberal", "Table A2: strong-tie liberal composition vs. raw count (r)", as.numeric(paste0(".", m[1, 3])), cor(scores$liberal, d$strong_variety_count1, use = "complete.obs"), tol = 0.01),
      num_row("tableA2_strong_conservative", "Table A2: strong-tie conservative composition vs. raw count (r)", as.numeric(paste0(".", m[1, 4])), cor(scores$conservative, d$strong_variety_count1, use = "complete.obs"), tol = 0.01)
    )
  },
  function(txt_flat, prep, mods) {
    # Wording varies across revision rounds: oblimin-era prose said
    # "liberal-composition factor correlates ..."; the varimax-reverted
    # text drops the hyphen and "factor" ("liberal composition
    # correlates ..."). Make both optional so future rewordings of this
    # sentence are less likely to silently produce a false NOT_FOUND.
    m <- str_match(txt_flat,
      "liberal(?:-composition factor| composition) correlates about as strongly with the raw group count \\(r = \\.(\\d{2})\\) as does the variety factor itself \\(r = \\.(\\d{2})\\), and conservative composition is not far behind \\(r = \\.(\\d{2})\\)")
    if (is.na(m[1, 1])) return(not_found_row("tableA2_weak", "Table A2 weak-tie variety/liberal/conservative correlations with raw group counts"))
    d <- prep$df
    idx <- prep$weak_fit$idx
    scores <- as.data.frame(prep$weak_fit$fa$scores)
    colnames(scores) <- names(idx)[match(seq_len(3), idx)]
    rbind(
      num_row("tableA2_weak_liberal", "Table A2: weak-tie liberal composition vs. raw count (r)", as.numeric(paste0(".", m[1, 2])), cor(scores$liberal, d$weak_variety_count1, use = "complete.obs"), tol = 0.01),
      num_row("tableA2_weak_variety", "Table A2: weak-tie variety vs. raw count (r)", as.numeric(paste0(".", m[1, 3])), cor(scores$variety, d$weak_variety_count1, use = "complete.obs"), tol = 0.01),
      num_row("tableA2_weak_conservative", "Table A2: weak-tie conservative composition vs. raw count (r)", as.numeric(paste0(".", m[1, 4])), cor(scores$conservative, d$weak_variety_count1, use = "complete.obs"), tol = 0.01)
    )
  },

  # -- Table 1 (arts participation): weak- vs strong-tie variety Wald --
  function(txt_flat, prep, mods) {
    m <- str_match(txt_flat, "F\\((\\d+),\\s*(\\d+)\\)\\s*=\\s*([\\d.]+),\\s*p\\s*<\\s*\\.(\\d+)")
    if (is.na(m[1, 1])) return(not_found_row("arts_wald", "Table 1: weak- vs. strong-tie variety Wald test (arts participation)"))
    actual_chisq <- unname(mods$arts_wald$Chisq[2])
    actual_df2 <- unname(mods$arts_wald$Res.Df[2])
    reported_df2 <- as.numeric(m[1, 3])
    rbind(
      num_row("arts_wald_stat", "Arts Wald test statistic (F/Chisq, df=1)", as.numeric(m[1, 4]), actual_chisq, tol = 0.02),
      num_row("arts_wald_df2", "Arts Wald test residual df", reported_df2, actual_df2, tol = 0),
      lt_row("arts_wald_p", "Arts Wald test p-value", as.numeric(paste0(".", m[1, 5])), unname(mods$arts_wald[["Pr(>Chisq)"]][2]))
    )
  },

  # -- Table 3 (residual leisure): composition-block nested Wald tests -
  function(txt_flat, prep, mods) {
    m <- str_match(txt_flat,
      "variety alone \\(\u03c7\u00b2\\(4\\)\\s*=\\s*([\\d.]+),\\s*p\\s*=\\s*\\.(\\d+)\\)\\s*or over models that already include the full set of\\s*socio-demographic controls \\(\u03c7\u00b2\\(4\\)\\s*=\\s*([\\d.]+),\\s*p\\s*=\\s*\\.(\\d+)\\)")
    if (is.na(m[1, 1])) return(not_found_row("residual_wald", "Table 3: composition-block nested Wald tests (residual leisure)"))
    rbind(
      num_row("residual_wald_m1m3_chisq", "Residual leisure: composition block Chisq (vs. variety-only model)", as.numeric(m[1, 2]), unname(mods$residual_nested_m1_m3$Chisq[2]), tol = 0.02),
      num_row("residual_wald_m1m3_p", "Residual leisure: composition block p (vs. variety-only model)", as.numeric(paste0(".", m[1, 3])), unname(mods$residual_nested_m1_m3[["Pr(>Chisq)"]][2]), tol = 0.002),
      num_row("residual_wald_m2m4_chisq", "Residual leisure: composition block Chisq (vs. full covariate model)", as.numeric(m[1, 4]), unname(mods$residual_nested_m2_m4$Chisq[2]), tol = 0.02),
      num_row("residual_wald_m2m4_p", "Residual leisure: composition block p (vs. full covariate model)", as.numeric(paste0(".", m[1, 5])), unname(mods$residual_nested_m2_m4[["Pr(>Chisq)"]][2]), tol = 0.002)
    )
  },

  # -- Table 2 (solitary leisure): weak-/strong-tie variety coefficient ranges across Models 1-4 --
  function(txt_flat, prep, mods) {
    m <- str_match(txt_flat,
      "approximately ([\\d.]+) to ([\\d.]+) standard deviations in solitary leisure across the four model specifications\\. In contrast, greater strong-tie variety is associated with a roughly ([\\d.]+) to ([\\d.]+) standard deviation decrease")
    if (is.na(m[1, 1])) return(not_found_row("leisure_variety_range", "Table 2: weak-/strong-tie variety coefficient ranges across Models 1-4"))
    weak_vals <- sapply(mods$leisure_models, function(x) coef_of(x, "weak_variety"))
    strong_vals <- abs(sapply(mods$leisure_models, function(x) coef_of(x, "strong_variety")))
    rbind(
      num_row("leisure_weak_min", "Table 2: weak-tie variety coefficient, min across Models 1-4", as.numeric(m[1, 2]), min(weak_vals), tol = 0.006),
      num_row("leisure_weak_max", "Table 2: weak-tie variety coefficient, max across Models 1-4", as.numeric(m[1, 3]), max(weak_vals), tol = 0.006),
      num_row("leisure_strong_min", "Table 2: |strong-tie variety coefficient|, min across Models 1-4", as.numeric(m[1, 4]), min(strong_vals), tol = 0.006),
      num_row("leisure_strong_max", "Table 2: |strong-tie variety coefficient|, max across Models 1-4", as.numeric(m[1, 5]), max(strong_vals), tol = 0.006)
    )
  },

  # -- Table 2 (solitary leisure): childhood arts exposure, Model 4 ----
  function(txt_flat, prep, mods) {
    m <- str_match(txt_flat, "composition variables are\\s*introduced in Model 4 \\(b\\s*=\\s*\\.(\\d+),\\s*p\\s*=\\s*\\.(\\d+)\\)")
    if (is.na(m[1, 1])) return(not_found_row("leisure_child_arts_m4", "Table 2: childhood arts exposure coefficient/p-value, Model 4"))
    rbind(
      num_row("leisure_child_arts_m4_b", "Table 2: childhood arts coefficient, Model 4", as.numeric(paste0(".", m[1, 2])), coef_of(mods$leisure_models$m4, "child_arts"), tol = 0.006),
      num_row("leisure_child_arts_m4_p", "Table 2: childhood arts p-value, Model 4", as.numeric(paste0(".", m[1, 3])), pval_of(mods$leisure_models$m4, "child_arts"), tol = 0.006)
    )
  },

  # -- Table 1 (arts participation): race/ethnicity contrasts, Model 2 -
  # Each of the three p-values can independently be reported as either
  # an exact value ("p = .0XX") or an upper-bound threshold ("p < .0XX")
  # depending on how small it is -- e.g. the multiracial contrast was
  # "p = .005" under oblimin but tightened to "p < .001" after the
  # varimax revert. Capture the operator for all three so a future
  # rewording that flips any of them between "<" and "=" doesn't
  # silently produce a NOT_FOUND.
  function(txt_flat, prep, mods) {
    m <- str_match(txt_flat,
      "than among AAPI \\(p\\s*(<|=)\\s*\\.(\\d+)\\) or multiracial respondents \\(p\\s*(<|=)\\s*\\.(\\d+)\\); the corresponding contrast with Hispanic/Latine respondents is in the same direction but only marginally significant \\(p\\s*(<|=)\\s*\\.(\\d+)\\)")
    if (is.na(m[1, 1])) return(not_found_row("arts_race_m2", "Table 1: race/ethnicity contrasts with White respondents, Model 2"))
    row_for <- function(id, description, op, digits, term) {
      reported <- as.numeric(paste0(".", digits))
      actual <- pval_of(mods$arts_models$m2, term)
      if (op == "<") lt_row(id, description, reported, actual) else num_row(id, description, reported, actual, tol = 0.006)
    }
    rbind(
      row_for("arts_race_aapi_p", "Table 1: AAPI vs. White p-value, Model 2", m[1, 2], m[1, 3], "race_fAAPI"),
      row_for("arts_race_mixed_p", "Table 1: multiracial vs. White p-value, Model 2", m[1, 4], m[1, 5], "race_fmixed/other"),
      row_for("arts_race_latine_p", "Table 1: Hispanic/Latine vs. White p-value, Model 2", m[1, 6], m[1, 7], "race_flatine")
    )
  }
)

# ---------------------------------------------------------------------
# 5. Run all checks and report
# ---------------------------------------------------------------------
results <- do.call(rbind, lapply(checks, function(f) f(txt_flat, prep, mods)))
rownames(results) <- NULL

status_order <- c(MISMATCH = 1, NOT_FOUND = 2, OK = 3)
results <- results[order(status_order[results$status]), ]

n_ok <- sum(results$status == "OK")
n_mismatch <- sum(results$status == "MISMATCH")
n_missing <- sum(results$status == "NOT_FOUND")

message(sprintf(
  "\n%d checks: %d OK, %d MISMATCH, %d NOT_FOUND.\n",
  nrow(results), n_ok, n_mismatch, n_missing
))
print(results, row.names = FALSE)

dir.create("cache", showWarnings = FALSE, recursive = TRUE)
report_path <- "cache/audit_report.md"
lines <- c(
  sprintf("# Manuscript statistic audit -- %s", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
  "",
  sprintf("%d checks: **%d OK**, **%d MISMATCH**, **%d NOT_FOUND**.", nrow(results), n_ok, n_mismatch, n_missing),
  "",
  "| id | description | reported | actual | status | note |",
  "|---|---|---|---|---|---|"
)
row_lines <- apply(results, 1, function(r) {
  sprintf("| %s | %s | %s | %s | %s | %s |", r["id"], r["description"], r["reported"], r["actual"], r["status"], r["note"])
})
writeLines(c(lines, row_lines), report_path)
message("Report written to ", report_path)

if (!interactive()) {
  quit(status = as.integer(n_mismatch > 0), save = "no")
}
