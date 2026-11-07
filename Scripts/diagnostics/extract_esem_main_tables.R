#' Extract every number needed for the ESEM-first results draft into one
#' canonical markdown + RDS (Phase A6 of the 2026-11-07 plan).
#'
#' Sources:
#'   cache/diagnostics/esem_ties_target.rds        (tie ESEM measurement, N = 1,258)
#'   cache/diagnostics/esem_reliability.rds        (unadjusted fit indices, omegas)
#'   cache/diagnostics/sem_set_esem_full.rds       (separate set-ESEM structural models, N = 1,227)
#'   cache/diagnostics/sem_set_esem_decomposition.rds (labeled refits, delta-method effects)
#'   cache/diagnostics/esem_fit_indices.rds        (antonym-residual variants)
#'
#' Run from the project root: Rscript Scripts/diagnostics/extract_esem_main_tables.R
#' Writes cache/esem_canonical/esem_main_numbers.md and esem_main_numbers.rds.

suppressPackageStartupMessages({
  library(lavaan)
})

dir.create("cache/esem_canonical", showWarnings = FALSE)
sink_file <- "cache/esem_canonical/esem_main_numbers.md"
con <- file(sink_file, open = "wt")
sink(con); sink(con, type = "message")

meas <- readRDS("cache/diagnostics/esem_ties_target.rds")
rel <- readRDS("cache/diagnostics/esem_reliability.rds")
sem <- readRDS("cache/diagnostics/sem_set_esem_full.rds")
dec <- readRDS("cache/diagnostics/sem_set_esem_decomposition.rds")
fi <- readRDS("cache/diagnostics/esem_fit_indices.rds")

fmt <- function(x, d = 2) formatC(x, format = "f", digits = d)
star <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))

# ---------- 1. Measurement ESEM loadings (Table 5 replacement) ----------
cat("# ESEM measurement loadings (pst target), N = 1,258 per battery\n\n")
load_out <- list()
for (tie in c("weak", "strong")) {
  obj <- meas[[paste(tie, "target")]]
  lam <- obj$loadings
  tgt <- obj$target
  cat(sprintf("## %s ties\n\n", tie))
  cat("| Item | Variety | Liberal | Conservative |\n|---|---|---|---|\n")
  for (i in seq_len(nrow(lam))) {
    vals <- sprintf("%.2f%s", lam[i, ], star(NULL))
    k <- which(tgt[i, ] == 1)
    load_out[[paste(tie, rownames(lam)[i])]] <- lam[i, ]
    cat(sprintf("| %s | %s |\n", rownames(lam)[i],
      paste(vapply(seq_along(vals), function(j) if (j == k) sprintf("**%s**", vals[j]) else vals[j], character(1)), collapse = " | ")))
  }
  fm <- obj$fitmeasures
  cat(sprintf("\nFit: chi2 = %.1f, df = %d, CFI = %.3f, RMSEA = %.3f [%.3f, %.3f], SRMR = %.3f\n",
    fm[1], fm[2], fm[3], fm[4], fm[5], fm[6], fm[7]))
  cors <- lavInspect(obj$fit, "cor.lv")
  cat("Factor correlations:\n\n")
  print(round(cors, 3))
  cat("\n")
}

# ---------- 2. Structural set-ESEM (Tables 6-10 replacements) ----------
cat("\n# Separate set-ESEM structural models (N = 1,227)\n\n")
cat("Fit (unadjusted ML-discrepancy indices; robust scaled correction unavailable):\n\n")
for (nm in c("sep_weak", "sep_strong")) {
  fm <- rel$fit_indices[[nm]]
  cat(sprintf("- %s: chisq = %.1f, df = %d, CFI = %.3f, TLI = %.3f, RMSEA = %.3f [%.3f, %.3f], SRMR = %.3f\n",
    sem[[nm]]$label, fm[1], fm[2], fm[3], fm[4], fm[5], fm[6], fm[7], fm[8]))
}
cat("\nOmega under ESEM (model-implied, cross-loadings included):\n\n")
for (nm in names(rel$omegas)) cat(sprintf("- %s: %.3f (target-only %.3f)\n", nm,
  rel$omegas[[nm]]["omega"], rel$omegas[[nm]]["omega_target_only"]))

cat("\n## Table 6 replacement: network-to-activity paths\n\n")
path_out <- list()
for (nm in c("sep_weak", "sep_strong")) {
  fit <- sem[[nm]]$fit
  pe <- parameterEstimates(fit, standardized = TRUE)
  cat(sprintf("### %s\n\n", sem[[nm]]$label))
  cat("| Network factor | Arts | Solitary leisure | Residual |\n|---|---|---|---|\n")
  for (nf in unique(pe$rhs[pe$op == "~" & pe$lhs %in% c("A", "Lis", "R") & grepl("^[WS][vlc]$", pe$rhs)])) next
  net_fac <- unique(pe$rhs[pe$op == "~" & pe$lhs %in% c("A", "Lis", "R") & grepl("^[WS][vlc]$", pe$rhs)])
  for (nf in net_fac) {
    row <- pe[pe$op == "~" & pe$rhs == nf & pe$lhs %in% c("A", "Lis", "R"), ]
    row <- row[match(c("A", "Lis", "R"), row$lhs), ]
    path_out[[paste(nm, nf)]] <- row
    cat(sprintf("| %s | %s | %s | %s |\n", nf,
      sprintf("%.3f%s", row$est[1], star(row$pvalue[1])),
      sprintf("%.3f%s", row$est[2], star(row$pvalue[2])),
      sprintf("%.3f%s", row$est[3], star(row$pvalue[3]))))
  }
}

cat("\n## Tables 7-8 replacement: demographic paths on network factors\n\n")
demo_net <- list()
for (nm in c("sep_weak", "sep_strong")) {
  fit <- sem[[nm]]$fit
  pe <- parameterEstimates(fit, standardized = TRUE)
  net_fac <- unique(pe$rhs[pe$op == "~" & pe$lhs %in% c("A", "Lis", "R") & grepl("^[WS][vlc]$", pe$rhs)])
  cat(sprintf("### %s\n\n", sem[[nm]]$label))
  cat("| Predictor | Variety | Liberal | Conservative |\n|---|---|---|---|\n")
  rhs_all <- unique(pe$rhs[pe$op == "~" & pe$lhs %in% net_fac])
  for (rv in rhs_all) {
    row <- pe[pe$op == "~" & pe$rhs == rv & pe$lhs %in% net_fac, ]
    row <- row[match(net_fac, row$lhs), ]
    demo_net[[paste(nm, rv)]] <- row
    cat(sprintf("| %s | %s | %s | %s |\n", rv,
      sprintf("%.3f%s", row$est[1], star(row$pvalue[1])),
      sprintf("%.3f%s", row$est[2], star(row$pvalue[2])),
      sprintf("%.3f%s", row$est[3], star(row$pvalue[3]))))
  }
}

cat("\n## Tables 9-10 replacement: demographic paths on activity factors\n\n")
demo_act <- list()
for (nm in c("sep_weak", "sep_strong")) {
  fit <- sem[[nm]]$fit
  pe <- parameterEstimates(fit, standardized = TRUE)
  cat(sprintf("### %s\n\n", sem[[nm]]$label))
  cat("| Predictor | Arts | Solitary leisure | Residual |\n|---|---|---|---|\n")
  rhs_all <- unique(pe$rhs[pe$op == "~" & pe$lhs %in% c("A", "Lis", "R")])
  rhs_all <- rhs_all[!grepl("^[WS][vlc]$", rhs_all)]
  for (rv in rhs_all) {
    row <- pe[pe$op == "~" & pe$rhs == rv & pe$lhs %in% c("A", "Lis", "R"), ]
    row <- row[match(c("A", "Lis", "R"), row$lhs), ]
    demo_act[[paste(nm, rv)]] <- row
    cat(sprintf("| %s | %s | %s | %s |\n", rv,
      sprintf("%.3f%s", row$est[1], star(row$pvalue[1])),
      sprintf("%.3f%s", row$est[2], star(row$pvalue[2])),
      sprintf("%.3f%s", row$est[3], star(row$pvalue[3]))))
  }
}

# ---------- 3. Decomposition (Tables 11-12 replacements) ----------
cat("\n# Decomposition (delta method, standardized)\n\n")
dec_out <- list()
for (tname in c("weak", "strong")) {
  defs <- dec[[tname]]$defs
  cat(sprintf("### %s ties\n\n", tname))
  cat("| Outcome | Predictor | Direct | Network-mediated | Total |\n|---|---|---|---|---|\n")
  for (cv in unique(defs$covariate)) for (o in c("A", "Lis", "R")) {
    r <- defs[defs$outcome == o & defs$covariate == cv, ]
    r <- r[match(c("dir", "ind", "tot"), r$effect), ]
    dec_out[[paste(tname, o, cv)]] <- r
    cat(sprintf("| %s | %s | %s | %s | %s |\n", o, cv,
      sprintf("%.3f%s", r$est[1], star(r$pvalue[1])),
      sprintf("%.3f%s", r$est[2], star(r$pvalue[2])),
      sprintf("%.3f%s", r$est[3], star(r$pvalue[3]))))
  }
}

# ---------- 4. Antonym variants under ESEM ----------
cat("\n# Antonym residual variants under the ESEM specification\n\n")
for (nm in names(fi)) {
  if (is.null(fi[[nm]]$fit)) next
  f <- fi[[nm]]$fit
  fm <- fitMeasures(f, c("chisq", "df", "cfi", "rmsea", "srmr"))
  cat(sprintf("- %s: chisq = %.1f, df = %d, CFI = %.3f, RMSEA = %.3f, SRMR = %.3f\n",
    nm, fm[1], fm[2], fm[3], fm[4], fm[5]))
}

saveRDS(list(loadings = load_out, paths = path_out, demo_net = demo_net,
  demo_act = demo_act, dec = dec_out, rel = rel, fit_indices = fit_out),
  "cache/esem_canonical/esem_main_numbers.rds")

sink(); sink()
cat("Wrote", sink_file, "\n")
