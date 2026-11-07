#' Checks two decomposition results: (1) whether the delta-method SE of each aggregate indirect
#' effect matches a direct calculation from the labeled model's covariance matrix, and (2) how the
#' aggregate indirect effect relates to its factor-specific components.
#'
#' Run from the project root: Rscript Scripts/diagnostics/check_decomposition_se.R

suppressPackageStartupMessages({
  library(lavaan)
})

decomp_lines <- readLines("Scripts/diagnostics/sem_decomposition.R")
stop_at <- grep("^decompose_tie <- function", decomp_lines)
eval(parse(text = decomp_lines[seq_len(stop_at - 1)]))

refit_labeled <- function(tie) {
  res <- fit_tie(tie, antonym = FALSE)
  net_fac <- res$net_fac
  dat <- lavInspect(res$fit, "data")
  ord <- lavNames(res$fit, "ov.ord")
  syn <- label_syntax(res$syntax, net_fac)
  defs <- character()
  for (o in act_fac) for (cv in names(covs)) {
    defs <- c(defs, sprintf("ind_%s_%s := %s", o, covs[[cv]],
      paste(sprintf("a_%s_%s*b_%s_%s", net_fac, covs[[cv]], o, net_fac), collapse = " + ")))
    for (k in net_fac) defs <- c(defs, sprintf("ip_%s_%s_%s := a_%s_%s*b_%s_%s", o, covs[[cv]], k, k, covs[[cv]], o, k))
  }
  syn_def <- paste(syn, paste(defs, collapse = "\n"), sep = "\n")
  sem(syn_def, data = dat, ordered = ord, estimator = "WLSMV", std.lv = TRUE, fixed.x = TRUE)
}

check_aggregate <- function(tie, o, cv, f1) {
  net_fac <- c("Wv", "Wl", "Wc", "Sv", "Sl", "Sc")
  net_fac <- if (tie == "weak") net_fac[1:3] else net_fac[4:6]
  a_names <- sprintf("a_%s_%s", net_fac, covs[[cv]])
  b_names <- sprintf("b_%s_%s", o, net_fac)
  est_a <- coef(f1)[a_names]; est_b <- coef(f1)[b_names]
  V <- vcov(f1)
  idx <- c(a_names, b_names)
  grad <- c(est_b, est_a)
  Vsub <- V[idx, idx]
  se_manual <- sqrt(as.numeric(t(grad) %*% Vsub %*% grad))
  comp <- est_a * est_b
  comp_se <- sqrt(est_b^2 * diag(V)[a_names] + est_a^2 * diag(V)[b_names])
  pe <- parameterEstimates(f1)
  agg <- pe[pe$lhs == sprintf("ind_%s_%s", o, covs[[cv]]) & pe$op == ":=", ]
  cat(sprintf("\n%s ties, %s on %s: aggregate est %.4f, lavaan SE %.4f, manual delta SE %.4f, p %.4g\n",
              tie, covs[[cv]], o, agg$est, agg$se, se_manual, agg$pvalue))
  cat(sprintf("  sum of components %.4f\n", sum(comp)))
  print(data.frame(factor = net_fac, a = round(est_a, 4), b = round(est_b, 4),
                   component = round(comp, 4), component_se_no_cov = round(comp_se, 4),
                   component_p = signif(2 * pnorm(-abs(comp / comp_se)), 3)), row.names = FALSE)
  off <- Vsub[a_names, b_names]
  cat("  covariance between a and b paths (rows a, cols b):\n")
  print(round(off, 5))
}

for (tie in c("weak", "strong")) {
  f1 <- refit_labeled(tie)
  cat(sprintf("\n===== %s ties, labeled model converged = %s =====\n", tie, lavInspect(f1, "converged")))
  check_aggregate(tie, "A", "child_arts", f1)
  check_aggregate(tie, "Lis", "child_arts", f1)
  check_aggregate(tie, "Lis", "poli", f1)
  check_aggregate(tie, "A", "poli", f1)
}
