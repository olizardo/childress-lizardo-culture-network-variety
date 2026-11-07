#' Collinearity check for the weak-tie SEM (script 07 sorting, baseline measurement model).
#'
#' 1. Residual (conditional on demographics) correlations among the weak-tie network factors,
#'    their variance inflation factors, and the condition number of the correlation matrix.
#' 2. Re-estimates the arts equation with subsets of the weak-tie network factors, holding the
#'    measurement model, demographic paths, and the other two activity equations fixed, to see
#'    how the variety and liberal coefficients and their standard errors respond when the other
#'    factor is dropped.
#'
#' Run from the project root: Rscript Scripts/diagnostics/collinearity_weak_sem.R

suppressPackageStartupMessages({
  library(lavaan)
})

script_lines <- readLines("Scripts/diagnostics/sem_by_tie_script07.R")
stop_at <- grep("^report_tie <- function", script_lines)
eval(parse(text = script_lines[seq_len(stop_at - 1)]))

res <- fit_tie("weak", antonym = FALSE)
f <- res$fit
net <- res$net_fac
pe <- parameterEstimates(f, standardized = TRUE)

R <- diag(length(net))
dimnames(R) <- list(net, net)
for (i in seq_along(net)) for (j in seq_along(net)) {
  if (i == j) next
  hit <- pe[pe$op == "~~" & ((pe$lhs == net[i] & pe$rhs == net[j]) | (pe$lhs == net[j] & pe$rhs == net[i])), "std.all"]
  R[i, j] <- hit
}
cat("Residual correlations among weak-tie network factors (conditional on demographics):\n")
print(round(R, 3))
cat(sprintf("\nVIF: %s\n", paste(sprintf("%s = %.2f", net, diag(solve(R))), collapse = ", ")))
cat(sprintf("Condition number of the correlation matrix: %.2f\n", kappa(R, exact = TRUE)))

dat <- lavInspect(f, "data")
ord <- lavNames(f, "ov.ord")
syn_full <- res$syntax
act_line <- grep("^A ~ ", strsplit(syn_full, "\n")[[1]], value = TRUE)
rest_of_act <- sub("^A ~ [^+]*\\+ ", "", act_line)
cov_terms <- rest_of_act
cov_only <- sub(paste0("^(", paste(net, collapse = " \\+ "), ") \\+ "), "", act_line)

refit_arts <- function(predictors) {
  new_line <- sprintf("A ~ %s", paste(c(predictors, res$cov_names), collapse = " + "))
  syn <- sub("^A ~ .*$", new_line, syn_full, perl = TRUE)
  syn <- gsub("\nA ~ [^\n]*", paste0("\n", new_line), syn_full)
  fit <- sem(syn, data = dat, ordered = ord, estimator = "WLSMV", std.lv = TRUE, fixed.x = TRUE)
  pe2 <- parameterEstimates(fit)
  out <- pe2[pe2$op == "~" & pe2$lhs == "A" & pe2$rhs %in% predictors, c("rhs", "est", "se", "pvalue")]
  list(coef = out, converged = lavInspect(fit, "converged"),
       pd = all(eigen(lavInspect(fit, "cov.lv"))$values > 0))
}

sets <- list(
  "all three" = net,
  "variety only" = "Wv",
  "liberal only" = "Wl",
  "conservative only" = "Wc",
  "variety and liberal" = c("Wv", "Wl"),
  "variety and conservative" = c("Wv", "Wc"))
for (nm in names(sets)) {
  r <- refit_arts(sets[[nm]])
  cat(sprintf("\nArts equation with %s (converged = %s, PD = %s):\n", nm, r$converged, r$pd))
  print(transform(r$coef, est = round(est, 3), se = round(se, 3), pvalue = signif(pvalue, 3)), row.names = FALSE)
}
