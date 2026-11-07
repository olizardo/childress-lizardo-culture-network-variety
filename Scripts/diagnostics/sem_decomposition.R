#' Direct, indirect, and total effects of education, childhood arts exposure, and politics on the
#' three activity factors, decomposed through the six network factors (script 07 sorting,
#' baseline measurement model, separate weak-tie and strong-tie structural models).
#'
#' The structural models are refit with labeled paths so that the indirect effects are defined
#' as sums of products of paths (a * b) and their standard errors come from the delta method in
#' lavaan. The labeled refit should reproduce the original fit exactly, and the script checks this.
#'
#' Run from the project root: Rscript Scripts/diagnostics/sem_decomposition.R

suppressPackageStartupMessages({
  library(lavaan)
})

script_lines <- readLines("Scripts/diagnostics/sem_by_tie_script07.R")
stop_at <- grep("^report_tie <- function", script_lines)
eval(parse(text = script_lines[seq_len(stop_at - 1)]))

covs <- c(educ_ord = "educ", child_arts = "arts", poli = "poli")
act_fac <- c("A", "Lis", "R")

label_syntax <- function(syn, net_fac) {
  lines <- strsplit(syn, "\n")[[1]]
  out <- vapply(lines, function(line) {
    if (grepl("^(Wv|Wl|Wc|Sv|Sl|Sc) ~ ", line)) {
      f <- sub(" ~.*$", "", line)
      lhs <- sub(" ~.*$", "", line)
      rhs <- sub("^[^~]*~ ", "", line)
      for (cv in names(covs)) rhs <- gsub(paste0("\\b", cv, "\\b"), sprintf("a_%s_%s*%s", f, covs[[cv]], cv), rhs)
      return(paste(lhs, "~", rhs))
    }
    if (grepl("^(A|Lis|R) ~ ", line)) {
      o <- sub(" ~.*$", "", line)
      rhs <- sub("^[^~]*~ ", "", line)
      for (nf in net_fac) rhs <- gsub(paste0("\\b", nf, "\\b"), sprintf("b_%s_%s*%s", o, nf, nf), rhs)
      for (cv in names(covs)) rhs <- gsub(paste0("\\b", cv, "\\b"), sprintf("d_%s_%s*%s", o, covs[[cv]], cv), rhs)
      return(paste(o, "~", rhs))
    }
    line
  }, character(1))
  paste(out, collapse = "\n")
}

decompose_tie <- function(tie) {
  res <- fit_tie(tie, antonym = FALSE)
  net_fac <- res$net_fac
  f0 <- res$fit
  dat <- lavInspect(f0, "data")
  ord <- lavNames(f0, "ov.ord")
  syn <- label_syntax(res$syntax, net_fac)
  defs <- character()
  for (o in act_fac) for (cv in names(covs)) {
    ind_terms <- sprintf("a_%s_%s*b_%s_%s", net_fac, covs[[cv]], o, net_fac)
    defs <- c(defs,
      sprintf("ind_%s_%s := %s", o, covs[[cv]], paste(ind_terms, collapse = " + ")),
      sprintf("dir_%s_%s := d_%s_%s", o, covs[[cv]], o, covs[[cv]]),
      sprintf("tot_%s_%s := dir_%s_%s + ind_%s_%s", o, covs[[cv]], o, covs[[cv]], o, covs[[cv]]))
  }
  for (o in act_fac) for (cv in names(covs)) for (k in net_fac) {
    defs <- c(defs, sprintf("ip_%s_%s_%s := a_%s_%s*b_%s_%s", o, covs[[cv]], k, k, covs[[cv]], o, k))
  }
  syn_def <- paste(syn, paste(defs, collapse = "\n"), sep = "\n")
  f1 <- sem(syn_def, data = dat, ordered = ord, estimator = "WLSMV", std.lv = TRUE, fixed.x = TRUE)
  cat(sprintf("\n===== %s ties: labeled refit reproduces CFI %.4f vs %.4f; converged = %s =====\n",
              tie, fitMeasures(f1, "cfi.scaled"), fitMeasures(f0, "cfi.scaled"), lavInspect(f1, "converged")))
  pe <- parameterEstimates(f1, standardized = TRUE)
  dd <- pe[pe$op == ":=", c("lhs", "est", "se", "pvalue", "std.all")]
  dd$tie <- tie
  dd
}

out <- rbind(decompose_tie("weak"), decompose_tie("strong"))
out$est <- round(out$est, 3)
out$se <- round(out$se, 3)
out$pvalue <- signif(out$pvalue, 3)
out$std.all <- round(out$std.all, 3)
print(out, row.names = FALSE)
saveRDS(out, "cache/diagnostics/sem_decomposition.rds")
