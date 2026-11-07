#' Nonparametric bootstrap of the CFA-based indirect effects (main structural models).
#'
#' Targets the two aggregates that the delta-method decomposition flagged as sensitive:
#'   (a) weak-tie solitary-leisure network-mediated effect of childhood arts (ind_Lis_arts, weak);
#'   (b) strong-tie solitary-leisure network-mediated effect of political conservatism (ind_Lis_poli, strong).
#' All defined indirect, direct, and total effects are bootstrapped so the full table is reported
#' with bootstrap SEs and percentile intervals beside the delta-method values.
#'
#' Method: ordinary case resampling of the complete-case analytic sample (N = 1,227). Each draw
#' refits the labeled CFA decomposition model (same syntax as sem_decomposition.R) with WLSMV,
#' std.lv, and fixed.x. Draws that fail to converge or return non-finite defined effects are
#' dropped and counted.
#'
#' Usage (from the project root):
#'   Rscript Scripts/diagnostics/bootstrap_cfa_decomposition.R 1000
#' Writes cache/diagnostics/bootstrap_cfa_decomposition.rds and progress to stdout.

suppressPackageStartupMessages({
  library(lavaan)
  library(parallel)
})

args <- commandArgs(trailingOnly = TRUE)
R_draws <- if (length(args) >= 1) as.integer(args[1]) else 1000L
N_CORES <- as.integer(Sys.getenv("BOOT_CORES", "12"))

# Reuse the model builder from sem_decomposition.R (everything before decompose_tie)
script_lines <- readLines("Scripts/diagnostics/sem_decomposition.R")
stop_at <- grep("^decompose_tie <- function", script_lines)
eval(parse(text = script_lines[seq_len(stop_at - 1)]))

build_labeled <- function(tie) {
  res <- fit_tie(tie, antonym = FALSE)
  net_fac <- res$net_fac
  dat <- lavInspect(res$fit, "data")
  ord <- lavNames(res$fit, "ov.ord")
  syn <- label_syntax(res$syntax, net_fac)
  defs <- character()
  for (o in act_fac) for (cv in names(covs)) {
    ind_terms <- sprintf("a_%s_%s*b_%s_%s", net_fac, covs[[cv]], o, net_fac)
    defs <- c(defs,
      sprintf("ind_%s_%s := %s", o, covs[[cv]], paste(ind_terms, collapse = " + ")),
      sprintf("dir_%s_%s := d_%s_%s", o, covs[[cv]], o, covs[[cv]]),
      sprintf("tot_%s_%s := dir_%s_%s + ind_%s_%s", o, covs[[cv]], o, covs[[cv]], o, covs[[cv]]))
  }
  list(syntax = paste(syn, paste(defs, collapse = "\n"), sep = "\n"), data = dat, ordered = ord)
}

extract_defs <- function(fit) {
  pe <- parameterEstimates(fit)
  d <- pe[pe$op == ":=", ]
  setNames(d$est, d$lhs)
}

one_draw <- function(seed, spec, n) {
  tryCatch({
    set.seed(seed)
    idx <- sample.int(n, n, replace = TRUE)
    d <- spec$data[idx, , drop = FALSE]
    fit <- sem(spec$syntax, data = d, ordered = spec$ordered, estimator = "WLSMV",
      std.lv = TRUE, fixed.x = TRUE)
    if (!lavInspect(fit, "converged")) return(NULL)
    est <- extract_defs(fit)
    if (any(!is.finite(est))) return(NULL)
    est
  }, error = function(e) NULL)
}

results <- list()
for (tie in c("weak", "strong")) {
  spec <- build_labeled(tie)
  n <- nrow(spec$data)
  base <- sem(spec$syntax, data = spec$data, ordered = spec$ordered, estimator = "WLSMV",
    std.lv = TRUE, fixed.x = TRUE)
  est0 <- extract_defs(base)
  cat(sprintf("\n===== %s ties: %d draws on %d cores (N = %d) =====\n", tie, R_draws, N_CORES, n))
  set.seed(if (tie == "weak") 4417 else 4421)
  seeds <- sample.int(1e8, R_draws)
  t0 <- Sys.time()
  cl <- makeCluster(N_CORES, type = "PSOCK")
  clusterEvalQ(cl, suppressPackageStartupMessages(library(lavaan)))
  clusterExport(cl, c("one_draw", "extract_defs"))
  draws <- parLapply(cl, seeds, one_draw, spec = spec, n = n)
  stopCluster(cl)
  cat(sprintf("elapsed: %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  ok <- !vapply(draws, is.null, logical(1))
  bt <- do.call(rbind, draws[ok])
  cat(sprintf("valid draws: %d of %d\n", nrow(bt), R_draws))
  tab <- data.frame(
    effect = names(est0),
    est = unname(est0),
    boot_se = apply(bt, 2, sd),
    ci_lo = apply(bt, 2, quantile, probs = 0.025),
    ci_hi = apply(bt, 2, quantile, probs = 0.975),
    n_valid = nrow(bt),
    row.names = NULL
  )
  tab$excludes_zero <- tab$ci_lo > 0 | tab$ci_hi < 0
  tab$tie <- tie
  results[[tie]] <- list(table = tab, draws = bt)
  keep <- grepl("^(ind|dir|tot)_(Lis|A)_", tab$effect)
  print(transform(tab[keep, c("effect", "est", "boot_se", "ci_lo", "ci_hi", "excludes_zero")],
    est = round(est, 3), boot_se = round(boot_se, 3),
    ci_lo = round(ci_lo, 3), ci_hi = round(ci_hi, 3)), row.names = FALSE)
}

saveRDS(results, "cache/diagnostics/bootstrap_cfa_decomposition.rds")
cat("\nDone.\n")
