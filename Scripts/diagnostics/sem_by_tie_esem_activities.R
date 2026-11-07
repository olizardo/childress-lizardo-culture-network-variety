#' SEMs by tie strength with an ESEM (target-rotation) activities measurement block.
#'
#' Replaces the three-factor activities CFA block in sem_by_tie_script07.R with an
#' EFA block (set-ESEM) that lets all 20 activity items cross-load, with the varimax
#' assignment as the rotation target. The tie batteries keep their three-factor CFA
#' blocks. Answers the question: what happens to the structural estimates when the
#' activity measurement layer is the ESEM instead of the simple-structure CFA?
#'
#' Notes:
#' - EFA-block factor covariances are determined by the (oblique) rotation and are
#'   not specified separately; network-factor disturbances keep their covariances.
#' - Same sample, coding, and covariates as sem_by_tie_script07.R (N = 1,227,
#'   WLSMV, std.lv, fixed.x, education as one ordinal variable).
#' - Run from the project root: Rscript Scripts/diagnostics/sem_by_tie_esem_activities.R
#'   (writes cache/diagnostics/sem_by_tie_esem_activities.rds and a log to stdout).

suppressPackageStartupMessages({
  library(lavaan)
})

prep <- readRDS("cache/01_prepared_data.rds")
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df

act <- s07$activities
arts <- names(act$arts)
leis <- names(act$leisure)
resid_act <- names(act$residual)
all_act <- c(arts, leis, resid_act)

# Target matrix: 1 = free primary loading, 0 = target-to-zero (pst-style target).
# Columns follow the EFA block factor order A, Lis, R.
target_mat <- matrix(0, nrow = length(all_act), ncol = 3,
  dimnames = list(all_act, c("A", "Lis", "R")))
target_mat[arts, "A"] <- 1
target_mat[leis, "Lis"] <- 1
target_mat[resid_act, "R"] <- 1

df$educ_ord <- as.numeric(df$educ_cat)
df$educ_ord[is.na(df$educ_cat)] <- NA
covars <- c("gender_f", "educ_ord", "child_arts", "income", "age2", "race_f", "poli")

fit_tie <- function(tie) {
  s <- s07[[tie]]
  pre <- if (tie == "weak") "W" else "S"
  net_fac <- paste0(pre, c("v", "l", "c"))
  fac_items <- lapply(c("variety", "liberal", "conservative"), function(f) {
    items <- names(s[[f]])
    items[!grepl("^Know(men|women)_", items)]
  })
  net_items <- unlist(fac_items)
  net_meas <- paste(sprintf("%s =~ %s", net_fac, vapply(fac_items, paste, character(1), collapse = " + ")), collapse = "\n")

  need <- unique(c(all_act, net_items, covars))
  cc <- df[complete.cases(df[need]), need, drop = FALSE]
  cf <- cc[covars]
  cf[] <- lapply(cf, function(x) if (is.numeric(x)) x else factor(x))
  mm <- model.matrix(as.formula(paste("~", paste(covars, collapse = " + "))), data = cf)[, -1, drop = FALSE]
  dm <- as.data.frame(mm)
  names(dm) <- make.names(names(dm))
  cov_names <- names(dm)

  dat <- cbind(cc[c(all_act, net_items)], dm)
  dat[c(all_act, net_items)] <- lapply(dat[c(all_act, net_items)], function(x) as.numeric(unclass(x)))

  esem_block <- paste0(
    "efa(\"act\")*A + efa(\"act\")*Lis + efa(\"act\")*R =~ ",
    paste(all_act, collapse = " + "))
  demo_to_net <- paste(sprintf("%s ~ %s", net_fac, paste(cov_names, collapse = " + ")), collapse = "\n")
  demo_to_act <- paste(sprintf("%s ~ %s", c("A", "Lis", "R"), paste(c(net_fac, cov_names), collapse = " + ")), collapse = "\n")
  net_disturb <- paste(combn(net_fac, 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")

  syn <- paste(esem_block, net_meas, demo_to_net, demo_to_act, net_disturb, sep = "\n")
  fit <- sem(syn, data = dat, ordered = c(all_act, net_items), estimator = "WLSMV",
    std.lv = TRUE, fixed.x = TRUE,
    rotation = list("target", target = target_mat))
  list(fit = fit, net_fac = net_fac, cov_names = cov_names, n = nrow(dat), syntax = syn)
}

report_tie <- function(tie) {
  res <- fit_tie(tie)
  f <- res$fit
  cat(sprintf("\n===== %s ties with ESEM activities block, N = %d =====\n", tie, res$n))
  cat(sprintf("converged=%s npar=%d\n", lavInspect(f, "converged"), lavInspect(f, "npar")))
  cat(sprintf("CFI=%.3f TLI=%.3f RMSEA=%.3f [%.3f, %.3f] SRMR=%.3f\n",
    fitMeasures(f, "cfi.scaled"), fitMeasures(f, "tli.scaled"),
    fitMeasures(f, "rmsea.scaled"), fitMeasures(f, "rmsea.ci.lower.scaled"),
    fitMeasures(f, "rmsea.ci.upper.scaled"), fitMeasures(f, "srmr")))
  cat("Latent covariance matrix positive definite:", all(eigen(lavInspect(f, "cov.lv"))$values > 0), "\n")
  pe <- parameterEstimates(f, standardized = TRUE)
  paths <- pe[pe$op == "~" & pe$lhs %in% c("A", "Lis", "R") & pe$rhs %in% res$net_fac,
              c("lhs", "rhs", "est", "se", "pvalue", "std.all")]
  cat("\nNetwork-to-activity paths:\n")
  print(transform(paths, est = round(est, 3), se = round(se, 3), pvalue = signif(pvalue, 3),
                  std.all = round(std.all, 3)), row.names = FALSE)
  b <- coef(f)
  V <- vcov(f)
  for (o in c("A", "Lis", "R")) {
    idx <- match(sprintf("%s~%s", o, res$net_fac), names(b))
    bb <- b[idx]
    stat <- as.numeric(t(bb) %*% solve(V[idx, idx]) %*% bb)
    cat(sprintf("Joint Wald for %-4s network block: chi2(3) = %.2f, p = %.4g\n",
                o, stat, pchisq(stat, df = 3, lower.tail = FALSE)))
  }
  nc <- pe[pe$op == "~~" & pe$lhs %in% res$net_fac & pe$rhs %in% res$net_fac & pe$lhs != pe$rhs,
           c("lhs", "rhs", "std.all")]
  cat("\nNetwork factor residual correlations:\n")
  print(transform(nc[!duplicated(t(apply(nc[, c("lhs", "rhs")], 1, sort))), ], std.all = round(std.all, 3)),
        row.names = FALSE)
  demo <- pe[pe$op == "~" & pe$lhs %in% c(res$net_fac, "A", "Lis", "R") & pe$rhs %in% res$cov_names,
             c("lhs", "rhs", "est", "se", "pvalue")]
  cat("\nDemographic paths on activity factors:\n")
  print(transform(demo[demo$lhs %in% c("A", "Lis", "R"), ], est = round(est, 3), se = round(se, 3),
                  pvalue = signif(pvalue, 3)), row.names = FALSE)
  # EFA factor correlations (rotation-determined)
  cat("\nEFA activity factor correlations:\n")
  print(round(lavInspect(f, "cor.lv")[c("A", "Lis", "R"), c("A", "Lis", "R")], 3))
  invisible(res)
}

res_list <- list()
for (tie in c("weak", "strong")) res_list[[tie]] <- report_tie(tie)
saveRDS(res_list, "cache/diagnostics/sem_by_tie_esem_activities.rds")
cat("\nDone.\n")
