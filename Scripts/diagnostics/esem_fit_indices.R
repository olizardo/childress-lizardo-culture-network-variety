#' Recover robust fit indices for the set-ESEM structural models and test the
#' antonym residual covariances alongside the tie EFA blocks.
#'
#' Background: the separate set-ESEM models (sem_set_esem_full.R) converged with
#' positive definite latent covariances and acceptable SRMR (.037/.040), but lavaan
#' could not invert the information matrix for the robust test statistic, so CFI and
#' RMSEA came back NA. This script attempts:
#'   (1) information = "expected" (WLSMV keeps the robust correction but uses the
#'       expected information for the weight matrix);
#'   (2) information = "default" with the observed-information refit as baseline;
#'   (3) antonym residual covariances (very liberal ~~ very conservative,
#'       little church ~~ lots of church, city ~~ rural) specified alongside the
#'       tie EFA blocks, to see whether the CFA-era antonym sensitivity carries
#'       over to the ESEM specification.
#'
#' Run from the project root: Rscript Scripts/diagnostics/esem_fit_indices.R
#' Writes cache/diagnostics/esem_fit_indices.rds and a log to stdout.

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

target_for <- function(fac_items, fac_names) {
  items <- unlist(fac_items)
  m <- matrix(0, nrow = length(items), ncol = length(fac_names),
    dimnames = list(items, fac_names))
  for (k in seq_along(fac_items)) m[fac_items[[k]], fac_names[k]] <- 1
  m
}

act_target <- target_for(list(arts, leis, resid_act), c("A", "Lis", "R"))

tie_setup <- function(tie) {
  s <- s07[[tie]]
  pre <- if (tie == "weak") "W" else "S"
  fac_names <- paste0(pre, c("v", "l", "c"))
  items_list <- lapply(c("variety", "liberal", "conservative"), function(f) {
    items <- names(s[[f]])
    items[!grepl("^Know(men|women)_", items)]
  })
  list(items_list = items_list, fac_names = fac_names,
       target = target_for(items_list, fac_names),
       items = unlist(items_list))
}
weak <- tie_setup("weak")
strong <- tie_setup("strong")

df$educ_ord <- as.numeric(df$educ_cat)
df$educ_ord[is.na(df$educ_cat)] <- NA
covars <- c("gender_f", "educ_ord", "child_arts", "income", "age2", "race_f", "poli")

make_data <- function(need) {
  cc <- df[complete.cases(df[need]), need, drop = FALSE]
  cf <- cc[covars]
  cf[] <- lapply(cf, function(x) if (is.numeric(x)) x else factor(x))
  mm <- model.matrix(as.formula(paste("~", paste(covars, collapse = " + "))), data = cf)[, -1, drop = FALSE]
  dm <- as.data.frame(mm)
  names(dm) <- make.names(names(dm))
  cov_names <- names(dm)
  dat <- cbind(cc[need[need %in% c(all_act, weak$items, strong$items)]], dm)
  cols <- c(all_act, weak$items, strong$items)
  dat[cols[cols %in% names(dat)]] <- lapply(cols[cols %in% names(dat)], function(cn) as.numeric(unclass(dat[[cn]])))
  list(dat = dat, cov_names = cov_names, n = nrow(dat))
}

efa_block_syntax <- function(label, fac_names, items) {
  mods <- paste(sprintf("efa(\"%s\")*%s", label, fac_names), collapse = " + ")
  sprintf("%s =~ %s", mods, paste(items, collapse = " + "))
}

structural <- function(net_fac, cov_names) {
  demo_to_net <- paste(sprintf("%s ~ %s", net_fac, paste(cov_names, collapse = " + ")), collapse = "\n")
  demo_to_act <- paste(sprintf("%s ~ %s", c("A", "Lis", "R"), paste(c(net_fac, cov_names), collapse = " + ")), collapse = "\n")
  net_disturb <- paste(combn(net_fac, 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")
  act_disturb <- paste(combn(c("A", "Lis", "R"), 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")
  paste(demo_to_net, demo_to_act, net_disturb, act_disturb, sep = "\n")
}

antonym_syntax <- function(tie_items) {
  paste(sprintf("Know%s_%s ~~ Know%s_%s",
    c("VeryLib", "LittleChurch", "Cityppl"), tie_items,
    c("VeryCons", "LotsaChurch", "Ruralppl"), tie_items), collapse = "\n")
}

out <- list()

for (tie in list(weak, strong)) {
  tname <- if (identical(tie, weak)) "weak" else "strong"
  need <- unique(c(all_act, tie$items, covars))
  d <- make_data(need)
  block_label <- tname
  target_list <- list(act = act_target)
  target_list[[block_label]] <- tie$target

  base_syn <- paste(
    efa_block_syntax("act", c("A", "Lis", "R"), all_act),
    efa_block_syntax(block_label, tie$fac_names, tie$items),
    structural(tie$fac_names, d$cov_names),
    sep = "\n")

  for (info in c("default", "expected")) {
    lab <- sprintf("%s, information = %s", tname, info)
    cat(sprintf("\n===== %s =====\n", lab))
    fit <- tryCatch(
      sem(base_syn, data = d$dat, ordered = c(all_act, tie$items), estimator = "WLSMV",
        std.lv = TRUE, fixed.x = TRUE, information = info,
        rotation = list("target", target = target_list)),
      error = function(e) e)
    if (inherits(fit, "error")) {
      cat(sprintf("FAILED: %s\n", conditionMessage(fit)))
      out[[lab]] <- list(error = conditionMessage(fit))
      next
    }
    fm <- fitMeasures(fit, c("chisq.scaled", "df", "cfi.scaled", "tli.scaled",
      "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr"))
    cat(sprintf("converged=%s chi2=%s df=%s CFI=%s RMSEA=%s [%.3f, %.3f] SRMR=%.3f\n",
      lavInspect(fit, "converged"),
      ifelse(is.na(fm[1]), "NA", sprintf("%.1f", fm[1])),
      ifelse(is.na(fm[2]), "NA", fm[2]),
      ifelse(is.na(fm[3]), "NA", sprintf("%.3f", fm[3])),
      ifelse(is.na(fm[5]), "NA", sprintf("%.3f", fm[5])),
      fm[6], fm[7], fm[8]))
    pe <- parameterEstimates(fit, standardized = TRUE)
    paths <- pe[pe$op == "~" & pe$lhs %in% c("A", "Lis", "R") & pe$rhs %in% tie$fac_names, ]
    cat("Network-to-activity paths (est, se, p, std.all):\n")
    print(transform(paths[, c("lhs", "rhs", "est", "se", "pvalue", "std.all")],
      est = round(est, 3), se = round(se, 3), pvalue = signif(pvalue, 3),
      std.all = round(std.all, 3)), row.names = FALSE)
    out[[lab]] <- list(fit = fit, fitmeasures = fm)
  }

  # Antonym residuals alongside the EFA blocks
  lab <- sprintf("%s, antonym residuals", tname)
  cat(sprintf("\n===== %s =====\n", lab))
  syn_ant <- paste(base_syn, antonym_syntax(if (tname == "weak") "weak" else "strong"), sep = "\n")
  fit <- tryCatch(
    sem(syn_ant, data = d$dat, ordered = c(all_act, tie$items), estimator = "WLSMV",
      std.lv = TRUE, fixed.x = TRUE,
      rotation = list("target", target = target_list)),
    error = function(e) e)
  if (inherits(fit, "error")) {
    cat(sprintf("FAILED: %s\n", conditionMessage(fit)))
    out[[lab]] <- list(error = conditionMessage(fit))
  } else {
    fm <- fitMeasures(fit, c("chisq.scaled", "df", "cfi.scaled", "tli.scaled",
      "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr"))
    cat(sprintf("converged=%s chi2=%s df=%s CFI=%s RMSEA=%s [%.3f, %.3f] SRMR=%.3f\n",
      lavInspect(fit, "converged"),
      ifelse(is.na(fm[1]), "NA", sprintf("%.1f", fm[1])),
      ifelse(is.na(fm[2]), "NA", fm[2]),
      ifelse(is.na(fm[3]), "NA", sprintf("%.3f", fm[3])),
      ifelse(is.na(fm[5]), "NA", sprintf("%.3f", fm[5])),
      fm[6], fm[7], fm[8]))
    pe <- parameterEstimates(fit, standardized = TRUE)
    res <- pe[pe$op == "~~" & pe$lhs != pe$rhs & grepl("^Know", pe$lhs) & grepl("^Know", pe$rhs),
              c("lhs", "rhs", "est", "se", "pvalue")]
    cat("Antonym residual covariances:\n")
    print(transform(res, est = round(est, 3), se = round(se, 3), pvalue = signif(pvalue, 3)),
          row.names = FALSE)
    paths <- pe[pe$op == "~" & pe$lhs %in% c("A", "Lis", "R") & pe$rhs %in% tie$fac_names, ]
    cat("Network-to-activity paths (est, se, p, std.all):\n")
    print(transform(paths[, c("lhs", "rhs", "est", "se", "pvalue", "std.all")],
      est = round(est, 3), se = round(se, 3), pvalue = signif(pvalue, 3),
      std.all = round(std.all, 3)), row.names = FALSE)
    eig <- eigen(lavInspect(fit, "cov.lv"))$values
    cat("Latent covariance positive definite:", all(eig > 0),
      sprintf("(min eigenvalue %.4f)\n", min(eig)))
    out[[lab]] <- list(fit = fit, fitmeasures = fm)
  }
}

saveRDS(out, "cache/diagnostics/esem_fit_indices.rds")
cat("\nDone.\n")
