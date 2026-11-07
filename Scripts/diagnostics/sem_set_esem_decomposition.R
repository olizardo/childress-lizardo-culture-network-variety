#' Direct, network-mediated, and total effects under the separate set-ESEM
#' structural models (tie EFA block + activities EFA block).
#'
#' Refits the set-ESEM structural models with labeled paths so that indirect
#' effects are defined as sums of products of paths (a * b) with delta-method
#' standard errors, following the labeling scheme of sem_decomposition.R.
#' The labeled refit must reproduce the unlabeled set-ESEM estimates exactly;
#' the script checks this before reporting.
#'
#' Run from the project root: Rscript Scripts/diagnostics/sem_set_esem_decomposition.R
#' Writes cache/diagnostics/sem_set_esem_decomposition.rds and a log to stdout.

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
       target = target_for(items_list, fac_names), items = unlist(items_list))
}
weak <- tie_setup("weak")
strong <- tie_setup("strong")

df$educ_ord <- as.numeric(df$educ_cat)
df$educ_ord[is.na(df$educ_cat)] <- NA
covars <- c("gender_f", "educ_ord", "child_arts", "income", "age2", "race_f", "poli")

covs <- c(educ_ord = "educ", child_arts = "arts", poli = "poli")
act_fac <- c("A", "Lis", "R")

label_syntax <- function(syn, net_fac) {
  lines <- strsplit(syn, "\n")[[1]]
  vapply(lines, function(line) {
    if (grepl(paste0("^(", paste(net_fac, collapse = "|"), ") ~ "), line)) {
      f <- sub(" ~.*$", "", line)
      rhs <- sub("^[^~]*~ ", "", line)
      for (cv in names(covs)) rhs <- gsub(paste0("\\b", cv, "\\b"), sprintf("a_%s_%s*%s", f, covs[[cv]], cv), rhs)
      return(paste(f, "~", rhs))
    }
    if (grepl("^(A|Lis|R) ~ ", line)) {
      o <- sub(" ~.*$", "", line)
      rhs <- sub("^[^~]*~ ", "", line)
      for (nf in net_fac) rhs <- gsub(paste0("\\b", nf, "\\b"), sprintf("b_%s_%s*%s", o, nf, nf), rhs)
      for (cv in names(covs)) rhs <- gsub(paste0("\\b", cv, "\\b"), sprintf("d_%s_%s*%s", o, covs[[cv]], cv), rhs)
      return(paste(o, "~", rhs))
    }
    line
  }, character(1)) |> paste(collapse = "\n")
}

add_defs <- function(syn, net_fac) {
  defs <- c()
  for (o in act_fac) for (cv in names(covs)) {
    ind_terms <- sprintf("a_%s_%s*b_%s_%s", net_fac, covs[[cv]], o, net_fac)
    defs <- c(defs,
      sprintf("ind_%s_%s := %s", o, covs[[cv]], paste(ind_terms, collapse = " + ")),
      sprintf("dir_%s_%s := d_%s_%s", o, covs[[cv]], o, covs[[cv]]),
      sprintf("tot_%s_%s := dir_%s_%s + ind_%s_%s", o, covs[[cv]], o, covs[[cv]], o, covs[[cv]]))
  }
  paste(syn, paste(defs, collapse = "\n"), sep = "\n")
}

run_tie <- function(tie_obj, tname) {
  need <- unique(c(all_act, tie_obj$items, covars))
  cc <- df[complete.cases(df[need]), need, drop = FALSE]
  cf <- cc[covars]
  cf[] <- lapply(cf, function(x) if (is.numeric(x)) x else factor(x))
  mm <- model.matrix(as.formula(paste("~", paste(covars, collapse = " + "))), data = cf)[, -1, drop = FALSE]
  dm <- as.data.frame(mm)
  names(dm) <- make.names(names(dm))
  cov_names <- names(dm)
  dat <- cbind(cc[need[need %in% c(all_act, tie_obj$items)]], dm)
  cols <- c(all_act, tie_obj$items)
  dat[cols] <- lapply(cols, function(cn) as.numeric(unclass(dat[[cn]])))

  efa_block_syntax <- function(label, fac_names, items) {
    mods <- paste(sprintf("efa(\"%s\")*%s", label, fac_names), collapse = " + ")
    sprintf("%s =~ %s", mods, paste(items, collapse = " + "))
  }
  demo_to_net <- paste(sprintf("%s ~ %s", tie_obj$fac_names, paste(cov_names, collapse = " + ")), collapse = "\n")
  demo_to_act <- paste(sprintf("%s ~ %s", act_fac, paste(c(tie_obj$fac_names, cov_names), collapse = " + ")), collapse = "\n")
  net_disturb <- paste(combn(tie_obj$fac_names, 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")
  act_disturb <- paste(combn(act_fac, 2, function(p) sprintf("%s ~~ %s", p[1], p[2])), collapse = "\n")
  syn <- paste(efa_block_syntax("act", c("A", "Lis", "R"), all_act),
    efa_block_syntax(tname, tie_obj$fac_names, tie_obj$items),
    demo_to_net, demo_to_act, net_disturb, act_disturb, sep = "\n")
  target_list <- list(act = act_target)
  target_list[[tname]] <- tie_obj$target

  fit <- sem(syn, data = dat, ordered = cols, estimator = "WLSMV",
    std.lv = TRUE, fixed.x = TRUE, rotation = list("target", target = target_list))
  syn_lab <- add_defs(label_syntax(syn, tie_obj$fac_names), tie_obj$fac_names)
  fit_lab <- sem(syn_lab, data = dat, ordered = cols, estimator = "WLSMV",
    std.lv = TRUE, fixed.x = TRUE, rotation = list("target", target = target_list))

  # Check the labeled refit reproduces the unlabeled fit
  p1 <- parameterEstimates(fit, standardized = TRUE)
  p2 <- parameterEstimates(fit_lab, standardized = TRUE)
  key1 <- paste(p1$lhs, p1$op, p1$rhs)
  key2 <- paste(p2$lhs, p2$op, p2$rhs)
  common <- intersect(key1, key2)
  dmax <- max(abs(p1$est[match(common, key1)] - p2$est[match(common, key2)]))
  cat(sprintf("\n===== %s ties (set-ESEM decomposition), N = %d =====\n", tname, nrow(dat)))
  cat(sprintf("Labeled-refit max estimate difference: %.2e (must be ~0)\n", dmax))
  stopifnot(dmax < 1e-6)

  defs <- p2[p2$op == ":=", c("lhs", "est", "se", "pvalue", "std.all")]
  parts <- do.call(rbind, strsplit(defs$lhs, "_"))
  defs$effect <- parts[, 1]
  defs$outcome <- parts[, 2]
  defs$covariate <- apply(cbind(parts[, 3], ifelse(ncol(parts) > 3, parts[, 4], "")), 1, paste, collapse = "")
  defs$covariate <- ifelse(defs$covariate == "educ", "Education (ordinal)",
    ifelse(defs$covariate == "arts", "Childhood arts exposure",
      ifelse(defs$covariate == "poli", "Political conservatism", defs$covariate)))
  print(transform(defs[, c("outcome", "covariate", "effect", "est", "se", "pvalue")],
    est = round(est, 3), se = round(se, 3), pvalue = signif(pvalue, 3)), row.names = FALSE)

  out_obj <- list(fit = fit, fit_lab = fit_lab, defs = defs, n = nrow(dat))
  out_obj
}

out <- list()
out$weak <- run_tie(weak, "weak")
out$strong <- run_tie(strong, "strong")
saveRDS(out, "cache/diagnostics/sem_set_esem_decomposition.rds")
cat("\nDone.\n")
