suppressPackageStartupMessages(library(lavaan))
prep <- readRDS("cache/01_prepared_data.rds")
df <- prep$df
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
act <- s07$activities
arts <- names(act$arts)
covars <- c("gender_f","educ_cat","child_arts","income","age2","race_f","poli")
w <- list(v = names(s07$weak$variety), l = names(s07$weak$liberal), c = names(s07$weak$conservative))
s <- list(v = names(s07$strong$variety), l = names(s07$strong$liberal), c = names(s07$strong$conservative))
items <- c(unlist(w), unlist(s))
items32 <- items[!grepl("^Know(men|women)_", items)]
liberal_items <- c(w$l, s$l)[!grepl("^Know(men|women)_", c(w$l, s$l))]
conservative_items <- c(w$c, s$c)[!grepl("^Know(men|women)_", c(w$c, s$c))]
weak_items <- items32[grepl("_weak$", items32)]
pairs <- data.frame(w = weak_items, s = sub("_weak$", "_strong", weak_items), stringsAsFactors = FALSE)
pairs <- pairs[pairs$s %in% items32, ]
meas <- paste0(
  "G =~ ", paste(items32, collapse = " + "), "\n",
  "L =~ ", paste(liberal_items, collapse = " + "), "\n",
  "C =~ ", paste(conservative_items, collapse = " + "), "\n",
  "G ~~ 0*L\nG ~~ 0*C\n",
  paste(sprintf("%s ~~ %s", pairs$w, pairs$s), collapse = "\n"), "\n")
leis_alt <- c("go_for_walk", "exercise_or_yoga", "hiking_camping_boating")
need <- unique(c(arts, items32, leis_alt, covars, "educ", "child_arts"))
cc <- df[complete.cases(df[intersect(need, names(df))]), , drop = FALSE]
cf <- cc[covars]; cf[] <- lapply(cf, function(x) if (is.numeric(x)) x else factor(x))
mm <- model.matrix(as.formula(paste("~", paste(covars, collapse = " + "))), data = cf)[, -1, drop = FALSE]
dm <- as.data.frame(mm); names(dm) <- make.names(names(dm))
cov_names <- names(dm)
dat <- cbind(cc[unique(c(arts, items32, leis_alt))], dm)
dat[unique(c(arts, items32, leis_alt))] <- lapply(dat[unique(c(arts, items32, leis_alt))], function(x) as.numeric(unclass(x)))
cat("analytic N =", nrow(cc), "\n")
run <- function(label, leis_block, obs_outcome = NULL) {
  arts_line <- sprintf("A =~ %s\n", paste(arts, collapse = " + "))
  if (is.null(obs_outcome)) {
    lat <- paste0(arts_line, "Lis =~ ", paste(leis_block, collapse = " + "), "\n",
                  "A ~ G + L + C + ", paste(cov_names, collapse = " + "), "\n",
                  "Lis ~ G + L + C + ", paste(cov_names, collapse = " + "), "\n")
    ord <- c(arts, items32, leis_block)
    syn <- paste(meas, lat, sep = "\n")
  } else {
    lat <- paste0(arts_line,
                  "A ~ G + L + C + ", paste(cov_names, collapse = " + "), "\n",
                  obs_outcome, " ~ G + L + C + ", paste(cov_names, collapse = " + "), "\n")
    ord <- c(arts, items32, obs_outcome)
    syn <- paste(meas, lat, sep = "\n")
  }
  fit <- sem(syn, data = dat, ordered = unique(ord), estimator = "WLSMV", std.lv = TRUE, fixed.x = TRUE)
  pe <- parameterEstimates(fit)
  rows <- if (is.null(obs_outcome)) pe[pe$op == "~" & pe$lhs == "Lis" & pe$rhs %in% c("G","L","C"), ] else pe[pe$op == "~" & pe$lhs == obs_outcome & pe$rhs %in% c("G","L","C"), ]
  cat(sprintf("\n%s: converged=%s CFI=%.3f RMSEA=%.3f\n", label, lavInspect(fit, "converged"),
      fitMeasures(fit, "cfi.scaled"), fitMeasures(fit, "rmsea.scaled")))
  for (i in seq_len(nrow(rows))) cat(sprintf("  %s on %s: b = %.3f (SE %.3f), p = %.3g\n", label, rows$rhs[i], rows$est[i], rows$se[i], rows$pvalue[i]))
}
run("Single item: walking observed", NULL, obs_outcome = "go_for_walk")
run("Three items: walk + exercise + hiking/camping", c("go_for_walk", "exercise_or_yoga", "hiking_camping_boating"))
