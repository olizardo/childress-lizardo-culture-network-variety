suppressPackageStartupMessages(library(lavaan))
setwd("/home/omarlizardo/projects/CULTURE/childress-lizardo-culture-network-variety")
prep <- readRDS("cache/01_prepared_data.rds")
str07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df
drop_g <- function(x, flag) if (flag) x[!grepl("^Know(men|women)_", x)] else x

run_condition <- function(drop_gender) {
  wv <- drop_g(names(str07$weak$variety), drop_gender); wl <- drop_g(names(str07$weak$liberal), drop_gender)
  wc <- drop_g(names(str07$weak$conservative), drop_gender)
  sv <- drop_g(names(str07$strong$variety), drop_gender); sl <- drop_g(names(str07$strong$liberal), drop_gender)
  sc <- drop_g(names(str07$strong$conservative), drop_gender)
  all_items <- c(wv, wl, wc, sv, sl, sc)
  weak_items <- c(wv, wl, wc); strong_items <- c(sv, sl, sc)
  pair_w <- weak_items; pair_s <- sub("_weak$", "_strong", weak_items)
  stopifnot(all(pair_s %in% strong_items))
  resid <- paste(sprintf("%s ~~ %s", pair_w, pair_s), collapse = "\n")
  dat <- df[complete.cases(df[all_items]), all_items, drop = FALSE]
  dat[] <- lapply(dat, function(x) as.numeric(unclass(x)))

  sub_pooled <- paste0("V =~ ", paste(c(wv, sv), collapse = " + "), "\n",
    "L =~ ", paste(c(wl, sl), collapse = " + "), "\n",
    "C =~ ", paste(c(wc, sc), collapse = " + "))
  one_factor <- paste0("G =~ ", paste(all_items, collapse = " + "))
  method <- paste0(
    "Mw =~ ", paste(weak_items, collapse = " + "), "\n",
    "Ms =~ ", paste(strong_items, collapse = " + "), "\n",
    "Mw ~~ 0*V\nMw ~~ 0*L\nMw ~~ 0*C\nMs ~~ 0*V\nMs ~~ 0*L\nMs ~~ 0*C\n")
  bif_corr <- paste0("G =~ ", paste(all_items, collapse = " + "), "\n",
    "L =~ ", paste(c(wl, sl), collapse = " + "), "\n",
    "C =~ ", paste(c(wc, sc), collapse = " + "), "\n", "G ~~ 0*L\nG ~~ 0*C\n")
  bif_base <- paste0(bif_corr, "L ~~ 0*C\n")
  models <- list(
    "1. One general factor" = one_factor,
    "2. Pooled 3-factor, correlated" = sub_pooled,
    "3. Pooled 3-factor + battery method factors" = paste0(sub_pooled, "\n", method),
    "4. Bifactor, composition orthogonal" = bif_base,
    "5. Bifactor, composition correlated" = bif_corr,
    "6. Bifactor orthogonal + weak-strong residuals" = paste0(bif_base, resid),
    "7. Bifactor correlated + weak-strong residuals" = paste0(bif_corr, resid)
  )
  fits <- lapply(models, function(s)
    tryCatch(cfa(s, data = dat, ordered = all_items, estimator = "WLSMV", std.lv = TRUE), error = function(e) NULL))
  tab <- do.call(rbind, lapply(names(fits), function(nm) {
    f <- fits[[nm]]
    if (is.null(f)) return(data.frame(model = nm, df = NA, CFI = NA, RMSEA = NA, SRMR = NA, PD = NA, conv = FALSE))
    data.frame(model = nm, df = fitMeasures(f, "df.scaled"),
      CFI = round(fitMeasures(f, "cfi.scaled"), 3), RMSEA = round(fitMeasures(f, "rmsea.scaled"), 3),
      SRMR = round(fitMeasures(f, "srmr"), 3),
      PD = all(eigen(lavInspect(f, "cov.lv"))$values > 0), conv = lavInspect(f, "converged"),
      row.names = NULL)
  }))
  nest <- list(c("1. One general factor", "2. Pooled 3-factor, correlated"),
    c("2. Pooled 3-factor, correlated", "3. Pooled 3-factor + battery method factors"),
    c("4. Bifactor, composition orthogonal", "5. Bifactor, composition correlated"),
    c("4. Bifactor, composition orthogonal", "6. Bifactor orthogonal + weak-strong residuals"),
    c("4. Bifactor, composition orthogonal", "6. Bifactor orthogonal + weak-strong residuals"),
    c("5. Bifactor, composition correlated", "7. Bifactor correlated + weak-strong residuals"),
    c("6. Bifactor orthogonal + weak-strong residuals", "7. Bifactor correlated + weak-strong residuals"),
    c("6. Bifactor orthogonal + weak-strong residuals", "7. Bifactor correlated + weak-strong residuals"))
  diffs <- do.call(rbind, lapply(nest, function(p) {
    a <- fits[[p[1]]]; b <- fits[[p[2]]]
    if (is.null(a) || is.null(b)) return(NULL)
    r <- tryCatch(anova(a, b), error = function(e) NULL)
    if (is.null(r)) return(NULL)
    data.frame(simpler = p[1], richer = p[2], scaled_chisq_diff = round(r$`Chisq diff`[2], 1),
      df_diff = r$`Df diff`[2], p = signif(r$`Pr(>Chisq)`[2], 2), row.names = NULL)
  }))
  list(tab = tab, diffs = diffs, n = nrow(dat), items = length(all_items))
}

for (flag in c(FALSE, TRUE)) {
  res <- run_condition(flag)
  cat(sprintf("\n### %s, N = %d, %d items ###\n", if (flag) "No gender items" else "All items",
    res$n, res$items))
  print(res$tab, row.names = FALSE)
  cat("\nNested comparisons (scaled difference tests):\n")
  print(res$diffs, row.names = FALSE)
}
