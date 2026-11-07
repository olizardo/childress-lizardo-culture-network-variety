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
  resid <- paste(sprintf("%s ~~ %s", weak_items, sub("_weak$", "_strong", weak_items)), collapse = "\n")
  dat <- df[complete.cases(df[all_items]), all_items, drop = FALSE]
  dat[] <- lapply(dat, function(x) as.numeric(unclass(x)))
  bif <- paste0("G =~ ", paste(all_items, collapse = " + "), "\n",
    "L =~ ", paste(c(wl, sl), collapse = " + "), "\n",
    "C =~ ", paste(c(wc, sc), collapse = " + "), "\n", "G ~~ 0*L\nG ~~ 0*C\n")
  meth <- paste0("Mw =~ ", paste(weak_items, collapse = " + "), "\n",
    "Ms =~ ", paste(strong_items, collapse = " + "), "\n",
    "Mw ~~ 0*G\nMw ~~ 0*L\nMw ~~ 0*C\nMs ~~ 0*G\nMs ~~ 0*L\nMs ~~ 0*C\n")
  models <- list(
    "7. Bifactor correlated + weak-strong residuals" = paste0(bif, "L ~~ C\n", resid),
    "8. Bifactor correlated + battery method factors" = paste0(bif, "L ~~ C\n", meth),
    "9. Bifactor correlated + battery method factors + residuals" = paste0(bif, "L ~~ C\n", meth, resid)
  )
  fits <- lapply(models, function(s) tryCatch(cfa(s, data = dat, ordered = all_items,
    estimator = "WLSMV", std.lv = TRUE), error = function(e) NULL))
  tab <- do.call(rbind, lapply(names(fits), function(nm) {
    f <- fits[[nm]]
    if (is.null(f)) return(data.frame(model = nm, df = NA, CFI = NA, RMSEA = NA, SRMR = NA, PD = NA, conv = NA))
    pe <- parameterEstimates(f, standardized = TRUE)
    mw_ms <- pe$std.all[pe$op == "~~" & pe$lhs == "Mw" & pe$rhs == "Ms"]
    data.frame(model = nm, df = fitMeasures(f, "df.scaled"),
      CFI = round(fitMeasures(f, "cfi.scaled"), 3), RMSEA = round(fitMeasures(f, "rmsea.scaled"), 3),
      SRMR = round(fitMeasures(f, "srmr"), 3),
      PD = all(eigen(lavInspect(f, "cov.lv"))$values > 0), conv = lavInspect(f, "converged"),
      Mw_Ms_r = if (length(mw_ms)) round(mw_ms, 2) else NA, row.names = NULL)
  }))
  diffs <- list(
    "7 -> 8 (method factors instead of residuals)" = c("7. Bifactor correlated + weak-strong residuals", "8. Bifactor correlated + battery method factors"),
    "8 -> 9 (residuals added to method model)" = c("8. Bifactor correlated + battery method factors", "9. Bifactor correlated + battery method factors + residuals"))
  dtab <- do.call(rbind, lapply(names(diffs), function(lbl) {
    a <- fits[[diffs[[lbl]][1]]]; b <- fits[[diffs[[lbl]][2]]]
    if (is.null(a) || is.null(b)) return(NULL)
    r <- tryCatch(anova(a, b), error = function(e) NULL)
    if (is.null(r)) return(NULL)
    data.frame(comparison = lbl, dChisq = round(r$`Chisq diff`[2], 1), ddf = r$`Df diff`[2],
      p = signif(r$`Pr(>Chisq)`[2], 2), row.names = NULL)
  }))
  list(tab = tab, dtab = dtab, n = nrow(dat), items = length(all_items))
}

for (flag in c(FALSE, TRUE)) {
  res <- run_condition(flag)
  cat(sprintf("\n### %s, N = %d, %d items ###\n", if (flag) "No gender items" else "All items", res$n, res$items))
  print(res$tab, row.names = FALSE)
  cat("\nNested comparisons:\n")
  print(res$dtab, row.names = FALSE)
}
