suppressPackageStartupMessages(library(lavaan))
setwd("/home/omarlizardo/projects/CULTURE/childress-lizardo-culture-network-variety")
prep <- readRDS("cache/01_prepared_data.rds")
str07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df
run <- function(drop_gender) {
  dg <- function(x) if (drop_gender) x[!grepl("^Know(men|women)_", x)] else x
  wv <- dg(names(str07$weak$variety)); wl <- dg(names(str07$weak$liberal)); wc <- dg(names(str07$weak$conservative))
  sv <- dg(names(str07$strong$variety)); sl <- dg(names(str07$strong$liberal)); sc <- dg(names(str07$strong$conservative))
  all_items <- c(wv, wl, wc, sv, sl, sc); weak_items <- c(wv, wl, wc); strong_items <- c(sv, sl, sc)
  dat <- df[complete.cases(df[all_items]), all_items, drop = FALSE]
  dat[] <- lapply(dat, function(x) as.numeric(unclass(x)))
  resid <- paste(sprintf("%s ~~ %s", weak_items, sub("_weak$", "_strong", weak_items)), collapse = "\n")
  bif <- paste0("G =~ ", paste(all_items, collapse = " + "), "\n",
    "L =~ ", paste(c(wl, sl), collapse = " + "), "\n",
    "C =~ ", paste(c(wc, sc), collapse = " + "), "\n", "G ~~ 0*L\nG ~~ 0*C\n")
  meth <- paste0("Mw =~ ", paste(weak_items, collapse = " + "), "\n",
    "Ms =~ ", paste(strong_items, collapse = " + "), "\n",
    "Mw ~~ 0*G\nMw ~~ 0*L\nMw ~~ 0*C\nMs ~~ 0*G\nMs ~~ 0*L\nMs ~~ 0*C\n")
  specs <- list(
    "5. Bifactor, correlated" = paste0(bif, "L ~~ C\n"),
    "6. Bifactor orth + resid" = paste0(bif, "L ~~ 0*C\n", resid),
    "7. Bifactor corr + resid" = paste0(bif, "L ~~ C\n", resid),
    "8. Bifactor corr + method" = paste0(bif, "L ~~ C\n", meth),
    "9. Bifactor corr + method + resid" = paste0(bif, "L ~~ C\n", meth, resid))
  out <- lapply(names(specs), function(nm) {
    f <- tryCatch(cfa(specs[[nm]], data = dat, ordered = all_items, estimator = "WLSMV", std.lv = TRUE), error = function(e) NULL)
    if (is.null(f)) return(data.frame(model = nm, cond = drop_gender, CFI = NA, RMSEA = NA, LC_r = NA, PD = NA))
    pe <- parameterEstimates(f, standardized = TRUE)
    lc <- pe$std.all[pe$op == "~~" & pe$lhs == "L" & pe$rhs == "C"]
    data.frame(model = nm, cond = if (drop_gender) "no gender" else "all items",
      CFI = round(fitMeasures(f, "cfi.scaled"), 3), RMSEA = round(fitMeasures(f, "rmsea.scaled"), 3),
      LC_r = round(lc, 2), PD = all(eigen(lavInspect(f, "cov.lv"))$values > 0), row.names = NULL)
  })
  do.call(rbind, out)
}
print(rbind(run(FALSE), run(TRUE)), row.names = FALSE)
