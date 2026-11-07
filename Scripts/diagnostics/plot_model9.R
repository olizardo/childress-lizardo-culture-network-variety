suppressPackageStartupMessages({library(lavaan); library(semPlot)})
setwd("/home/omarlizardo/projects/CULTURE/childress-lizardo-culture-network-variety")
prep <- readRDS("cache/01_prepared_data.rds")
str07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df
wv <- names(str07$weak$variety); wl <- names(str07$weak$liberal); wc <- names(str07$weak$conservative)
sv <- names(str07$strong$variety); sl <- names(str07$strong$liberal); sc <- names(str07$strong$conservative)
all_items <- c(wv, wl, wc, sv, sl, sc)
weak_items <- c(wv, wl, wc); strong_items <- c(sv, sl, sc)
dat <- df[complete.cases(df[all_items]), all_items, drop = FALSE]
dat[] <- lapply(dat, function(x) as.numeric(unclass(x)))
resid <- paste(sprintf("%s ~~ %s", weak_items, sub("_weak$", "_strong", weak_items)), collapse = "\n")
bif <- paste0("G =~ ", paste(all_items, collapse = " + "), "\n",
  "L =~ ", paste(c(wl, sl), collapse = " + "), "\n",
  "C =~ ", paste(c(wc, sc), collapse = " + "), "\n", "G ~~ 0*L\nG ~~ 0*C\nL ~~ C\n")
meth <- paste0("Mw =~ ", paste(weak_items, collapse = " + "), "\n",
  "Ms =~ ", paste(strong_items, collapse = " + "), "\n",
  "Mw ~~ 0*G\nMw ~~ 0*L\nMw ~~ 0*C\nMs ~~ 0*G\nMs ~~ 0*L\nMs ~~ 0*C\n")
fit9 <- cfa(paste0(bif, meth, resid), data = dat, ordered = all_items, estimator = "WLSMV", std.lv = TRUE)
cat("converged:", lavInspect(fit9, "converged"), " CFI:", round(fitMeasures(fit9, "cfi.scaled"), 3), "\n")
dir.create("Plots/diagnostics", showWarnings = FALSE, recursive = TRUE)
png("Plots/diagnostics/model9_path_diagram.png", width = 3000, height = 3000, res = 200)
semPaths(fit9, what = "std", whatLabels = "none", layout = "circle", style = "lisrel",
  nCharNodes = 0, sizeMan = 1.6, sizeLat = 9, residuals = FALSE, edge.label.cex = 0,
  intercepts = FALSE, curve = 0, rotation = 1, title = FALSE, mar = c(2, 2, 2, 2),
  nodeLabels = NULL)
dev.off()
cat("saved Plots/diagnostics/model9_path_diagram.png\n")
