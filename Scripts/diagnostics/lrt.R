suppressPackageStartupMessages(library(lavaan))
setwd("/home/omarlizardo/projects/CULTURE/childress-lizardo-culture-network-variety")
prep <- readRDS("cache/01_prepared_data.rds")
str07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df
spec_str <- function(ibf) paste(sapply(names(ibf), function(fid)
  sprintf("%s =~ %s", fid, paste(ibf[[fid]], collapse = " + "))), collapse = "\n")
prep_dat <- function(items) {
  d <- df[complete.cases(df[items]), items, drop = FALSE]
  d[] <- lapply(d, function(x) as.numeric(unclass(x)))
  d
}
for (nm in c("weak", "strong")) {
  ibf <- lapply(str07[[nm]], names)
  merged <- list(variety = ibf$variety, composition = c(ibf$liberal, ibf$conservative))
  dat <- prep_dat(unlist(ibf))
  f3 <- cfa(spec_str(ibf), data = dat, ordered = unlist(ibf), estimator = "WLSMV", std.lv = TRUE)
  f2 <- cfa(spec_str(merged), data = dat, ordered = unlist(merged), estimator = "WLSMV", std.lv = TRUE)
  cat("\n==", nm, "battery, N =", nrow(dat), "==\n")
  print(lavTestLRT(f3, f2))
  cat("Robust chi-sq 3-factor:", round(fitMeasures(f3, c("chisq.scaled", "df.scaled", "pvalue.scaled")), 3), "\n")
  cat("Robust chi-sq merged:  ", round(fitMeasures(f2, c("chisq.scaled", "df.scaled", "pvalue.scaled")), 3), "\n")
}
