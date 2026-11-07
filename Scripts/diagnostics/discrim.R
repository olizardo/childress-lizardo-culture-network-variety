suppressPackageStartupMessages(library(lavaan))
setwd("/home/omarlizardo/projects/CULTURE/childress-lizardo-culture-network-variety")
prep <- readRDS("cache/01_prepared_data.rds")
str07 <- readRDS("cache/07_reliability_cfa.rds")$structures
df <- prep$df

mk <- function(ibf) lapply(ibf, names)
spec_str <- function(items_by_factor) {
  paste(sapply(names(items_by_factor), function(fid)
    sprintf("%s =~ %s", fid, paste(items_by_factor[[fid]], collapse = " + "))), collapse = "\n")
}
prep_dat <- function(items) {
  d <- df[complete.cases(df[items]), items, drop = FALSE]
  d[] <- lapply(d, function(x) as.numeric(unclass(x)))
  d
}
fit3 <- function(ibf, dat) cfa(spec_str(ibf), data = dat, ordered = unlist(ibf), estimator = "WLSMV", std.lv = TRUE)

# Merged model: liberal + conservative items on one composition factor
merge2 <- function(ibf) {
  list(variety = ibf$variety, composition = c(ibf$liberal, ibf$conservative))
}

cat("\n### Within-battery discriminant check (3-factor vs merged 2-factor) ###\n")
for (nm in c("weak", "strong")) {
  ibf <- mk(str07[[nm]])
  dat <- prep_dat(unlist(ibf))
  f3 <- fit3(ibf, dat)
  f2 <- fit3(merge2(ibf), dat)
  cat(sprintf("\n%s battery, N = %d\n", nm, nrow(dat)))
  print(anova(f2, f3))
  cat("CFI/RMSEA 3-factor:", round(fitMeasures(f3, c("cfi.scaled","rmsea.scaled")), 3), "\n")
  cat("CFI/RMSEA merged:  ", round(fitMeasures(f2, c("cfi.scaled","rmsea.scaled")), 3), "\n")
}

cat("\n### Joint weak + strong model: cross-battery latent correlations ###\n")
wk <- mk(str07$weak); sg <- mk(str07$strong)
names(wk) <- paste0("w_", names(wk)); names(sg) <- paste0("s_", names(sg))
ibf <- c(wk, sg)
dat <- prep_dat(unlist(ibf))
fj <- fit3(ibf, dat)
cat("N =", nrow(dat), " CFI:", round(fitMeasures(fj, "cfi.scaled"), 3),
    " RMSEA:", round(fitMeasures(fj, "rmsea.scaled"), 3), "\n")
pe <- parameterEstimates(fj, standardized = TRUE)
cr <- pe[pe$op == "~~" & pe$lhs != pe$rhs & pe$lhs %in% lavNames(fj, "lv") & pe$rhs %in% lavNames(fj, "lv"),
         c("lhs", "rhs", "std.all", "se")]
print(cr, row.names = FALSE, digits = 3)
