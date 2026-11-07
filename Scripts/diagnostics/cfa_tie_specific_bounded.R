# Six-factor tie-specific CFA (weak and strong variety, liberal, conservative) on the 32 no-gender
# network items, with the weak-strong residual covariances from model 7 and each weak-strong
# latent correlation bounded at .95 (lavaan inequality constraint), WLSMV.
# Also fits the unbounded version with the same residuals for comparison.
# Run from project root: Rscript Scripts/diagnostics/cfa_tie_specific_bounded.R
suppressPackageStartupMessages(library(lavaan))
prep <- readRDS("cache/01_prepared_data.rds")
df <- prep$df
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
drop_gender <- function(x) x[!grepl("^Know(men|women)_", x)]
wv <- drop_gender(names(s07$weak$variety)); wl <- drop_gender(names(s07$weak$liberal)); wc <- drop_gender(names(s07$weak$conservative))
sv <- drop_gender(names(s07$strong$variety)); sl <- drop_gender(names(s07$strong$liberal)); sc <- drop_gender(names(s07$strong$conservative))
all_items <- c(wv, wl, wc, sv, sl, sc)
dat <- df[complete.cases(df[all_items]), all_items, drop = FALSE]
dat[] <- lapply(dat, function(x) as.numeric(unclass(x)))
weak_items <- c(wv, wl, wc)
pairs_w <- weak_items
pairs_s <- sub("_weak$", "_strong", weak_items)
stopifnot(all(pairs_s %in% c(sv, sl, sc)))
resid_syntax <- paste(sprintf("%s ~~ %s", pairs_w, pairs_s), collapse = "\n")
tie_syn <- paste0(
  "Wv =~ ", paste(wv, collapse = " + "), "\n",
  "Wl =~ ", paste(wl, collapse = " + "), "\n",
  "Wc =~ ", paste(wc, collapse = " + "), "\n",
  "Sv =~ ", paste(sv, collapse = " + "), "\n",
  "Sl =~ ", paste(sl, collapse = " + "), "\n",
  "Sc =~ ", paste(sc, collapse = " + "), "\n")
bound_syn <- paste0(
  "Wv ~~ r1*Sv\nWl ~~ r2*Sl\nWc ~~ r3*Sc\n",
  "r1 < 0.95\nr2 < 0.95\nr3 < 0.95\n")
fit_unb <- cfa(paste0(tie_syn, resid_syntax), data = dat, ordered = all_items, estimator = "WLSMV", std.lv = TRUE)
fit_bnd <- cfa(paste0(tie_syn, resid_syntax, "\n", bound_syn), data = dat, ordered = all_items, estimator = "WLSMV", std.lv = TRUE)
row <- function(nm, f) data.frame(model = nm, conv = lavInspect(f, "converged"),
  chisq_scaled = round(fitMeasures(f, "chisq.scaled"), 1), df = fitMeasures(f, "df"),
  CFI = round(fitMeasures(f, "cfi.scaled"), 3), RMSEA = round(fitMeasures(f, "rmsea.scaled"), 3),
  SRMR = round(fitMeasures(f, "srmr"), 3))
cat("N =", nrow(dat), " items =", length(all_items), " residual pairs =", length(pairs_w), "\n\n")
print(rbind(row("unbounded + residuals", fit_unb), row("bounded r <= .95 + residuals", fit_bnd)), row.names = FALSE)
for (nm in c("unbounded", "bounded")) {
  f <- if (nm == "unbounded") fit_unb else fit_bnd
  pe <- parameterEstimates(f, standardized = TRUE)
  cr <- pe[pe$op == "~~" & pe$lhs %in% c("Wv","Wl","Wc") & pe$rhs == sub("W", "S", pe$lhs) |
           pe$op == "~~" & pe$lhs %in% c("Wv","Wl","Wc") & pe$rhs == pe$lhs, c("lhs","rhs","est","std.all")]
  cat("\nWeak-strong latent correlations,", nm, ":\n")
  print(transform(cr[cr$lhs != cr$rhs, ], std.all = round(std.all, 3), est = round(est, 3)), row.names = FALSE)
  cat("Lowest latent variance (should be > 0):", round(min(pe$est[pe$op == "~~" & pe$lhs == pe$rhs & pe$lhs %in% c("Wv","Wl","Wc","Sv","Sl","Sc")]), 3), "\n")
}
