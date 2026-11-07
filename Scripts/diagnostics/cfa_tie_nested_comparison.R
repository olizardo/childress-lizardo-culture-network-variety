# Nested comparison of tie structures on the 32 no-gender network items (N = 1,258, WLSMV), all with
# the model 7 weak-strong residual covariances:
#   (a) pooled: one variety, one liberal, one conservative factor across ties (no tie distinction)
#   (b) tie-specific: six factors (weak and strong V, L, C), unconstrained
#   (c) tie-collapsed: (b) with each weak-strong factor correlation fixed at 1; equivalent to (a)
#       (a weak and strong factor with correlation 1 is one pooled factor), so (c) nests in (b)
# Run from project root: Rscript Scripts/diagnostics/cfa_tie_nested_comparison.R
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
pairs_w <- c(wv, wl, wc)
pairs_s <- sub("_weak$", "_strong", pairs_w)
stopifnot(all(pairs_s %in% c(sv, sl, sc)))
stopifnot(identical(sub("_strong$", "", c(sv, sl, sc)[match(pairs_s, c(sv, sl, sc))]), sub("_weak$", "", pairs_w)))
resid_syntax <- paste(sprintf("%s ~~ %s", pairs_w, pairs_s), collapse = "\n")
pooled_syn <- paste0(
  "V =~ ", paste(c(wv, sv), collapse = " + "), "\n",
  "L =~ ", paste(c(wl, sl), collapse = " + "), "\n",
  "C =~ ", paste(c(wc, sc), collapse = " + "), "\n")
tie_syn <- paste0(
  "Wv =~ ", paste(wv, collapse = " + "), "\n",
  "Wl =~ ", paste(wl, collapse = " + "), "\n",
  "Wc =~ ", paste(wc, collapse = " + "), "\n",
  "Sv =~ ", paste(sv, collapse = " + "), "\n",
  "Sl =~ ", paste(sl, collapse = " + "), "\n",
  "Sc =~ ", paste(sc, collapse = " + "), "\n")
collapsed_syn <- paste0(tie_syn, "Wv ~~ 1*Sv\nWl ~~ 1*Sl\nWc ~~ 1*Sc\n")
# the collapsed model has no weak-only or strong-only items beyond the matched set, so fit it on
# the matched items only, keeping the tie-level residual structure
fit_a <- cfa(paste0(pooled_syn, resid_syntax), data = dat, ordered = all_items, estimator = "WLSMV", std.lv = TRUE)
fit_b <- cfa(paste0(tie_syn, resid_syntax), data = dat, ordered = all_items, estimator = "WLSMV", std.lv = TRUE)
fit_c <- cfa(paste0(collapsed_syn, resid_syntax), data = dat, ordered = all_items, estimator = "WLSMV", std.lv = TRUE)
row <- function(nm, f) data.frame(model = nm, conv = lavInspect(f, "converged"),
  chisq_scaled = round(fitMeasures(f, "chisq.scaled"), 1), df = fitMeasures(f, "df"),
  CFI = round(fitMeasures(f, "cfi.scaled"), 3), RMSEA = round(fitMeasures(f, "rmsea.scaled"), 3),
  SRMR = round(fitMeasures(f, "srmr"), 3))
cat("N =", nrow(dat), " items =", length(all_items), " residual pairs =", length(pairs_w), "\n\n")
print(rbind(row("(a) pooled V, L, C + residuals", fit_a),
            row("(b) tie-specific 6-factor + residuals", fit_b),
            row("(c) tie-specific with weak-strong r = 1", fit_c)), row.names = FALSE)
cat("\nNested test (c) within (b), scaled difference (lavTestLRT):\n")
print(lavTestLRT(fit_c, fit_b))
cat("\nWeak-strong latent correlations, (b):\n")
pe_b <- parameterEstimates(fit_b, standardized = TRUE)
print(transform(pe_b[pe_b$op == "~~" & pe_b$lhs %in% c("Wv","Wl","Wc") & pe_b$rhs %in% c("Sv","Sl","Sc") & pe_b$lhs == sub("S", "W", pe_b$rhs), c("lhs","rhs","std.all")], std.all = round(std.all, 3)), row.names = FALSE)
