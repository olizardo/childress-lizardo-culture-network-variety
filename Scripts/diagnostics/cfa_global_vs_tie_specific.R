# Compares two tie-battery CFA structures on the same 32 no-gender network items (N = 1,258, WLSMV):
#   (a) global: one variety, one liberal, one conservative factor pooling weak and strong items
#   (b) tie-specific: six factors (weak variety, weak liberal, weak conservative, and the strong
#       counterparts), as in the original paper's separate weak- and strong-tie factor models
# Run from project root: Rscript Scripts/diagnostics/cfa_global_vs_tie_specific.R
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
cat("N =", nrow(dat), " items =", length(all_items), "\n")
global_syn <- paste0(
  "V =~ ", paste(c(wv, sv), collapse = " + "), "\n",
  "L =~ ", paste(c(wl, sl), collapse = " + "), "\n",
  "C =~ ", paste(c(wc, sc), collapse = " + "))
tie_syn <- paste0(
  "Wv =~ ", paste(wv, collapse = " + "), "\n",
  "Wl =~ ", paste(wl, collapse = " + "), "\n",
  "Wc =~ ", paste(wc, collapse = " + "), "\n",
  "Sv =~ ", paste(sv, collapse = " + "), "\n",
  "Sl =~ ", paste(sl, collapse = " + "), "\n",
  "Sc =~ ", paste(sc, collapse = " + "))
fit_global <- cfa(global_syn, data = dat, ordered = all_items, estimator = "WLSMV", std.lv = TRUE)
fit_tie <- cfa(tie_syn, data = dat, ordered = all_items, estimator = "WLSMV", std.lv = TRUE)
fit_row <- function(nm, f) data.frame(model = nm,
  conv = lavInspect(f, "converged"),
  chisq_scaled = round(fitMeasures(f, "chisq.scaled"), 1),
  df = fitMeasures(f, "df"),
  CFI = round(fitMeasures(f, "cfi.scaled"), 3),
  TLI = round(fitMeasures(f, "tli.scaled"), 3),
  RMSEA = round(fitMeasures(f, "rmsea.scaled"), 3),
  SRMR = round(fitMeasures(f, "srmr"), 3))
cat("\nFit comparison:\n")
print(rbind(fit_row("global (V, L, C pooled)", fit_global), fit_row("tie-specific (6 factors)", fit_tie)), row.names = FALSE)
pe_t <- parameterEstimates(fit_tie, standardized = TRUE)
cors <- pe_t[pe_t$op == "~~" & pe_t$lhs != pe_t$rhs & pe_t$lhs %in% c("Wv","Wl","Wc","Sv","Sl","Sc") & pe_t$rhs %in% c("Wv","Wl","Wc","Sv","Sl","Sc"), c("lhs","rhs","std.all")]
cat("\nLatent correlations, tie-specific model:\n")
print(transform(cors, std.all = round(std.all, 2)), row.names = FALSE)
pe_g <- parameterEstimates(fit_global, standardized = TRUE)
gcors <- pe_g[pe_g$op == "~~" & pe_g$lhs != pe_g$rhs, c("lhs","rhs","std.all")]
cat("\nLatent correlations, global model:\n")
print(transform(gcors, std.all = round(std.all, 2)), row.names = FALSE)
