#' Pooled vs per-battery oblimin loadings, item by item
#'
#' Three 3-factor oblimin EFAs (polychoric, WLS): one on all 36 network items
#' (pooled), one on the 18 weak-tie items, one on the 18 strong-tie items.
#' Factor columns are mapped to variety / liberal / conservative by the mean
#' absolute loading of the script 01 anchor items on each column.
#' Writes cache/diagnostics/oblimin_loading_comparison.md.
#' Run from the project root: Rscript Scripts/diagnostics/oblimin_loading_comparison.R
suppressPackageStartupMessages(library(psych))

prep <- readRDS("cache/01_prepared_data.rds")
w_items <- rownames(unclass(prep$weak_fit$fa$loadings))
s_items <- rownames(unclass(prep$strong_fit$fa$loadings))
all36 <- c(w_items, s_items)
d <- prep$df[all36]
d[] <- lapply(d, function(x) as.numeric(unclass(x)))
dd <- d[complete.cases(d), ]

anchors <- list(
  variety = c("KnowHawaiiPI", "KnowAmIndianppl", "KnowMENAppl"),
  liberal = c("KnowVeryLib", "KnowLGBTQ"),
  conservative = c("KnowVeryCons", "KnowLotsaChurch")
)
map_cols <- function(L, suffixes) {
  sapply(anchors, function(a) {
    rows <- as.vector(outer(a, suffixes, paste0))
    which.max(colMeans(abs(L[rows, , drop = FALSE])))
  })
}
fit_oblimin <- function(items) {
  fa(polychoric(dd[items])$rho, nfactors = 3, n.obs = nrow(dd), rotate = "oblimin", fm = "wls")
}
fw <- fit_oblimin(w_items)
fs <- fit_oblimin(s_items)
fp <- fit_oblimin(all36)
Lw <- unclass(fw$loadings)
Ls <- unclass(fs$loadings)
Lp <- unclass(fp$loadings)
cw <- map_cols(Lw, "_weak")
cs <- map_cols(Ls, "_strong")
cp <- map_cols(Lp, c("_weak", "_strong"))

items <- unique(sub("_(weak|strong)$", "", all36))
fmt <- function(x) sprintf("%.2f", x)
rows <- lapply(items, function(it) {
  wn <- paste0(it, "_weak")
  sn <- paste0(it, "_strong")
  data.frame(
    item = it,
    pwv = Lp[wn, cp[1]], pwl = Lp[wn, cp[2]], pwc = Lp[wn, cp[3]],
    psv = Lp[sn, cp[1]], psl = Lp[sn, cp[2]], psc = Lp[sn, cp[3]],
    wv = Lw[wn, cw[1]], wl = Lw[wn, cw[2]], wc = Lw[wn, cw[3]],
    sv = Ls[sn, cs[1]], sl = Ls[sn, cs[2]], sc = Ls[sn, cs[3]],
    stringsAsFactors = FALSE
  )
})
tbl <- do.call(rbind, rows)
tbl <- tbl[order(-abs(tbl$pwv)), ]
header <- "| Item | Pooled-W V | Pooled-W L | Pooled-W C | Pooled-S V | Pooled-S L | Pooled-S C | Weak V | Weak L | Weak C | Strong V | Strong L | Strong C |"
sep <- "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|"
body <- apply(tbl, 1, function(r) sprintf("| %s | %s |", r[1], paste(sapply(r[-1], function(x) fmt(as.numeric(x))), collapse = " | ")))
out <- c(
  "## Pooled vs Per-Battery Oblimin Loadings (Item by Item)",
  "",
  sprintf("Pooled: one 3-factor oblimin EFA on all 36 items (N = %d complete cases), which gives one row per item-battery. Pooled-W and Pooled-S are the pooled loadings on the item's weak-tie and strong-tie rows. Weak and Strong are separate 3-factor oblimin EFAs on each battery. V, L, and C are variety, liberal, and conservative, labeled by the script 01 anchor items. Rows are sorted by pooled-W variety.", nrow(dd)),
  "",
  header, sep, body
)
writeLines(out, "cache/diagnostics/oblimin_loading_comparison.md")
cat(out, sep = "\n")
