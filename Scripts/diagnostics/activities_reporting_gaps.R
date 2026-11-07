# Diagnostics: reporting gaps for the "Factor Validation of Activities" draft section.
# Computes:
#   (1) one/two/three-factor CFA (WLSMV, polychoric) with RMSEA CIs + nested scaled chi-square tests
#   (2) ESEM with target rotation (validation against Table 1 numbers) + RMSEA CI
#   (3) oblimin EFA fit indices (Table A2 currently has none)
#   (4) top-category counts and a collapsed-top-category sensitivity refit
#   (5) polychoric-based Cronbach alpha for the activity factors (and omega cross-check from cache 07)
# Run: Rscript Scripts/diagnostics/activities_reporting_gaps.R   (from project root)

suppressPackageStartupMessages({
  library(psych); library(lavaan)
})

prep <- readRDS("cache/01_prepared_data.rds")
df <- prep$df
items <- prep$activity_items

# Varimax assignment (Table A3 targets)
arts_items <- c("museum", "art_gallery", "symphony_orchestra_opera",
  "carnival_fair_amusement_park", "music_concert_festival", "play_or_musical",
  "library", "fancy_restaurant", "dance_performance", "movie_theater",
  "attend_sports", "hiking_camping_boating", "historic_site", "go_to_festival")
leisure_items <- c("go_for_walk", "exercise_or_yoga", "read_novel_poem_or_play")
resid_items <- c("gardening", "fast_food", "home_auto_repair")

act_df <- as.data.frame(lapply(df[items], function(x) as.numeric(unclass(x))))
cc <- complete.cases(act_df)
act_full <- act_df[cc, ]
N <- nrow(act_full)
message("Complete cases: ", N)

# ---------------------------------------------------------------- (4a) top-category counts
cat_counts <- sapply(act_full, function(x) sum(x == 7))
sparse <- names(cat_counts)[cat_counts < 10]
message("Category-7 counts < 10: ", paste(sparse, cat_counts[cat_counts < 10], collapse = ", "))

# ---------------------------------------------------------------- (1) CFA models
mod3 <- paste(
  "arts =~", paste(arts_items, collapse = " + "),
  "\nleisure =~", paste(leisure_items, collapse = " + "),
  "\nresidual =~", paste(resid_items, collapse = " + ")
)
mod2_arts <- paste("arts =~", paste(c(arts_items, resid_items), collapse = " + "),
  "\nleisure =~", paste(leisure_items, collapse = " + "))
mod2_leis <- paste("arts =~", paste(arts_items, collapse = " + "),
  "\nleisure =~", paste(c(leisure_items, resid_items), collapse = " + "))
mod1 <- paste("f =~", paste(items, collapse = " + "))

fit_cfa <- function(model) {
  cfa(model, data = act_full, ordered = items, estimator = "WLSMV", std.lv = TRUE)
}
f1 <- fit_cfa(mod1); f2a <- fit_cfa(mod2_arts); f2l <- fit_cfa(mod2_leis); f3 <- fit_cfa(mod3)

fm <- function(f) {
  m <- fitMeasures(f, c("chisq.scaled", "df", "cfi.scaled", "rmsea.scaled",
    "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr"))
  cat(sprintf("chi2=%.1f df=%d CFI=%.3f RMSEA=%.3f [%.3f, %.3f] SRMR=%.3f\n",
    m[1], m[2], m[3], m[4], m[5], m[6], m[7]))
  m
}
message("One-factor:");       m1 <- fm(f1)
message("Two-factor (resid into arts):"); ma <- fm(f2a)
message("Two-factor (resid into leisure):"); ml <- fm(f2l)
message("Three-factor:");     m3 <- fm(f3)

# Scaled difference test. METHOD NOTE (November 7, 2026): the manuscript's
# Table 1 Delta-chi-square column (146.4/121.6/51.0/746.7) reproduces exactly
# under the Satorra-Bentler 2001-style scaled difference using the scaled
# chi-squares and per-model slot scaling factors:
#   cd = (df_restricted*sf_restricted - df_unrestricted*sf_unrestricted)/df_diff
#   T  = (chisq.scaled_restricted - chisq.scaled_unrestricted)/cd
# where sf = fit@tools? slot(fit, "test")[[2]]$scaling.factor. lavaan's own
# lavTestLRT (scaled-shifted Satorra 2000) gives different values (85.6/106.0/
# 59.5) and satorra.bentler.2010 fails or goes negative for these WLSMV fits;
# the SB2001-style formula above is the one consistent with the manuscript.
sf_of <- function(f) slot(f, "test")[[2]]$scaling.factor
sb <- function(unres, res, label) {
  dfu <- unlist(fitMeasures(unres, "df")); dfr <- unlist(fitMeasures(res, "df"))
  m <- dfr - dfu
  cd <- (dfr * sf_of(res) - dfu * sf_of(unres)) / m
  Td <- (unlist(fitMeasures(res, "chisq.scaled")) -
    unlist(fitMeasures(unres, "chisq.scaled"))) / cd
  p <- pchisq(Td, m, lower.tail = FALSE)
  cat(sprintf("%s: scaled diff chisq = %.1f on %d df, p = %.4g\n", label, Td, m, p))
  c(Td = Td, df = m, p = p)
}
sb(f3, f1, "three vs one (expect 146.4)")
sb(f3, f2a, "three vs two-arts (expect 121.6)")
sb(f3, f2l, "three vs two-leis (expect 51.0)")
sb(f2a, f1, "two-arts vs one")
sb(f2l, f1, "two-leis vs one")

phi3 <- lavInspect(f3, "cor.lv")
message("Three-factor CFA latent correlations:")
print(round(phi3, 3))

# ---------------------------------------------------------------- (2) ESEM with target rotation
# Target matrix: binary, 1 = free primary loading, 0 = targeted to zero
target_mat <- matrix(0, nrow = length(items), ncol = 3, dimnames = list(items, c("arts", "leisure", "residual")))
target_mat[arts_items, "arts"] <- 1
target_mat[leisure_items, "leisure"] <- 1
target_mat[resid_items, "residual"] <- 1

esem <- efa(data = act_full, nfactors = 3, ordered = items, estimator = "WLSMV",
  rotation = list("target", target = target_mat))
me <- fitMeasures(esem, c("chisq.scaled", "df", "cfi.scaled", "rmsea.scaled",
  "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr"))
message("ESEM (target):")
cat(sprintf("chi2=%.1f df=%d CFI=%.3f RMSEA=%.3f [%.3f, %.3f] SRMR=%.3f\n",
  me[1], me[2], me[3], me[4], me[5], me[6], me[7]))

# ---------------------------------------------------------------- (3) oblimin EFA fit indices
# Same estimation as Table A2 (WLS on the smoothed polychoric matrix).
# psych leaves df NULL for correlation-matrix inputs; the EFA df under psych's
# counting is 133 (p(p+1)/2 - [pk + p - k(k-1)/2]), which matches lavaan's
# efa() df for the unrestricted solution, so compute p from chi and 133.
ef_obi <- fa(polychoric(act_full)$rho, nfactors = 3, rotate = "oblimin",
  fm = "wls", n.obs = N)
efa_df <- 133
efa_p <- pchisq(ef_obi$chi, efa_df, lower.tail = FALSE)
message(sprintf("Oblimin EFA (WLS, polychoric): chi=%.1f df=%d p=%.3g TLI=%.3f RMSEA=%.3f [%.3f, %.3f]",
  ef_obi$chi, efa_df, efa_p, ef_obi$TLI, ef_obi$RMSEA[1], ef_obi$RMSEA[2], ef_obi$RMSEA[3]))

# ---------------------------------------------------------------- (4b) collapsed-top-category sensitivity
act_collapsed <- act_full
for (v in sparse) act_collapsed[[v]] <- pmin(act_collapsed[[v]], 6)
poly_c <- polychoric(act_collapsed)$rho
fa_c <- principal(poly_c, nfactors = 3, rotate = "varimax", n.obs = N)
load_c <- unclass(fa_c$loadings)
# Map collapsed RC columns to canonical factor labels by matching loading
# profiles to the canonical varimax solution (column-wise correlation, greedy
# one-to-one via the canonical assignment's factor of each item).
assign_o <- c(setNames(rep("arts", length(arts_items)), arts_items),
  setNames(rep("leisure", length(leisure_items)), leisure_items),
  setNames(rep("residual", length(resid_items)), resid_items))
canon <- prep$act_fa$loadings
col_map <- character(3)
for (j in seq_len(3)) {
  # factor label whose canonical loadings best match this collapsed column
  cor_by_lab <- sapply(c("arts", "leisure", "residual"), function(lab) {
    ids <- names(assign_o)[assign_o == lab]
    # canonical column for this label = column where these items load highest on average
    col_o <- which.max(colMeans(abs(unclass(canon)[ids, , drop = FALSE])))
    cor(load_c[ids, j], unclass(canon)[ids, col_o])
  })
  col_map[j] <- names(which.max(cor_by_lab))
}
assign_c <- setNames(col_map[apply(abs(load_c), 1, which.max)], rownames(load_c))
message("Collapsed-varimax assignment changes vs canonical:")
print(assign_c[assign_c != assign_o[rownames(load_c)]])
message("Collapsed variance explained: ", sprintf("%.1f", 100 * fa_c$Vaccounted["Cumulative Var", 3]))

mod3_c <- paste(
  "arts =~", paste(arts_items, collapse = " + "),
  "\nleisure =~", paste(leisure_items, collapse = " + "),
  "\nresidual =~", paste(resid_items, collapse = " + ")
)
f3_c <- cfa(mod3_c, data = act_collapsed, ordered = items, estimator = "WLSMV", std.lv = TRUE)
message("Collapsed three-factor CFA:")
mc3 <- fm(f3_c)
message("Collapsed latent correlations:")
print(round(lavInspect(f3_c, "cor.lv"), 3))

# ---------------------------------------------------------------- (5) polychoric alpha per factor
poly_alpha <- function(rho) {
  k <- ncol(rho)
  k / (k - 1) * (1 - k / sum(rho))
}
message("Polychoric alphas:")
alpha_arts <- poly_alpha(polychoric(act_full[, arts_items])$rho)
alpha_leis <- poly_alpha(polychoric(act_full[, leisure_items])$rho)
alpha_res <- poly_alpha(polychoric(act_full[, resid_items])$rho)
cat(sprintf("  arts (14 items): %.3f\n", alpha_arts))
cat(sprintf("  leisure (3 items): %.3f\n", alpha_leis))
cat(sprintf("  residual (3 items): %.3f\n", alpha_res))

saveRDS(list(
  N = N, sparse_items = sparse, cat_counts = cat_counts,
  cfa = list(one = m1, two_arts = ma, two_leis = ml, three = m3, phi = phi3),
  lrt = list(
    method = "Satorra-Bentler 2001-style scaled difference: cd = (df_r*sf_r - df_u*sf_u)/df_diff; T = (scaled chisq_r - scaled chisq_u)/cd",
    three_vs_one = sb(f3, f1, "three vs one"),
    three_vs_two_arts = sb(f3, f2a, "three vs two-arts"),
    three_vs_two_leis = sb(f3, f2l, "three vs two-leis"),
    two_arts_vs_one = sb(f2a, f1, "two-arts vs one"),
    two_leis_vs_one = sb(f2l, f1, "two-leis vs one")),
  esem = me, efa_oblimin = list(chi = ef_obi$chi, df = efa_df, pvalue = efa_p,
    TLI = ef_obi$TLI, RMSEA = ef_obi$RMSEA),
  alphas = list(arts = alpha_arts, leisure = alpha_leis, residual = alpha_res),
  collapsed = list(cfa = mc3, phi = lavInspect(f3_c, "cor.lv"),
    var_explained = fa_c$Vaccounted["Cumulative Var", 3],
    assignment_changes = assign_c[assign_c != assign_o[rownames(load_c)]])
), "cache/diagnostics/activities_reporting_gaps.rds")
message("Saved cache/diagnostics/activities_reporting_gaps.rds")
