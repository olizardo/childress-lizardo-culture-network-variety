#' Registry of every statistic cited in the running prose of results_draft.tex.
#' Each value is computed from a saved cache (or, for the activity target ESEM, from a
#' five-second refit because no cache stores that fit object) and formatted as printed.
#' Writes cache/manuscript/prose_numbers.csv with columns id, section, value, description, source.
#'
#' Value conventions: decimals keep the leading zero and sign ("-0.12" for "$-.12$");
#' integers keep thousands separators ("1,243"); counts written out in the prose are
#' recorded as words ("two"). Threshold claims ("p < .001", "over .80") record the
#' threshold and are checked here; a failed claim is written as "CLAIM FAILS" so that
#' Scripts/manuscript/check_prose_numbers.py reports it as MISSING.
#'
#' Run from the project root: Rscript --no-save --no-restore Scripts/manuscript/build_prose_numbers.R

suppressPackageStartupMessages(library(lavaan))

S1 <- "Factor Validation of Activities"
S2 <- "Factor Validation of Network Composition"
S3 <- "Structural Models of Activity Participation"
S3a <- "Sociodemographic Gradients in Network Composition"
S3b <- "Network Effects on Activity Participation"
S3c <- "Direct, Network-Mediated, and Total Effects of the Focal Predictors"
S3d <- "Other Sociodemographic Direct Effects on Activity Factors"

prep <- readRDS("cache/01_prepared_data.rds")
mn <- readRDS("cache/manuscript/measurement_numbers.rds")
gaps <- readRDS("cache/diagnostics/activities_reporting_gaps.rds")
c07 <- readRDS("cache/07_reliability_cfa.rds")
c09 <- readRDS("cache/09_no_gender_robustness.rds")
esem_t <- readRDS("cache/diagnostics/esem_ties_target.rds")
joint <- readRDS("cache/diagnostics/sem_gpa_fixed_joint_covars.rds")
sc <- readRDS("cache/diagnostics/sem_gpa_fixed_sep_covars.rds")
dc <- readRDS("cache/diagnostics/sem_gpa_fixed_sep_decomposition.rds")

d1 <- function(x) sprintf("%.1f", round(x, 1))
d2 <- function(x) sub("^-(0\\.00)$", "\\1", sprintf("%.2f", round(x, 2)))
d3 <- function(x) sub("^-(0\\.000)$", "\\1", sprintf("%.3f", round(x, 3)))
pv <- function(p) if (p < .01) d3(p) else d2(p)
thr <- function(p) if (p < .001) "0.001" else if (p < .01) "0.01" else if (p < .05) "0.05" else "CLAIM FAILS"
claim <- function(ok, value) if (isTRUE(ok)) value else "CLAIM FAILS"
nfmt <- function(n) format(n, big.mark = ",", trim = TRUE)
words <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
  "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen",
  "nineteen", "twenty")

reg <- list()
add <- function(id, section, value, description, source) {
  reg[[length(reg) + 1]] <<- data.frame(id = id, section = section, value = value,
    description = description, source = source)
}

# ---------------------------------------------------------------- Factor Validation of Activities
items <- prep$activity_items
act_df <- as.data.frame(lapply(prep$df[items], function(x) as.numeric(unclass(x))))
act_full <- act_df[complete.cases(act_df), ]
s_act <- c07$structures$activities
arts_items <- names(s_act$arts)

src_mn <- "cache/manuscript/measurement_numbers.rds"
src_gaps <- "cache/diagnostics/activities_reporting_gaps.rds"
src_esem <- "refit in build_prose_numbers.R: lavaan::efa() WLSMV target ESEM on cache/01_prepared_data.rds complete cases, targets from cache/07_reliability_cfa.rds $structures$activities (no cache stores this fit)"

add("act_n_items", S1, as.character(length(items)), "Number of activity items",
  "cache/01_prepared_data.rds: length(prep$activity_items)")
add("act_ve", S1, d1(100 * sum(unclass(prep$act_fa$loadings)[items, ]^2) / length(items)),
  "Percent variance explained by the three-component varimax PCA",
  "cache/01_prepared_data.rds: 100 * sum(prep$act_fa$loadings^2) / 20")
add("act_kmo", S1, d3(prep$act_kmo), "KMO sampling adequacy, activity battery (polychoric)",
  "cache/01_prepared_data.rds: prep$act_kmo")
add("fit_cfi_benchmark", S1, "0.95", "Conventional CFI benchmark cited in the fit-index footnote",
  "standard convention (Hu and Bentler 1999); no cache")
add("fit_rmsea_srmr_benchmark", S1, "0.08", "Conventional RMSEA and SRMR benchmark cited in the fit-index footnote",
  "standard convention (Hu and Bentler 1999); no cache")
add("act_n", S1, nfmt(mn$n_activities), "Complete-case N on all 20 activity items",
  paste0(src_mn, ": n_activities"))

add("act_n_arts", S1, words[length(s_act$arts)], "Number of items on the public arts factor (Fourteen/fourteen)",
  "cache/07_reliability_cfa.rds: length($structures$activities$arts)")
add("act_n_leis", S1, words[length(s_act$leisure)], "Number of items on the solitary leisure factor",
  "cache/07_reliability_cfa.rds: length($structures$activities$leisure)")
add("act_n_resid", S1, words[length(s_act$residual)], "Number of items on the residual factor",
  "cache/07_reliability_cfa.rds: length($structures$activities$residual)")

efa <- gaps$efa_oblimin
add("efa_df", S1, as.character(efa$df), "Oblimin EFA chi-square df", paste0(src_gaps, ": efa_oblimin$df"))
add("efa_chi", S1, d1(efa$chi), "Oblimin EFA chi-square", paste0(src_gaps, ": efa_oblimin$chi"))
add("efa_p", S1, thr(efa$pvalue), "Oblimin EFA chi-square p < .001", paste0(src_gaps, ": efa_oblimin$pvalue"))
add("efa_tli", S1, d3(efa$TLI), "Oblimin EFA TLI", paste0(src_gaps, ": efa_oblimin$TLI"))
add("efa_rmsea", S1, d3(efa$RMSEA[1]), "Oblimin EFA RMSEA", paste0(src_gaps, ": efa_oblimin$RMSEA[1]"))
add("efa_rmsea_lo", S1, d3(efa$RMSEA[2]), "Oblimin EFA RMSEA 90% CI lower", paste0(src_gaps, ": efa_oblimin$RMSEA[2]"))
add("efa_rmsea_hi", S1, d3(efa$RMSEA[3]), "Oblimin EFA RMSEA 90% CI upper", paste0(src_gaps, ": efa_oblimin$RMSEA[3]"))
phi_ob <- mn$oblimin_phi
add("efa_phi_al", S1, d2(phi_ob[2, 1]), "Oblimin factor correlation, arts and leisure",
  paste0(src_mn, ": oblimin_phi[2, 1]"))
add("efa_phi_ar", S1, d2(phi_ob[3, 1]), "Oblimin factor correlation, arts and residual",
  paste0(src_mn, ": oblimin_phi[3, 1]"))
add("efa_phi_lr", S1, d2(phi_ob[3, 2]), "Oblimin factor correlation, leisure and residual",
  paste0(src_mn, ": oblimin_phi[3, 2]"))

lrt <- gaps$lrt
for (k in list(c("two_arts", "residual merged into arts"), c("two_leis", "residual merged into leisure"),
  c("one", "one-factor"))) {
  x <- lrt[[paste0("three_vs_", k[1])]]
  add(paste0("dchi_", k[1]), S1, d1(x[1]), paste("Scaled delta chi-square, three-factor CFA vs", k[2]),
    paste0(src_gaps, ": lrt$three_vs_", k[1], "[1] (SB2001-style scaled difference)"))
  add(paste0("dchi_", k[1], "_df"), S1, as.character(x[2]), paste("Delta df, three-factor CFA vs", k[2]),
    paste0(src_gaps, ": lrt$three_vs_", k[1], "[2]"))
  add(paste0("dchi_", k[1], "_p"), S1, thr(x[3]), paste("Delta chi-square p < .001, three-factor CFA vs", k[2]),
    paste0(src_gaps, ": lrt$three_vs_", k[1], "[3]"))
}
t2_3 <- mn$table2$three
add("cfa3_cfi", S1, d3(t2_3["cfi.scaled"]), "Three-factor activity CFA scaled CFI", paste0(src_mn, ": table2$three['cfi.scaled']"))
add("cfa3_rmsea", S1, d3(t2_3["rmsea.scaled"]), "Three-factor activity CFA scaled RMSEA", paste0(src_mn, ": table2$three['rmsea.scaled']"))
add("cfa3_srmr", S1, d3(t2_3["srmr"]), "Three-factor activity CFA SRMR", paste0(src_mn, ": table2$three['srmr']"))

tm <- matrix(0, length(items), 3, dimnames = list(items, names(s_act)))
for (f in names(s_act)) tm[names(s_act[[f]]), f] <- 1
esem_act <- efa(data = act_full, nfactors = 3, ordered = items, estimator = "WLSMV",
  rotation = list("target", target = tm))[[1]]
f3 <- c07$fits$activities$fit
sf_of <- function(f) slot(f, "test")[[2]]$scaling.factor
sb <- function(unres, res) {
  dfu <- unname(fitMeasures(unres, "df")); dfr <- unname(fitMeasures(res, "df"))
  m <- dfr - dfu
  cd <- (dfr * sf_of(res) - dfu * sf_of(unres)) / m
  Td <- (unname(fitMeasures(res, "chisq.scaled")) - unname(fitMeasures(unres, "chisq.scaled"))) / cd
  c(Td = Td, df = m, p = pchisq(Td, m, lower.tail = FALSE))
}
stopifnot(abs(unname(fitMeasures(esem_act, "chisq.scaled")) - mn$table2$esem["chisq.scaled"]) < 1e-3)
es <- gaps$esem[, 1]
add("esem_act_cfi", S1, d3(es["cfi.scaled"]), "Activity target ESEM scaled CFI", paste0(src_gaps, ": esem['cfi.scaled', ]"))
add("esem_act_rmsea", S1, d3(es["rmsea.scaled"]), "Activity target ESEM scaled RMSEA", paste0(src_gaps, ": esem['rmsea.scaled', ]"))
add("esem_act_rmsea_lo", S1, d3(es["rmsea.ci.lower.scaled"]), "Activity target ESEM RMSEA 90% CI lower",
  paste0(src_gaps, ": esem['rmsea.ci.lower.scaled', ]"))
add("esem_act_rmsea_hi", S1, d3(es["rmsea.ci.upper.scaled"]), "Activity target ESEM RMSEA 90% CI upper",
  paste0(src_gaps, ": esem['rmsea.ci.upper.scaled', ]"))
add("esem_act_srmr", S1, d3(es["srmr"]), "Activity target ESEM SRMR", paste0(src_gaps, ": esem['srmr', ]"))
dx <- sb(esem_act, f3)
add("dchi_esem", S1, d1(dx["Td"]), "Scaled delta chi-square, ESEM vs three-factor CFA",
  paste(src_esem, "+ cache/07_reliability_cfa.rds $fits$activities$fit; SB2001-style scaled difference"))
add("dchi_esem_df", S1, as.character(dx["df"]), "Delta df, ESEM vs three-factor CFA",
  paste(src_esem, "+ cache/07_reliability_cfa.rds $fits$activities$fit"))
add("dchi_esem_p", S1, thr(dx["p"]), "Delta chi-square p < .001, ESEM vs three-factor CFA",
  paste(src_esem, "+ cache/07_reliability_cfa.rds $fits$activities$fit"))

phi_e <- lavInspect(esem_act, "cor.lv")
add("esem_act_phi_al", S1, d2(phi_e[2, 1]), "ESEM factor correlation, arts and leisure", paste0(src_esem, ": cor.lv[2, 1]"))
add("esem_act_phi_ar", S1, d2(phi_e[3, 1]), "ESEM factor correlation, arts and residual", paste0(src_esem, ": cor.lv[3, 1]"))
add("esem_act_phi_lr", S1, d2(phi_e[3, 2]), "ESEM factor correlation, leisure and residual", paste0(src_esem, ": cor.lv[3, 2]"))

t1 <- mn$table1; t3 <- mn$table3; ta1 <- mn$tableA1
add("t3_arts_min", S1, d2(min(t3[arts_items, "arts"])), "Minimum primary arts loading, Table 3 (library)",
  paste0(src_mn, ": min(table3[arts items, 'arts'])"))
add("t3_arts_max", S1, d2(max(t3[arts_items, "arts"])), "Maximum primary arts loading, Table 3 (dance)",
  paste0(src_mn, ": max(table3[arts items, 'arts'])"))
t3_cell <- list(
  c("t3_walk", "go_for_walk", "leisure", "Walking on leisure, Table 3"),
  c("t3_exercise", "exercise_or_yoga", "leisure", "Exercise on leisure, Table 3"),
  c("t3_exercise_arts", "exercise_or_yoga", "arts", "Exercise secondary loading on arts, Table 3"),
  c("t3_read", "read_novel_poem_or_play", "leisure", "Reading on leisure, Table 3"),
  c("t3_garden", "gardening", "residual", "Gardening on residual, Table 3"),
  c("t3_repair", "home_auto_repair", "residual", "Home or auto repair on residual, Table 3"),
  c("t3_fastfood", "fast_food", "residual", "Fast food on residual, Table 3"),
  c("t3_museum_resid", "museum", "residual", "Museum secondary loading on residual, Table 3"),
  c("t3_historic_resid", "historic_site", "residual", "Historic site secondary loading on residual, Table 3"),
  c("t3_hiking_resid", "hiking_camping_boating", "residual", "Hiking secondary loading on residual, Table 3"))
for (k in t3_cell) add(k[1], S1, d2(t3[k[2], k[3]]), k[4], sprintf("%s: table3['%s', '%s']", src_mn, k[2], k[3]))
add("t1_read", S1, d2(t1["read_novel_poem_or_play", "Leisure"]), "Reading on leisure, Table 1 (varimax PCA)",
  paste0(src_mn, ": table1['read_novel_poem_or_play', 'Leisure']"))
add("ta1_read", S1, d2(ta1["read_novel_poem_or_play", "Leisure"]), "Reading on leisure, Table A1 (oblimin EFA)",
  paste0(src_mn, ": tableA1['read_novel_poem_or_play', 'Leisure']"))

top7 <- sapply(act_full, function(x) sum(x == 7))
for (k in list(c("symphony_orchestra_opera", "symphonies"), c("dance_performance", "dance performances"),
  c("carnival_fair_amusement_park", "carnivals"))) {
  add(paste0("top7_", k[1]), S1, words[top7[[k[1]]]], paste("Respondents in the top frequency category,", k[2]),
    sprintf("cache/01_prepared_data.rds: sum(prep$df$%s == 7) among activity complete cases", k[1]))
}
cc <- gaps$collapsed$cfa
add("collapsed_cfi", S1, d3(cc["cfi.scaled"]), "Three-factor CFA CFI with top two categories collapsed",
  paste0(src_gaps, ": collapsed$cfa['cfi.scaled']"))
add("collapsed_cfi_base", S1, d3(gaps$cfa$three["cfi.scaled"]), "Three-factor CFA CFI, uncollapsed comparison",
  paste0(src_gaps, ": cfa$three['cfi.scaled']"))
add("collapsed_rmsea", S1, claim(d3(cc["rmsea.scaled"]) == d3(gaps$cfa$three["rmsea.scaled"]), d3(cc["rmsea.scaled"])),
  "Three-factor CFA RMSEA, collapsed and uncollapsed (same in both)",
  paste0(src_gaps, ": collapsed$cfa['rmsea.scaled'] and cfa$three['rmsea.scaled']"))

# ---------------------------------------------------------------- Factor Validation of Network Composition
tie16 <- unlist(lapply(c07$structures$weak, names))
tie16 <- tie16[!grepl("^Know(men|women)_", tie16)]
add("tie_n_items", S2, words[length(tie16)], "Number of position-generator categories per battery (gender excluded)",
  "cache/07_reliability_cfa.rds: $structures$weak items minus Knowmen/Knowwomen")
add("tie_kmo_weak", S2, d3(prep$weak_fit$kmo), "KMO sampling adequacy, weak-tie battery (polychoric varimax)",
  "cache/01_prepared_data.rds: prep$weak_fit$kmo")
add("tie_kmo_strong", S2, d3(prep$strong_fit$kmo), "KMO sampling adequacy, strong-tie battery (polychoric varimax)",
  "cache/01_prepared_data.rds: prep$strong_fit$kmo")
add("tie_ve", S2, as.character(round(100 * prep$weak_fit$var_explained)), "Percent variance explained by the three-factor varimax solution (about 68 in both batteries)",
  "cache/01_prepared_data.rds: 100 * prep$weak_fit$var_explained (strong: prep$strong_fit$var_explained)")

gl <- function(tie, item, fac) {
  f <- prep[[paste0(tie, "_fit")]]
  unclass(f$fa$loadings)[paste0(item, "_", tie), f$idx[[fac]]]
}
gsrc <- function(tie, item, fac) sprintf("cache/01_prepared_data.rds: prep$%s_fit$fa$loadings['%s_%s', idx['%s']] (polychoric varimax PCA, 18 items)",
  tie, item, tie, fac)
for (k in list(c("g_women_weak_lib", "weak", "Knowwomen", "liberal", "Knowing women on liberal, weak ties (exploratory)"),
  c("g_women_weak_con", "weak", "Knowwomen", "conservative", "Knowing women on conservative, weak ties (exploratory)"),
  c("g_men_weak_lib", "weak", "Knowmen", "liberal", "Knowing men on liberal, weak ties (exploratory)"),
  c("g_men_weak_con", "weak", "Knowmen", "conservative", "Knowing men on conservative, weak ties (exploratory)"),
  c("g_men_strong_con", "strong", "Knowmen", "conservative", "Knowing men on conservative, strong ties (exploratory)"),
  c("g_men_strong_lib", "strong", "Knowmen", "liberal", "Knowing men on liberal, strong ties (exploratory)"))) {
  add(k[1], S2, d2(gl(k[2], k[3], k[4])), k[5], gsrc(k[2], k[3], k[4]))
}
for (tie in c("weak", "strong")) {
  m <- c07$fits[[tie]]$measures
  add(paste0("g_cfa_cfi_", tie), S2, d3(m["cfi.scaled"]), paste("Scaled CFI of the 18-item CFA with gender items,", tie, "ties"),
    sprintf("cache/07_reliability_cfa.rds: $fits$%s$measures['cfi.scaled']", tie))
  add(paste0("g_cfa_rmsea_", tie), S2, d3(m["rmsea.scaled"]), paste("Scaled RMSEA of the 18-item CFA with gender items,", tie, "ties"),
    sprintf("cache/07_reliability_cfa.rds: $fits$%s$measures['rmsea.scaled']", tie))
}
ng <- c09$rows
ng <- merge(ng[ng$variant == "Canonical", ], ng[ng$variant == "No gender", ], by = c("outcome", "model", "term"))
add("g_max_shift", S2, d3(max(abs(ng$est.x - ng$est.y))), "Largest network-coefficient shift, OLS refit without gender items",
  "cache/09_no_gender_robustness.rds: max |est(Canonical) - est(No gender)| over $rows")

vl <- function(x) { l <- x$loadings; l$std.all[l$op == "~~" & l$lhs == "V" & l$rhs == "L"] }
vl_w <- vl(mn$tableA2_weak); vl_s <- vl(mn$tableA2_strong)
add("cfa_vl_weak", S2, d2(vl_w), "Variety-liberal latent correlation in the strict weak-tie CFA",
  paste0(src_mn, ": tableA2_weak$loadings std.all for V ~~ L"))
add("cfa_vl_strong", S2, d2(vl_s), "Variety-liberal latent correlation in the strict strong-tie CFA",
  paste0(src_mn, ": tableA2_strong$loadings std.all for V ~~ L"))

efm <- sapply(c("weak", "strong"), function(t) fitMeasures(esem_t[[paste(t, "target")]]$fit, c("cfi.scaled", "rmsea.scaled", "srmr")))
src_et <- "cache/diagnostics/esem_ties_target.rds: fitMeasures($`<tie> target`$fit)"
add("tie_esem_cfi_weak", S2, d3(efm["cfi.scaled", "weak"]), "Scaled CFI of the weak-tie ESEM", src_et)
add("tie_esem_rmsea_weak", S2, d3(efm["rmsea.scaled", "weak"]), "Scaled RMSEA of the weak-tie ESEM", src_et)
add("tie_esem_srmr_weak", S2, d3(efm["srmr", "weak"]), "SRMR of the weak-tie ESEM", src_et)
add("tie_esem_cfi_strong", S2, d3(efm["cfi.scaled", "strong"]), "Scaled CFI of the strong-tie ESEM", src_et)
add("tie_esem_rmsea_strong", S2, d3(efm["rmsea.scaled", "strong"]), "Scaled RMSEA of the strong-tie ESEM", src_et)
add("tie_esem_srmr_strong", S2, d3(efm["srmr", "strong"]), "SRMR of the strong-tie ESEM", src_et)
cor_lv <- function(t) lavInspect(esem_t[[paste(t, "target")]]$fit, "cor.lv")
src_cl <- "cache/diagnostics/esem_ties_target.rds: lavInspect($`<tie> target`$fit, 'cor.lv')"
for (k in list(c("w", "weak"), c("s", "strong"))) {
  cm <- cor_lv(k[2])
  add(paste0("tie_esem_", k[1], "_vl"), S2, d2(cm[2, 1]),
    paste("Variety-liberal ESEM correlation,", k[2], "ties"), src_cl)
  add(paste0("tie_esem_", k[1], "_vc"), S2, d2(cm[3, 1]),
    paste("Variety-conservative ESEM correlation,", k[2], "ties"), src_cl)
  add(paste0("tie_esem_", k[1], "_lc"), S2, d2(cm[3, 2]),
    paste("Liberal-conservative ESEM correlation,", k[2], "ties"), src_cl)
}

omegas <- unlist(lapply(c("weak", "strong"), function(t) {
  st <- lavInspect(esem_t[[paste(t, "target")]]$fit, "std")
  lam <- as.matrix(st$lambda); psi <- as.matrix(st$psi); th <- as.matrix(st$theta)
  sapply(seq_len(ncol(lam)), function(k) {
    w <- lam[, k]
    common <- as.numeric(t(w) %*% (lam %*% psi %*% t(lam)) %*% w)
    common / (common + as.numeric(t(w) %*% th %*% w))
  })
}))
src_om <- "cache/diagnostics/esem_ties_target.rds: model-implied omega (w'L Psi L'w / (w'L Psi L'w + w'Theta w)), formula of Scripts/diagnostics/esem_reliability.R"
add("tie_omega_weak_min", S2, d2(min(omegas[1:3])), "Lowest model-implied omega among the three weak-tie ESEM factors", src_om)
add("tie_omega_weak_max", S2, d2(max(omegas[1:3])), "Highest model-implied omega among the three weak-tie ESEM factors", src_om)
add("tie_omega_strong_min", S2, d2(min(omegas[4:6])), "Lowest model-implied omega among the three strong-tie ESEM factors", src_om)
add("tie_omega_strong_max", S2, d2(max(omegas[4:6])), "Highest model-implied omega among the three strong-tie ESEM factors", src_om)

pj <- parameterEstimates(joint$matched$fit, standardized = TRUE)
vv_est <- pj$est[pj$op == "~~" & pj$lhs == "Wv" & pj$rhs == "Sv"]
rv_w <- pj$std.lv[pj$op == "~~" & pj$lhs == "Wv" & pj$rhs == "Wv"]
rv_s <- pj$std.lv[pj$op == "~~" & pj$lhs == "Sv" & pj$rhs == "Sv"]
add("joint_vv_cor", S2, d2(vv_est),
  "Weak-strong variety latent correlation in the joint model with matched residuals (the implied disturbance correlation exceeds 1)",
  "cache/diagnostics/sem_gpa_fixed_joint_covars.rds: $matched$fit parameterEstimates est for Wv ~~ Sv")
add("joint_vv_rescor", S2, claim(vv_est / sqrt(rv_w * rv_s) > 1, "1"),
  sprintf("Disturbance correlation implied by the joint model's Wv-Sv covariance and residual variances (computes to %.2f, exceeds 1)", vv_est / sqrt(rv_w * rv_s)),
  "cache/diagnostics/sem_gpa_fixed_joint_covars.rds: std.lv(Wv ~~ Sv) / sqrt(std.lv(Wv ~~ Wv) * std.lv(Sv ~~ Sv))")

# ---------------------------------------------------------------- Structural Models of Activity Participation
st_e <- lavInspect(esem_act, "std")
lam_e <- as.matrix(st_e$lambda); psi_e <- as.matrix(st_e$psi); th_e <- as.matrix(st_e$theta)
om_e <- sapply(seq_len(ncol(lam_e)), function(k) {
  w <- lam_e[, k]
  common <- as.numeric(t(w) %*% (lam_e %*% psi_e %*% t(lam_e)) %*% w)
  common / (common + as.numeric(t(w) %*% th_e %*% w))
})
add("act_resid_omega", S3, d2(om_e[3]),
  "Model-implied omega of the residual activity factor under the target ESEM (arts .97, leisure .74)",
  paste0(src_esem, ": model-implied omega, third factor (residual)"))
add("act_arts_omega", S1, d2(om_e[1]),
  "Model-implied omega of the arts factor under the target ESEM",
  paste0(src_esem, ": model-implied omega, first factor (arts)"))
add("act_leisure_omega", S1, d2(om_e[2]),
  "Model-implied omega of the solitary leisure factor under the target ESEM",
  paste0(src_esem, ": model-implied omega, second factor (leisure)"))
add("act_resid_omega_s1", S1, d2(om_e[3]),
  "Model-implied omega of the residual activity factor under the target ESEM, stated in the activities section",
  paste0(src_esem, ": model-implied omega, third factor (residual)"))
sem_dat <- lavInspect(sc$weak$fit, "data")
add("poli_min", S3, as.character(min(sem_dat[, "poli"])), "Political ideology scale minimum (1 to 7)",
  "cache/diagnostics/sem_gpa_fixed_sep_covars.rds: min(lavInspect($weak$fit, 'data')[, 'poli'])")
add("poli_max", S3, as.character(max(sem_dat[, "poli"])), "Political ideology scale maximum (1 to 7)",
  "cache/diagnostics/sem_gpa_fixed_sep_covars.rds: max(lavInspect($weak$fit, 'data')[, 'poli'])")
add("educ_min", S3, as.character(min(sem_dat[, "educ_ord"])), "Education ordinal minimum (high school or less)",
  "cache/diagnostics/sem_gpa_fixed_sep_covars.rds: min(lavInspect($weak$fit, 'data')[, 'educ_ord'])")
add("educ_max", S3, as.character(max(sem_dat[, "educ_ord"])), "Education ordinal maximum (graduate credentials)",
  "cache/diagnostics/sem_gpa_fixed_sep_covars.rds: max(lavInspect($weak$fit, 'data')[, 'educ_ord'])")
n_sem <- sapply(sc, function(x) lavInspect(x$fit, "ntotal"))
add("sem_n", S3, claim(n_sem[["weak"]] == n_sem[["strong"]], nfmt(n_sem[["weak"]])), "Complete-case N of both structural models",
  "cache/diagnostics/sem_gpa_fixed_sep_covars.rds: lavInspect($<tie>$fit, 'ntotal')")
for (tie in c("weak", "strong")) {
  fm <- fitMeasures(sc[[tie]]$fit, c("cfi.scaled", "rmsea.scaled", "srmr"))
  s <- sprintf("cache/diagnostics/sem_gpa_fixed_sep_covars.rds: fitMeasures($%s$fit, '%%s')", tie)
  add(paste0("sem_cfi_", tie), S3, d3(fm[1]), paste("Structural model scaled CFI,", tie, "ties"), sprintf(s, "cfi.scaled"))
  add(paste0("sem_rmsea_", tie), S3, d3(fm[2]), paste("Structural model scaled RMSEA,", tie, "ties"), sprintf(s, "rmsea.scaled"))
  add(paste0("sem_srmr_", tie), S3, d3(fm[3]), paste("Structural model SRMR,", tie, "ties"), sprintf(s, "srmr"))
}

pes <- lapply(sc, function(x) parameterEstimates(x$fit, standardized = TRUE))
path <- function(tie, lhs, rhs) {
  r <- pes[[tie]][pes[[tie]]$op == "~" & pes[[tie]]$lhs == lhs & pes[[tie]]$rhs == rhs, ]
  stopifnot(nrow(r) == 1)
  list(b = r$std.all, p = r$pvalue,
    src = sprintf("cache/diagnostics/sem_gpa_fixed_sep_covars.rds: std.all %s ~ %s, $%s$fit", lhs, rhs, tie))
}
pathrow <- function(id, tie, lhs, rhs, desc, p_style = c("none", "exact", "thr"), sec = S3) {
  p_style <- match.arg(p_style)
  x <- path(tie, lhs, rhs)
  add(id, sec, d2(x$b), desc, x$src)
  if (p_style == "exact") add(paste0(id, "_p"), sec, pv(x$p), paste(desc, "(p-value)"), sub("std.all", "pvalue", x$src))
  if (p_style == "thr") add(paste0(id, "_p"), sec, thr(x$p), paste(desc, "(p threshold)"), sub("std.all", "pvalue", x$src))
}
pathrow("p_wv_arts", "weak", "A", "Wv", "Weak variety -> arts", "thr", sec = S3b)
pathrow("p_sv_arts", "strong", "A", "Sv", "Strong variety -> arts", "thr", sec = S3b)
comp_arts <- list(path("weak", "A", "Wl"), path("weak", "A", "Wc"), path("strong", "A", "Sl"), path("strong", "A", "Sc"))
cb <- sapply(comp_arts, `[[`, "b"); cp <- sapply(comp_arts, `[[`, "p")
src_comp <- "cache/diagnostics/sem_gpa_fixed_sep_covars.rds: std.all A ~ Wl, Wc (weak) and A ~ Sl, Sc (strong)"
add("p_comp_arts_min", S3b, claim(all(cb > 0), d2(min(cb))), "Smallest liberal or conservative composition path on arts (both ties)", src_comp)
add("p_comp_arts_max", S3b, claim(all(cb > 0), d2(max(cb))), "Largest liberal or conservative composition path on arts (both ties)", src_comp)
add("p_comp_arts_p", S3b, thr(max(cp)), "All four composition paths on arts significant (p threshold)", sub("std.all", "pvalue", src_comp))

fdec <- readRDS("cache/diagnostics/sem_gpa_fixed_fit_decomp.rds")
add("decomp_cfi_weak", S3, d3(fdec$weak$fm1[["cfi.scaled"]]), "CFI, fixed-loading measurement model without regressions, weak ties",
  "cache/diagnostics/sem_gpa_fixed_fit_decomp.rds: $weak$fm1['cfi.scaled']")
add("decomp_rmsea_weak", S3, d3(fdec$weak$fm1[["rmsea.scaled"]]), "RMSEA, fixed-loading measurement model without regressions, weak ties",
  "cache/diagnostics/sem_gpa_fixed_fit_decomp.rds: $weak$fm1['rmsea.scaled']")
add("decomp_srmr_weak", S3, d3(fdec$weak$fm1[["srmr"]]), "SRMR, fixed-loading measurement model without regressions, weak ties",
  "cache/diagnostics/sem_gpa_fixed_fit_decomp.rds: $weak$fm1['srmr']")
add("decomp_cfi_strong", S3, d3(fdec$strong$fm1[["cfi.scaled"]]), "CFI, fixed-loading measurement model without regressions, strong ties",
  "cache/diagnostics/sem_gpa_fixed_fit_decomp.rds: $strong$fm1['cfi.scaled']")
add("decomp_rmsea_strong", S3, d3(fdec$strong$fm1[["rmsea.scaled"]]), "RMSEA, fixed-loading measurement model without regressions, strong ties",
  "cache/diagnostics/sem_gpa_fixed_fit_decomp.rds: $strong$fm1['rmsea.scaled']")
add("decomp_srmr_strong", S3, d3(fdec$strong$fm1[["srmr"]]), "SRMR, fixed-loading measurement model without regressions, strong ties",
  "cache/diagnostics/sem_gpa_fixed_fit_decomp.rds: $strong$fm1['srmr']")
seed <- regmatches(readLines("Scripts/diagnostics/sem_gpa_fixed_splithalf.R"),
  regexpr("set\\.seed\\(\\d+\\)", readLines("Scripts/diagnostics/sem_gpa_fixed_splithalf.R")))
add("splithalf_seed", S3b, gsub("\\D", "", seed[1]), "Random seed for the split-half check",
  "Scripts/diagnostics/sem_gpa_fixed_splithalf.R: set.seed() call (seed not stored in cache/diagnostics/sem_gpa_fixed_splithalf.rds)")

pathrow("p_wl_leis", "weak", "Lis", "Wl", "Weak liberal -> leisure", "thr", sec = S3b)
pathrow("p_sl_leis", "strong", "Lis", "Sl", "Strong liberal -> leisure", "thr", sec = S3b)
pathrow("p_wv_leis", "weak", "Lis", "Wv", "Weak variety -> leisure", "exact", sec = S3b)
pathrow("p_sv_leis", "strong", "Lis", "Sv", "Strong variety -> leisure", "exact", sec = S3b)
pathrow("p_wv_resid", "weak", "R", "Wv", "Weak variety -> residual", "exact", sec = S3b)
pathrow("p_sv_resid", "strong", "R", "Sv", "Strong variety -> residual", "exact", sec = S3b)
pathrow("p_wl_resid", "weak", "R", "Wl", "Weak liberal -> residual", "exact", sec = S3b)

dpe <- lapply(dc, function(x) { p <- parameterEstimates(x$fit, standardized = TRUE); p[p$op == ":=", ] })
eff <- function(tie, lab) {
  r <- dpe[[tie]][dpe[[tie]]$lhs == lab, ]
  stopifnot(nrow(r) == 1)
  list(b = r$std.all, p = r$pvalue,
    src = sprintf("cache/diagnostics/sem_gpa_fixed_sep_decomposition.rds: $%s$fit := %s std.all (delta method)", tie, lab))
}
effrow <- function(id, tie, lab, desc, p_style = c("none", "exact", "thr"), sec = S3) {
  p_style <- match.arg(p_style)
  x <- eff(tie, lab)
  add(id, sec, d2(x$b), desc, x$src)
  if (p_style == "exact") add(paste0(id, "_p"), sec, pv(x$p), paste(desc, "(p-value)"), sub("std.all", "pvalue", x$src))
  if (p_style == "thr") add(paste0(id, "_p"), sec, thr(x$p), paste(desc, "(p threshold)"), sub("std.all", "pvalue", x$src))
}
for (tie in c("weak", "strong")) {
  effrow(paste0("tot_A_educ_", tie), tie, "tot_A_educ", paste("Total effect of education on arts,", tie, "ties"), sec = S3c)
  effrow(paste0("ind_A_educ_", tie), tie, "ind_A_educ", paste("Network-mediated effect of education on arts,", tie, "ties"), sec = S3c)
  effrow(paste0("tot_A_arts_", tie), tie, "tot_A_arts", paste("Total effect of childhood arts on arts,", tie, "ties"), sec = S3c)
  effrow(paste0("ind_A_arts_", tie), tie, "ind_A_arts", paste("Network-mediated effect of childhood arts on arts,", tie, "ties"), sec = S3c)
  effrow(paste0("tot_A_poli_", tie), tie, "tot_A_poli", paste("Total effect of political ideology on arts,", tie, "ties"), sec = S3c)
  effrow(paste0("dir_A_poli_", tie), tie, "dir_A_poli", paste("Direct effect of political ideology on arts,", tie, "ties"), sec = S3c)
  effrow(paste0("ind_A_poli_", tie), tie, "ind_A_poli", paste("Network-mediated effect of political ideology on arts,", tie, "ties"), sec = S3c)
  effrow(paste0("tot_Lis_poli_", tie), tie, "tot_Lis_poli", paste("Total effect of political ideology on leisure,", tie, "ties"), "exact", sec = S3c)
}
pctrow <- function(id, tie, ind_lab, tot_lab, desc, sec = S3) {
  x <- eff(tie, ind_lab); t <- eff(tie, tot_lab)
  add(id, sec, as.character(floor(100 * x$b / t$b + 0.5)), desc,
    sprintf("cache/diagnostics/sem_gpa_fixed_sep_decomposition.rds: 100 * := %s std.all / := %s std.all (delta method)", ind_lab, tot_lab))
}
pctrow("sh_A_educ_weak", "weak", "ind_A_educ", "tot_A_educ", "Share of education's total effect on arts through weak ties (percent)", sec = S3c)
pctrow("sh_A_educ_strong", "strong", "ind_A_educ", "tot_A_educ", "Share of education's total effect on arts through strong ties (percent)", sec = S3c)
pctrow("sh_A_arts_weak", "weak", "ind_A_arts", "tot_A_arts", "Share of childhood arts total effect on arts through weak ties (percent)", sec = S3c)
pctrow("sh_A_arts_strong", "strong", "ind_A_arts", "tot_A_arts", "Share of childhood arts total effect on arts through strong ties (percent)", sec = S3c)
pctrow("sh_A_poli_weak", "weak", "ind_A_poli", "tot_A_poli", "Share of ideology total effect on arts through weak ties (percent)", sec = S3c)
pctrow("sh_A_poli_strong", "strong", "ind_A_poli", "tot_A_poli", "Share of ideology total effect on arts through strong ties (percent)", sec = S3c)
pctrow("sh_Lis_poli_weak", "weak", "ind_Lis_poli", "tot_Lis_poli", "Share of ideology total effect on leisure through weak ties (percent)", sec = S3c)
pctrow("sh_Lis_poli_strong", "strong", "ind_Lis_poli", "tot_Lis_poli", "Share of ideology total effect on leisure through strong ties (percent)", sec = S3c)
pctrow("sh_dir_A_educ_weak", "weak", "dir_A_educ", "tot_A_educ", "Direct share of education's total effect on arts, weak ties (percent)", sec = S3c)
pctrow("sh_dir_A_educ_strong", "strong", "dir_A_educ", "tot_A_educ", "Direct share of education's total effect on arts, strong ties (percent)", sec = S3c)
pctrow("sh_dir_A_arts_weak", "weak", "dir_A_arts", "tot_A_arts", "Direct share of childhood arts total effect on arts, weak ties (percent)", sec = S3c)
pctrow("sh_dir_A_arts_strong", "strong", "dir_A_arts", "tot_A_arts", "Direct share of childhood arts total effect on arts, strong ties (percent)", sec = S3c)
artpath <- function(tie) {
  p <- parameterEstimates(sc[[tie]]$fit, standardized = TRUE)
  setNames(p$std.all[p$op == "~" & p$lhs == "A"], p$rhs[p$op == "~" & p$lhs == "A"])
}
vw <- artpath("weak"); vs <- artpath("strong")
add("ratio_arts_weak_min", S3b, as.character(round(min(vw["Wv"] / vw["Wl"], vw["Wv"] / vw["Wc"]))),
  "Smallest ratio of the weak variety path to the composition paths on arts",
  "cache/diagnostics/sem_gpa_fixed_sep_covars.rds: min of std.all(A~Wv)/std.all(A~Wl) and std.all(A~Wv)/std.all(A~Wc)")
add("ratio_arts_strong_min", S3b, as.character(round(min(vs["Sv"] / vs["Sl"], vs["Sv"] / vs["Sc"]))),
  "Smallest ratio of the strong variety path to the composition paths on arts",
  "cache/diagnostics/sem_gpa_fixed_sep_covars.rds: min of std.all(A~Sv)/std.all(A~Sl) and std.all(A~Sv)/std.all(A~Sc)")
add("ratio_arts_strong_max", S3b, as.character(round(max(vs["Sv"] / vs["Sl"], vs["Sv"] / vs["Sc"]))),
  "Largest ratio of the strong variety path to the composition paths on arts",
  "cache/diagnostics/sem_gpa_fixed_sep_covars.rds: max of std.all(A~Sv)/std.all(A~Sl) and std.all(A~Sv)/std.all(A~Sc)")
add("ind_A_poli_p", S3c, thr(max(eff("weak", "ind_A_poli")$p, eff("strong", "ind_A_poli")$p)),
  "Both mediated ideology-to-arts paths significant (p threshold)",
  "cache/diagnostics/sem_gpa_fixed_sep_decomposition.rds: max pvalue of := ind_A_poli over weak and strong")
effrow("ind_Lis_poli_weak", "weak", "ind_Lis_poli", "Network-mediated effect of political ideology on leisure, weak ties", "exact", sec = S3c)
effrow("ind_Lis_poli_strong", "strong", "ind_Lis_poli", "Network-mediated effect of political ideology on leisure, strong ties", "exact", sec = S3c)
tots <- sapply(c("weak", "strong"), function(t) { r <- dpe[[t]][grepl("^tot_", dpe[[t]]$lhs), ]; setNames(r$std.all, r$lhs) })
add("tot_maxdiff", S3c, d3(max(abs(tots[, "weak"] - tots[, "strong"]))),
  "Largest weak-strong difference in total effects over predictors and activity factors",
  "cache/diagnostics/sem_gpa_fixed_sep_decomposition.rds: max |tot_* std.all (weak) - tot_* std.all (strong)|")

for (k in list(c("educ_v", "Wv", "Sv", "educ_ord", "Education -> network variety", S3a),
  c("poli_l", "Wl", "Sl", "poli", "Political ideology -> liberal composition", S3a),
  c("poli_c", "Wc", "Sc", "poli", "Political ideology -> conservative composition", S3a),
  c("aapi_c", "Wc", "Sc", "race_fAAPI", "AAPI -> conservative composition", S3a),
  c("educ_a", "A", "A", "educ_ord", "Education -> arts (direct)", S3c),
  c("child_a", "A", "A", "child_arts", "Childhood arts exposure -> arts (direct)", S3c),
  c("aapi_a", "A", "A", "race_fAAPI", "AAPI -> arts (direct)", S3d))) {
  pathrow(paste0("c_", k[1], "_weak"), "weak", k[2], k[4], paste(k[5], "weak ties"), sec = k[6])
  pathrow(paste0("c_", k[1], "_strong"), "strong", k[3], k[4], paste(k[5], "strong ties"), sec = k[6])
}

out <- do.call(rbind, reg)
stopifnot(!anyDuplicated(out$id))
dir.create("cache/manuscript", showWarnings = FALSE)
write.csv(out, "cache/manuscript/prose_numbers.csv", row.names = FALSE)
cat(sprintf("wrote cache/manuscript/prose_numbers.csv (%d rows; %d failed claims)\n",
  nrow(out), sum(out$value == "CLAIM FAILS")))
