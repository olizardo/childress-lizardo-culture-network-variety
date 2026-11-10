#' Builds the measurement tables of results_draft.tex (Tables 1 to 4, A1, A2) from saved
#' caches and refits. Writes cache/manuscript/measurement_blocks.tex (LaTeX fragments) and
#' cache/manuscript/measurement_numbers.rds (every number printed in those tables).
#'
#' Sources:
#'   cache/01_prepared_data.rds             Table 1 (varimax PCA loadings, prep$act_fa)
#'   cache/diagnostics/esem_ties_target.rds Table 4 (tie ESEM target loadings)
#'   refits below                           Tables 2, 3, A1 (activity CFAs, ESEM, oblimin EFA)
#'                                          and A2 (tie three-factor CFAs with antonym residuals)
#'
#' Run from the project root: Rscript Scripts/manuscript/build_measurement_blocks.R
#' Runtime is roughly 10 to 15 minutes; run in the background with nohup.

suppressPackageStartupMessages({
  library(lavaan); library(psych)
})

prep <- readRDS("cache/01_prepared_data.rds")
df <- prep$df
items <- prep$activity_items
s07 <- readRDS("cache/07_reliability_cfa.rds")$structures
esem_t <- readRDS("cache/diagnostics/esem_ties_target.rds")

arts_items <- c("museum", "art_gallery", "symphony_orchestra_opera",
  "carnival_fair_amusement_park", "music_concert_festival", "play_or_musical",
  "library", "fancy_restaurant", "dance_performance", "movie_theater",
  "attend_sports", "hiking_camping_boating", "historic_site", "go_to_festival")
leisure_items <- c("go_for_walk", "exercise_or_yoga", "read_novel_poem_or_play")
resid_items <- c("gardening", "fast_food", "home_auto_repair")

act_df <- as.data.frame(lapply(df[items], function(x) as.numeric(unclass(x))))
act_full <- act_df[complete.cases(act_df), ]
N_act <- nrow(act_full)

act_labels <- c(museum = "Museum", art_gallery = "Art gallery",
  symphony_orchestra_opera = "Symphony, orchestra, or opera",
  carnival_fair_amusement_park = "Carnival, fair, or amusement park",
  music_concert_festival = "Music concert or festival", play_or_musical = "Play or musical",
  library = "Library", fancy_restaurant = "Fancy restaurant", dance_performance = "Dance performance",
  movie_theater = "Movie theater", attend_sports = "Attend sports",
  hiking_camping_boating = "Hiking, camping, or boating", historic_site = "Historic site",
  go_to_festival = "Go to festival", go_for_walk = "Go for a walk", exercise_or_yoga = "Exercise or yoga",
  read_novel_poem_or_play = "Read a novel, poem, or play", gardening = "Gardening",
  fast_food = "Fast food", home_auto_repair = "Home auto repair")
assign_of <- c(setNames(rep("Arts", 14), arts_items), setNames(rep("Leisure", 3), leisure_items),
  setNames(rep("Residual", 3), resid_items))

fmt <- function(x) { r <- round(x, 2); ifelse(r < 0, paste0("$-$", sprintf("%.2f", abs(r))), sprintf("%.2f", abs(r) + 0)) }
bold_if <- function(txt, b) ifelse(b, paste0("\\textbf{", txt, "}"), txt)

# ---------------------------------------------------------------- Table 1: varimax PCA loadings
lv <- unclass(prep$act_fa$loadings)[items, ]
colnames(lv) <- c("Arts", "Leisure", "Residual")
t1 <- sapply(c(arts_items, leisure_items, resid_items), function(it) {
  a <- assign_of[[it]]
  vals <- sapply(c("Arts", "Leisure", "Residual"), function(cn)
    bold_if(fmt(lv[it, cn]), cn == a))
  c(act_labels[[it]], a, vals)
})
t1 <- t(t1)

# ---------------------------------------------------------------- Table 2 and Table 3: CFAs and ESEM
cfa_fit <- function(model) cfa(model, data = act_full, ordered = items, estimator = "WLSMV", std.lv = TRUE)
mod1 <- paste("f =~", paste(items, collapse = " + "))
mod2_arts <- paste("arts =~", paste(c(arts_items, resid_items), collapse = " + "),
  "\nleisure =~", paste(leisure_items, collapse = " + "))
mod2_leis <- paste("arts =~", paste(arts_items, collapse = " + "),
  "\nleisure =~", paste(c(leisure_items, resid_items), collapse = " + "))
mod3 <- paste("arts =~", paste(arts_items, collapse = " + "),
  "\nleisure =~", paste(leisure_items, collapse = " + "),
  "\nresidual =~", paste(resid_items, collapse = " + "))
f1 <- cfa_fit(mod1); f2a <- cfa_fit(mod2_arts); f2l <- cfa_fit(mod2_leis); f3 <- cfa_fit(mod3)

sf_of <- function(f) slot(f, "test")[[2]]$scaling.factor
sb <- function(unres, res) {
  dfu <- unlist(fitMeasures(unres, "df")); dfr <- unlist(fitMeasures(res, "df"))
  m <- dfr - dfu
  cd <- (dfr * sf_of(res) - dfu * sf_of(unres)) / m
  Td <- (unlist(fitMeasures(res, "chisq.scaled")) - unlist(fitMeasures(unres, "chisq.scaled"))) / cd
  c(Td = unname(Td), df = unname(m))
}

esem <- efa(data = act_full, nfactors = 3, ordered = items, estimator = "WLSMV",
  rotation = list("target", target = {
    tm <- matrix(0, nrow = length(items), ncol = 3, dimnames = list(items, c("arts", "leisure", "residual")))
    tm[arts_items, "arts"] <- 1; tm[leisure_items, "leisure"] <- 1; tm[resid_items, "residual"] <- 1; tm
  }))
esem_obj <- esem[[1]]
esem_lam <- lavInspect(esem_obj, "std")$lambda
colnames(esem_lam) <- c("arts", "leisure", "residual")
esem_lam <- esem_lam[items, , drop = FALSE]

# Model-implied omega for a fitted SEM's factors (same formula as build_prose_numbers.R)
factor_omega <- function(fit) {
  st <- lavInspect(fit, "std")
  lam <- as.matrix(st$lambda); psi <- as.matrix(st$psi); th <- as.matrix(st$theta)
  sapply(seq_len(ncol(lam)), function(k) {
    w <- lam[, k]
    common <- as.numeric(t(w) %*% (lam %*% psi %*% t(lam)) %*% w)
    common / (common + as.numeric(t(w) %*% th %*% w))
  })
}
om_act <- factor_omega(esem_obj)

fit_row <- function(f) fitMeasures(f, c("chisq.scaled", "df", "cfi.scaled", "rmsea.scaled", "srmr"))
f1r <- fit_row(f1); f2ar <- fit_row(f2a); f2lr <- fit_row(f2l); f3r <- fit_row(f3)
er <- fitMeasures(esem_obj, c("chisq.scaled", "df", "cfi.scaled", "rmsea.scaled", "srmr"))
t2 <- list(
  list("One-factor CFA", f1r, sprintf("%.1f (%d), p \\textless{} .001", sb(f3, f1)["Td"], as.integer(sb(f3, f1)["df"]))),
  list("Two-factor CFA, residual merged into arts", f2ar, sprintf("%.1f (%d), p \\textless{} .001", sb(f3, f2a)["Td"], as.integer(sb(f3, f2a)["df"]))),
  list("Two-factor CFA, residual merged into leisure", f2lr, sprintf("%.1f (%d), p \\textless{} .001", sb(f3, f2l)["Td"], as.integer(sb(f3, f2l)["df"]))),
  list("Three-factor CFA", f3r, "---"),
  list("ESEM, target rotation", er, sprintf("%.1f (%d), p \\textless{} .001", sb(esem_obj, f3)["Td"], as.integer(sb(esem_obj, f3)["df"]))))

# Table 3 loadings (ESEM), with bold on the assigned factor
t3 <- t(sapply(c(arts_items, leisure_items, resid_items), function(it) {
  a <- assign_of[[it]]
  vals <- sapply(c("arts", "leisure", "residual"), function(cn) {
    tgt <- c(arts = "Arts", leisure = "Leisure", residual = "Residual")[[cn]]
    bold_if(fmt(esem_lam[it, cn]), tgt == a)
  })
  c(act_labels[[it]], a, vals)
}))
t3 <- rbind(t3, "MIDRULE", c("Model-implied omega", "", sapply(om_act, fmt)))

# ---------------------------------------------------------------- Table A1: oblimin EFA
ef_obi <- fa(polychoric(act_full)$rho, nfactors = 3, rotate = "oblimin", fm = "wls", n.obs = N_act)
pl <- unclass(ef_obi$loadings)[items, , drop = FALSE]
aa <- assign_of[items]
labs <- c("Arts", "Leisure", "Residual")
M <- sapply(labs, function(a) colSums(abs(pl[aa == a, , drop = FALSE])))  # M[factor, label]
perms <- list(c(1, 2, 3), c(1, 3, 2), c(2, 1, 3), c(2, 3, 1), c(3, 1, 2), c(3, 2, 1))
best <- perms[[which.max(sapply(perms, function(pm) sum(M[cbind(pm, 1:3)])))]]
pl <- pl[, best, drop = FALSE]
for (j in 1:2) pl[, j] <- pl[, j] * sign(sum(pl[aa == labs[j], j]))
# Residual orientation follows the printed table (column sum negative). Sign is arbitrary.
pl[, 3] <- pl[, 3] * ifelse(sum(pl[aa == labs[3], 3]) > 0, -1, 1)
colnames(pl) <- labs
ob_phi <- ef_obi$Phi[best, best]
colnames(pl) <- c("Arts", "Leisure", "Residual")
ta1 <- t(sapply(c(arts_items, leisure_items, resid_items), function(it) {
  a <- assign_of[[it]]
  vals <- sapply(c("Arts", "Leisure", "Residual"), function(cn) bold_if(fmt(pl[it, cn]), cn == a))
  c(act_labels[[it]], a, vals)
}))

# ---------------------------------------------------------------- Table 4 and Table A2: tie batteries
s_weak <- s07$weak; s_strong <- s07$strong
tie_items <- function(s) lapply(c("variety", "liberal", "conservative"), function(f) {
  x <- names(s[[f]]); x[!grepl("^Know(men|women)_", x)]
})
label_tie <- function(x) {
  base <- sub("_(weak|strong)$", "", x)
  lab <- c(KnowAsianppl = "Asian people", Know2ndHome = "Second home", KnowBornElsewhere = "Born elsewhere",
    KnowMENAppl = "MENA people", KnowHawaiiPI = "NHPI people", KnowAmIndianppl = "American Indian people",
    KnowLGBTQ = "LGBTQ people", KnowLittleChurch = "Little church", KnowVeryLib = "Very liberal",
    KnowHispanixppl = "Hispanic or Latine people", KnowBlackppl = "Black people", KnowCityppl = "City people",
    KnowLotsaChurch = "Lots of church", KnowVeryCons = "Very conservative", KnowWhiteppl = "White people",
    KnowRuralppl = "Rural people")
  unname(lab[base])
}
tie_block <- function(tie) {
  e <- esem_t[[paste(tie, "target")]]
  lam <- unclass(e$loadings)
  ti <- unlist(tie_items(s07[[tie]]))
  tgt <- e$target
  rownames(lam) <- ti; rownames(tgt) <- ti
  list(lam = lam, tgt = tgt, items = ti)
}
bw <- tie_block("weak"); bs <- tie_block("strong")
t4 <- t(sapply(seq_along(bw$items), function(i) {
  it <- bw$items[i]
  w <- sapply(1:3, function(k) bold_if(fmt(bw$lam[it, k]), bw$tgt[it, k] == 1))
  its <- paste0(sub("_weak$", "", it), "_strong")
  st <- sapply(1:3, function(k) bold_if(fmt(bs$lam[its, k]), bs$tgt[its, k] == 1))
  c(label_tie(it), w, st)
}))

# Table A2: baseline three-factor tie CFAs (script 07 sorting) and antonym-freed residuals
a2_fit <- function(tie) {
  il <- tie_items(s07[[tie]])
  fn <- c("variety", "liberal", "conservative")
  base <- paste(sprintf("%s =~ %s", c("V", "L", "C"), sapply(il, paste, collapse = " + ")), collapse = "\n")
  ant <- sprintf("Know%s_%s ~~ Know%s_%s", c("VeryLib", "LittleChurch", "Cityppl"), tie,
    c("VeryCons", "LotsaChurch", "Ruralppl"), tie)
  it <- unlist(il)
  d <- df[complete.cases(df[it]), it]
  d[] <- lapply(d, function(x) as.numeric(unclass(x)))
  f0 <- cfa(base, data = d, ordered = it, estimator = "WLSMV")
  f1a <- cfa(paste(base, paste(ant, collapse = "\n"), sep = "\n"), data = d, ordered = it, estimator = "WLSMV")
  pe1 <- parameterEstimates(f1a, standardized = TRUE)
  rr <- pe1[pe1$op == "~~" & pe1$lhs != pe1$rhs & pe1$lhs %in% it & pe1$rhs %in% it, ]
  list(loadings = parameterEstimates(f0, standardized = TRUE), antonym = rr, n = nrow(d),
    ant_se = rr$se)
}
a2_weak <- a2_fit("weak"); a2_strong <- a2_fit("strong")

# ---------------------------------------------------------------- note numbers (computed from the fits)
num <- list()
num$n_activities <- N_act
num$ve_activities <- sum(unclass(prep$act_fa$loadings)[items, ]^2) / length(items)

# Table 2: latent factor correlations in the three-factor CFA and RMSEA CIs for every model
num$cor_cfa3 <- lavInspect(f3, "std")$psi
rm_ci <- function(f) fitMeasures(f, c("rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled"))
num$rmsea_ci <- list(one = rm_ci(f1), two_arts = rm_ci(f2a), two_leis = rm_ci(f2l),
  three = rm_ci(f3), esem = rm_ci(esem_obj))

# Table 3: ESEM factor correlations
num$cor_esem_act <- lavInspect(esem_obj, "cor.lv")

# Table 4: factor correlations and N per battery (complete cases on the 16 non-gender items)
tie_dat <- function(tie) {
  it <- unlist(tie_items(s07[[tie]]))
  d <- df[complete.cases(df[it]), it]
  d[] <- lapply(d, function(x) as.numeric(unclass(x)))
  d
}
num$n_weak <- nrow(tie_dat("weak")); num$n_strong <- nrow(tie_dat("strong"))
num$cor_esem_weak <- lavInspect(esem_t[["weak target"]]$fit, "cor.lv")
num$cor_esem_strong <- lavInspect(esem_t[["strong target"]]$fit, "cor.lv")

# Model-implied omega for each tie ESEM factor
om_weak <- factor_omega(esem_t[["weak target"]]$fit)
om_strong <- factor_omega(esem_t[["strong target"]]$fit)
t4 <- rbind(t4, "MIDRULE", c("Model-implied omega", sapply(om_weak, fmt), sapply(om_strong, fmt)))

# Table A1: EFA fit statistics and factor correlations (psych stores RMSEA as a named
# numeric vector: RMSEA, lower, upper, confidence)
rm <- ef_obi$RMSEA
num$efa <- list(chi = ef_obi$chi, df = 133, tli = ef_obi$TLI,
  rmsea = rm[1], lo = rm[2], hi = rm[3], phi = ob_phi)

# Table A2: sample size and the largest antonym-residual standard error (unstandardized)
num$n_ties <- a2_weak$n
num$ant_se_max <- max(abs(c(a2_weak$ant_se, a2_strong$ant_se)))

numbers <- list(table1 = lv, table2 = list(one = f1r, two_arts = f2ar, two_leis = f2lr, three = f3r, esem = er),
  table3 = esem_lam, tableA1 = pl, oblimin_phi = ob_phi, table4_weak = bw$lam, table4_strong = bs$lam,
  tableA2_weak = a2_weak, tableA2_strong = a2_strong, n_activities = N_act, note_numbers = num)
saveRDS(numbers, "cache/manuscript/measurement_numbers.rds")

# ---------------------------------------------------------------- full blocks: caption, table, note
write_rows <- function(M) apply(M, 1, function(r) {
  if (r[1] == "MIDRULE") return("\\midrule")
  paste(paste(r, collapse = " & "), "\\\\")
})
# Table A2 rows: weak and strong baseline loadings, then antonym residuals
fac_lab <- c(V = "Variety", L = "Liberal", C = "Conservative")
a2_rows <- character()
wl <- a2_weak$loadings; sl <- a2_strong$loadings
wl <- wl[wl$op == "=~" & wl$lhs %in% names(fac_lab), ]; sl <- sl[sl$op == "=~" & sl$lhs %in% names(fac_lab), ]
for (k in seq_len(nrow(wl))) {
  it_w <- wl$rhs[k]; it_s <- paste0(sub("_weak$", "", it_w), "_strong")
  sr <- sl[sl$rhs == it_s, ]
  a2_rows <- c(a2_rows, paste(label_tie(it_w), "&", fac_lab[[wl$lhs[k]]], "&",
    sprintf("%.2f", wl$std.all[k]), "&", fac_lab[[sr$lhs]], "&", sprintf("%.2f", sr$std.all), "\\\\"))
}
ant_pairs <- list(c("KnowVeryLib", "KnowVeryCons"), c("KnowLittleChurch", "KnowLotsaChurch"), c("KnowCityppl", "KnowRuralppl"))
ant_word <- c(KnowVeryLib = "Very liberal", KnowVeryCons = "very conservative", KnowLittleChurch = "Little church",
  KnowLotsaChurch = "lots of church", KnowCityppl = "City people", KnowRuralppl = "rural people")
ant_value <- function(a2, pair) {
  r <- a2$antonym
  x <- paste0(pair[1], "_", ifelse(identical(a2, a2_weak), "weak", "strong"))
  y <- paste0(pair[2], "_", ifelse(identical(a2, a2_weak), "weak", "strong"))
  k <- which((r$lhs == x & r$rhs == y) | (r$lhs == y & r$rhs == x))
  stopifnot(length(k) == 1)
  r$std.all[k]
}
a2_res <- sapply(ant_pairs, function(pr) paste(
  paste0(ant_word[[pr[1]]], " with ", ant_word[[pr[2]]]), "& Residual &",
  fmt(ant_value(a2_weak, pr)), "& Residual &", fmt(ant_value(a2_strong, pr)), "\\\\"))
a2_rows <- c(a2_rows, a2_res)

lt_spec_5 <- "\\begin{longtable}[]{@{}
  >{\\raggedright\\arraybackslash}p{(\\columnwidth - 8\\tabcolsep) * \\real{0.4200}}
  >{\\raggedright\\arraybackslash}p{(\\columnwidth - 8\\tabcolsep) * \\real{0.1600}}
  >{\\raggedright\\arraybackslash}p{(\\columnwidth - 8\\tabcolsep) * \\real{0.1400}}
  >{\\raggedright\\arraybackslash}p{(\\columnwidth - 8\\tabcolsep) * \\real{0.1400}}
  >{\\raggedright\\arraybackslash}p{(\\columnwidth - 8\\tabcolsep) * \\real{0.1400}}@{}}"
lt_spec_7 <- "\\begin{longtable}[]{@{}
  >{\\raggedright\\arraybackslash}p{(\\columnwidth - 12\\tabcolsep) * \\real{0.3000}}
  >{\\raggedright\\arraybackslash}p{(\\columnwidth - 12\\tabcolsep) * \\real{0.0600}}
  >{\\raggedright\\arraybackslash}p{(\\columnwidth - 12\\tabcolsep) * \\real{0.1200}}
  >{\\raggedright\\arraybackslash}p{(\\columnwidth - 12\\tabcolsep) * \\real{0.0800}}
  >{\\raggedright\\arraybackslash}p{(\\columnwidth - 12\\tabcolsep) * \\real{0.1000}}
  >{\\raggedright\\arraybackslash}p{(\\columnwidth - 12\\tabcolsep) * \\real{0.1000}}
  >{\\raggedright\\arraybackslash}p{(\\columnwidth - 12\\tabcolsep) * \\real{0.2400}}@{}}"

long_block <- function(needspace, caption, spec, header, rows_, note) c(
  paste0("\\Needspace{", needspace, "\\baselineskip}"), caption, "", spec,
  "\\toprule", header, "\\midrule", rows_, "\\midrule", "\\endhead", "\\bottomrule", "\\end{longtable}", "", note)

star_block <- function(needspace, caption, spec, header, rows_, note, midrule_before = character(0)) c(
  paste0("\\Needspace{", needspace, "\\baselineskip}"), caption, "", spec,
  "\\toprule", header, "\\midrule", midrule_before, rows_, "\\bottomrule", "\\end{tabular*}", "", note)

t4_spec <- "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}>{\\raggedright\\arraybackslash}p{0.30\\textwidth} >{\\raggedleft\\arraybackslash}p{0.10\\textwidth} >{\\raggedleft\\arraybackslash}p{0.10\\textwidth} >{\\raggedleft\\arraybackslash}p{0.10\\textwidth} >{\\raggedleft\\arraybackslash}p{0.10\\textwidth} >{\\raggedleft\\arraybackslash}p{0.10\\textwidth} >{\\raggedleft\\arraybackslash}p{0.10\\textwidth}@{}}"
t4_header <- c(" & \\multicolumn{3}{c}{Weak ties} & \\multicolumn{3}{c}{Strong ties} \\\\",
  "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
  "Item & Variety & Lib. & Con. & Variety & Lib. & Con. \\\\")
a2_spec <- "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}>{\\raggedright\\arraybackslash}p{0.36\\textwidth} l >{\\raggedleft\\arraybackslash}p{0.12\\textwidth} l >{\\raggedleft\\arraybackslash}p{0.12\\textwidth}@{}}"
a2_header <- c("Item & Weak factor & Loading & Strong factor & Loading \\\\", "\\midrule")

pct1 <- function(x) sub("\\.$", ".0", sprintf("%.1f", 100 * x))
phi3 <- function(m) sprintf("%.2f", m[lower.tri(m)])
ci_pair <- function(x) {
  a <- sub("^0", "", sprintf("%.3f", x[1])); b <- sub("^0", "", sprintf("%.3f", x[2]))
  paste0("[{.", a, ", .", b, "}]")
}

blocks <- list()
blocks[["Table 1"]] <- long_block(38, "Table 1. Varimax Loadings for the Activity Battery", lt_spec_5,
  "Item & Assigned target & Arts & Leisure & Residual \\\\", write_rows(t1), paste0(
  "Note. Varimax-rotated component loadings from a principal components analysis (N = ",
  format(N_act, big.mark = ",", trim = TRUE), "). Bold entries mark each item's assigned factor. ",
  "The assigned target column reports the factor on which each item is assigned in the ESEM targets. ",
  "The three components together explain ", pct1(num$ve_activities), " percent of the variance."))

blocks[["Table 2"]] <- long_block(26,
  "Table 2. Confirmatory and Exploratory Structural Models of the Activity Battery", lt_spec_7,
  "Model & df & Scaled $\\chi^2$ & CFI & RMSEA & SRMR & Scaled $\\Delta\\chi^2$ (df) vs.~three-factor CFA \\\\",
  vapply(t2, function(r) paste(r[[1]], "&", sprintf("%d", as.integer(r[[2]]["df"])), "&",
    sprintf("%.1f", r[[2]]["chisq.scaled"]), "&", sprintf("%.3f", r[[2]]["cfi.scaled"]), "&",
    sprintf("%.3f", r[[2]]["rmsea.scaled"]), "&", sprintf("%.3f", r[[2]]["srmr"]), "&", r[[3]], "\\\\"),
    character(1)), paste0(
  "Note. N = ", format(N_act, big.mark = ",", trim = TRUE), ". WLSMV estimation. ",
  "The three-factor CFA is nested within the ESEM's implied covariance class as the case with zero cross-loadings, ",
  "so the ESEM row reports a scaled difference test against it. The ESEM degrees of freedom reflect its rotation constraints. ",
  "Latent factor correlations in the three-factor CFA are ", phi3(num$cor_cfa3)[1], " (arts and leisure), ",
  phi3(num$cor_cfa3)[2], " (arts and residual), and ", phi3(num$cor_cfa3)[3], " (leisure and residual). ",
  "RMSEA 90\\% confidence intervals are ", ci_pair(num$rmsea_ci$one), " (one-factor), ",
  ci_pair(num$rmsea_ci$two_arts), " (residual into arts), ", ci_pair(num$rmsea_ci$two_leis),
  " (residual into leisure), ", ci_pair(num$rmsea_ci$three), " (three-factor), and ",
  ci_pair(num$rmsea_ci$esem), " (ESEM). Scaled difference tests use the Satorra-Bentler scaled statistic with per-model scaling factors."))

blocks[["Table 3"]] <- long_block(38, "Table 3. ESEM Loadings with Target Rotation", lt_spec_5,
  "Item & Target factor & Arts & Leisure & Residual \\\\", write_rows(t3), paste0(
  "Note. Standardized loadings from an ESEM with target rotation (WLSMV, N = ",
  format(N_act, big.mark = ",", trim = TRUE), "). Bold entries mark each item's assigned factor. ",
  "Factor correlations: arts and leisure ", phi3(num$cor_esem_act)[1], ", arts and residual ",
  phi3(num$cor_esem_act)[2], ", leisure and residual ", phi3(num$cor_esem_act)[3], ". ",
  "The final row reports the model-implied omega of each factor."))

blocks[["Table 4"]] <- star_block(30,
  "Table 4. Standardized Loadings from the Target-Rotated ESEM of the Weak-Tie and Strong-Tie Batteries",
  t4_spec, t4_header, write_rows(t4), paste0(
  "Note. Standardized loadings from exploratory structural equation models with target rotation ",
  "(WLSMV, polychoric; N = ", format(num$n_weak, big.mark = ",", trim = TRUE),
  " per battery). Bold entries are the primary loadings from the target, and all other loadings are ",
  "estimated rather than fixed at zero. Gender items are excluded. Factor correlations are ",
  phi3(num$cor_esem_weak)[1], " (variety and liberal), ", phi3(num$cor_esem_weak)[2],
  " (variety and conservative), and ", phi3(num$cor_esem_weak)[3],
  " (liberal and conservative) for weak ties, and ", phi3(num$cor_esem_strong)[1], ", ",
  phi3(num$cor_esem_strong)[2], ", and ", phi3(num$cor_esem_strong)[3],
  " in the same order for strong ties. Item labels are abbreviated from the variable names; ",
  "MENA is Middle Eastern or North African, and NHPI is Native Hawaiian or Pacific Islander. ",
  "Lib. and Con. denote liberal and conservative composition. ",
  "The final row reports the model-implied omega of each factor."))

blocks[["Table A1"]] <- long_block(38,
  "Table A1. Oblimin-Rotated Exploratory Factor Loadings for the Activity Battery", lt_spec_5,
  "Item & Assigned target & Arts & Leisure & Residual \\\\", write_rows(ta1), paste0(
  "Note. Oblimin-rotated pattern loadings from an exploratory factor analysis (WLS estimation, N = ",
  format(N_act, big.mark = ",", trim = TRUE), "). Bold entries mark each item's assigned factor. ",
  "The EFA fits the 20 items adequately ($\\chi^2(", num$efa$df, ") = ", sprintf("%.1f", num$efa$chi),
  ", $p < .001$, $\\text{TLI} = ", sub("^0", "", sprintf("%.3f", num$efa$tli)),
  ", $\\text{RMSEA} = ", sub("^0", "", sprintf("%.3f", num$efa$rmsea)), "$ ",
  ci_pair(c(num$efa$lo, num$efa$hi)), "). The assigned target column reports the varimax assignment ",
  "used as the ESEM targets. Factor correlations from the oblique solution are ", phi3(num$efa$phi)[1],
  " (arts and leisure), ", phi3(num$efa$phi)[2], " (arts and residual), and ", phi3(num$efa$phi)[3],
  " (leisure and residual)."))

blocks[["Table A2"]] <- star_block(26,
  "Table A2. Standardized Loadings and Antonym Residual Covariances from the Separate Weak-Tie and Strong-Tie Confirmatory Models",
  a2_spec, a2_header, c(a2_rows[seq_len(nrow(wl))], "\\midrule", tail(a2_rows, 3)), paste0(
  "Note. Standardized loadings from separate three-factor WLSMV confirmatory models for weak ties and ",
  "strong ties (N = ", format(num$n_ties, big.mark = ",", trim = TRUE),
  " each), with the baseline models for the loadings. The residual rows come from the antonym-freed ",
  "models, and each has a standard error of ", sub("^0", "", sprintf("%.2f", num$ant_se_max)),
  ". Gender items are excluded. Item labels are abbreviated from the variable names and should be ",
  "checked against the question wording."),
  midrule_before = character(0))

out <- unlist(lapply(names(blocks), function(k) c(paste0("% === ", k), blocks[[k]])))
writeLines(out, "cache/manuscript/measurement_blocks.tex")
cat("wrote cache/manuscript/measurement_blocks.tex (full blocks) and measurement_numbers.rds\n")
