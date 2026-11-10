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

fmt <- function(x) ifelse(x < 0, paste0("$-$", sprintf("%.2f", abs(x))), sprintf("%.2f", x))
bold_if <- function(txt, b) ifelse(b, paste0("\\textbf{", txt, "}"), txt)

# ---------------------------------------------------------------- Table 1: varimax PCA loadings
lv <- unclass(prep$act_fa$loadings)[items, ]
colnames(lv) <- c("Arts", "Leisure", "Residual")
t1 <- sapply(items, function(it) {
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
  c(Td = unname(Td), df = m)
}

esem <- efa(data = act_full, nfactors = 3, ordered = items, estimator = "WLSMV",
  rotation = list("target", target = {
    tm <- matrix(0, nrow = length(items), ncol = 3, dimnames = list(items, c("arts", "leisure", "residual")))
    tm[arts_items, "arts"] <- 1; tm[leisure_items, "leisure"] <- 1; tm[resid_items, "residual"] <- 1; tm
  }))
esem_lam <- lavInspect(esem, "std")$lambda
esem_lam <- esem_lam[items, , drop = FALSE]

fit_row <- function(f) fitMeasures(f, c("chisq.scaled", "df", "cfi.scaled", "rmsea.scaled", "srmr"))
f1r <- fit_row(f1); f2ar <- fit_row(f2a); f2lr <- fit_row(f2l); f3r <- fit_row(f3)
er <- fitMeasures(esem, c("chisq.scaled", "df", "cfi.scaled", "rmsea.scaled", "srmr"))
t2 <- list(
  list("One-factor CFA", f1r, sprintf("%.1f (%d), p \\textless{} .001", sb(f3, f1)["Td"], as.integer(sb(f3, f1)["df"]))),
  list("Two-factor CFA, residual merged into arts", f2ar, sprintf("%.1f (%d), p \\textless{} .001", sb(f3, f2a)["Td"], as.integer(sb(f3, f2a)["df"]))),
  list("Two-factor CFA, residual merged into leisure", f2lr, sprintf("%.1f (%d), p \\textless{} .001", sb(f3, f2l)["Td"], as.integer(sb(f3, f2l)["df"]))),
  list("Three-factor CFA", f3r, "---"),
  list("ESEM, target rotation", er, sprintf("%.1f (%d), p \\textless{} .001", sb(esem, f3)["Td"], as.integer(sb(esem, f3)["df"]))))

# Table 3 loadings (ESEM), with bold on the assigned factor
t3 <- t(sapply(items, function(it) {
  a <- assign_of[[it]]
  vals <- sapply(c("arts", "leisure", "residual"), function(cn) {
    tgt <- c(arts = "Arts", leisure = "Leisure", residual = "Residual")[[cn]]
    bold_if(fmt(esem_lam[it, cn]), tgt == a)
  })
  c(act_labels[[it]], a, vals)
}))

# ---------------------------------------------------------------- Table A1: oblimin EFA
ef_obi <- fa(polychoric(act_full)$rho, nfactors = 3, rotate = "oblimin", fm = "wls", n.obs = N_act)
pl <- unclass(ef_obi$loadings)[items, ]
lab_of <- function(k) {
  sc <- sapply(c("Arts", "Leisure", "Residual"), function(a)
    sum(abs(pl[assign_of == a, k])))
  names(which.max(sc))
}
perm_names <- sapply(1:3, lab_of)
if (length(unique(perm_names)) < 3) stop("oblimin factor matching failed")
pl <- pl[, match(c("Arts", "Leisure", "Residual"), perm_names), drop = FALSE]
for (k in 1:3) pl[, k] <- pl[, k] * sign(sum(pl[assign_of == c("Arts", "Leisure", "Residual")[k], k]))
ob_phi <- ef_obi$Phi
colnames(pl) <- c("Arts", "Leisure", "Residual")
ta1 <- t(sapply(items, function(it) {
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
  st <- sapply(1:3, function(k) bold_if(fmt(bs$lam[it, k]), bs$tgt[it, k] == 1))
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
  list(loadings = parameterEstimates(f0, standardized = TRUE), antonym = rr, n = nrow(d))
}
a2_weak <- a2_fit("weak"); a2_strong <- a2_fit("strong")

# ---------------------------------------------------------------- write fragments and numbers
numbers <- list(table1 = lv, table2 = list(one = f1r, two_arts = f2ar, two_leis = f2lr, three = f3r, esem = er),
  table3 = esem_lam, tableA1 = pl, oblimin_phi = ob_phi, table4_weak = bw$lam, table4_strong = bs$lam,
  tableA2_weak = a2_weak, tableA2_strong = a2_strong, n_activities = N_act)
saveRDS(numbers, "cache/manuscript/measurement_numbers.rds")

write_rows <- function(M) apply(M, 1, function(r) paste(paste(r, collapse = " & "), "\\\\"))
writeLines(c("% Table 1", write_rows(t1), "% Table 2", vapply(t2, function(r) paste(r[[1]], "&",
  sprintf("%d", as.integer(r[[2]]["df"])), "&", sprintf("%.1f", r[[2]]["chisq.scaled"]), "&",
  sprintf("%.3f", r[[2]]["cfi.scaled"]), "&", sprintf("%.3f", r[[2]]["rmsea.scaled"]), "&",
  sprintf("%.3f", r[[2]]["srmr"]), "&", r[[3]], "\\\\"), character(1)),
  "% Table 3", write_rows(t3), "% Table 4", write_rows(t4), "% Table A1", write_rows(ta1)),
  "cache/manuscript/measurement_blocks.tex")
cat("wrote cache/manuscript/measurement_blocks.tex and measurement_numbers.rds\n")
