#' Builds Tables A4 and A5 (OLS arts and solitary leisure) from cache/02_models.rds.
#' Each term has a coefficient row and a standard-error row. Writes
#' cache/manuscript/ols_appendix.tex, which Scripts/manuscript/splice_ols_appendix.py inserts.
#' Run from the project root: Rscript Scripts/manuscript/build_ols_appendix.R

m <- readRDS("cache/02_models.rds")

rowspec <- list(
  c("weak_variety", "Weak-tie variety"), c("strong_variety", "Strong-tie variety"),
  c("weak_lib_comp", "Weak-tie liberal composition"), c("weak_cons_comp", "Weak-tie conservative composition"),
  c("strong_lib_comp", "Strong-tie liberal composition"), c("strong_cons_comp", "Strong-tie conservative composition"),
  c("gender_fMan", "Man (ref. woman)"), c("gender_fNon-binary/Trans", "Non-binary/trans (ref. woman)"),
  c("educ_catSome college", "Some college (ref. high school)"), c("educ_catCollege degree", "College degree (ref. high school)"),
  c("educ_catGraduate degree", "Graduate degree (ref. high school)"), c("child_arts", "Childhood arts exposure"),
  c("income", "Household income"), c("age2", "Age (years)"), c("race_fblack", "Black (ref. White)"),
  c("race_flatine", "Hispanic (ref. White)"), c("race_fAAPI", "AAPI (ref. White)"),
  c("race_fmixed/other", "Mixed/other (ref. White)"), c("poli", "Political ideology (1 to 7)"),
  c("(Intercept)", "Intercept"))

stars <- function(p) ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "")))
num <- function(x) ifelse(x < 0, paste0("$-$", sprintf("%.2f", abs(x))), sprintf("\\hphantom{$-$}%.2f", x))

cell_coef <- function(fit, term) {
  if (!(term %in% names(fit$coefficients))) return("")
  paste0(num(fit$coefficients[[term]]), stars(fit$p.value[[term]]))
}
cell_se <- function(fit, term) {
  if (!(term %in% names(fit$coefficients))) return("")
  sprintf("(%.2f)", fit$std.error[[term]])
}

tex_table <- function(mods, label, caption) {
  note <- paste0("Note. OLS coefficients with heteroskedasticity-consistent (HC1) robust standard errors in parentheses (N = 1,227). ",
    "Outcomes are factor scores computed by the regression method, which weights each standardized item by its loading on the polychoric varimax components. ",
    "Models add covariates (Model 2, 4) and composition terms (Model 3, 4). Education is relative to high school or less. ",
    "* p < .05, ** p < .01, *** p < .001.")
  out <- c(sprintf("%s. %s", label, caption), "",
    "{\\renewcommand{\\arraystretch}{0.8}", "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}>{\\raggedright\\arraybackslash}p{0.42\\textwidth} >{\\raggedleft\\arraybackslash}p{0.13\\textwidth} >{\\raggedleft\\arraybackslash}p{0.13\\textwidth} >{\\raggedleft\\arraybackslash}p{0.13\\textwidth} >{\\raggedleft\\arraybackslash}p{0.13\\textwidth}@{}}",
    "\\toprule", " & Model 1 & Model 2 & Model 3 & Model 4 \\\\", "\\midrule")
  for (r in rowspec) {
    out <- c(out,
      sprintf("%s & %s \\\\", r[2], paste(sapply(mods, cell_coef, term = r[1]), collapse = " & ")),
      sprintf(" & %s \\\\", paste(sapply(mods, cell_se, term = r[1]), collapse = " & ")))
  }
  out <- c(out, "\\midrule",
    sprintf("Observations & %s \\\\", paste(sapply(mods, function(f) format(f$nobs, big.mark = ",")), collapse = " & ")),
    sprintf("$R^2$ & %s \\\\", paste(sapply(mods, function(f) sprintf("%.3f", f$r.squared)), collapse = " & ")),
    "\\bottomrule", "\\end{tabular*}", "}", "", paste0("{\\small ", note, "}"), "")
  out
}

frag <- c("\\Needspace{52\\baselineskip}",
  tex_table(m$arts_models, "Table A4", "Ordinary Least Squares Estimates of Public Arts Participation"),
  "\\Needspace{52\\baselineskip}",
  tex_table(m$leisure_models, "Table A5", "Ordinary Least Squares Estimates of Solitary Leisure"))
writeLines(frag, "cache/manuscript/ols_appendix.tex")
cat("wrote cache/manuscript/ols_appendix.tex\n")
