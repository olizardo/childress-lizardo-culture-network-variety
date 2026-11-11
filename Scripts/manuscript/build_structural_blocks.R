#' Builds the structural blocks of results_draft.tex from the saved fixed-loading fits:
#' Tables 5 to 10 (Table 8 combines weak and strong activity predictors), Figures 5 to 10
#' (TikZ path diagrams, arts outcome only, paths with p < .05), and Table A3 (split-half and
#' rotation paths). Writes cache/manuscript/structural_blocks.tex and
#' cache/manuscript/structural_numbers.rds.
#'
#' Sources (all written by the scripts named in the comments):
#'   cache/diagnostics/sem_gpa_fixed_sep_covars.rds        Scripts/diagnostics/sem_gpa_fixed_sep_covars.R
#'   cache/diagnostics/sem_gpa_fixed_sep_decomposition.rds Scripts/diagnostics/sem_gpa_fixed_sep_decomposition.R
#'   cache/diagnostics/sem_gpa_fixed_splithalf.rds         Scripts/diagnostics/sem_gpa_fixed_splithalf.R
#'
#' Run from the project root: Rscript Scripts/manuscript/build_structural_blocks.R

suppressPackageStartupMessages(library(lavaan))

sc <- readRDS("cache/diagnostics/sem_gpa_fixed_sep_covars.rds")
dc <- readRDS("cache/diagnostics/sem_gpa_fixed_sep_decomposition.rds")
sh <- readRDS("cache/diagnostics/sem_gpa_fixed_splithalf.rds")

star <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
cell <- function(b, p) {
  r <- round(b, 2)
  # Sign slot via \hphantom keeps digit widths equal; stars hang right (\rlap, zero
  # width) so starred and unstarred cells align on the decimal point. Do not use
  # \llap here: in raggedleft p-columns it forces a line break before the number.
  sg <- ifelse(r < 0, "$-$", "\\hphantom{$-$}")
  st <- star(p)
  st <- if (nzchar(st)) paste0("\\rlap{", st, "}") else ""
  paste0(sg, sprintf("%.2f", abs(r)), st)
}
pe_of <- function(fit) parameterEstimates(fit, standardized = TRUE)
pick <- function(pe, op, lhs, rhs) {
  r <- pe[pe$op == op & pe$lhs == lhs & pe$rhs == rhs, ]
  if (nrow(r) == 0) return(NULL)
  list(b = r$std.all[1], p = r$pvalue[1])
}
cell_or_dash <- function(r) if (is.null(r)) "--" else cell(r$b, r$p)

act <- c("A", "Lis", "R")
act_names <- c(A = "Arts", Lis = "Leisure", R = "Residual")
demo <- c(age2 = "Age (years)", educ_ord = "Education (1 to 4)", child_arts = "Childhood arts",
  income = "Household income", poli = "Political ideology (1 to 7)", gender_fMan = "Man (ref. woman)",
  gender_fNon.binary.Trans = "Non-binary/trans", race_fblack = "Black (ref. White)",
  race_flatine = "Hispanic (ref. White)", race_fAAPI = "AAPI (ref. White)",
  race_fmixed.other = "Mixed/other (ref. White)")
net_names <- list(weak = c(Wv = "Variety", Wl = "Liberal", Wc = "Conservative"),
  strong = c(Sv = "Variety", Sl = "Liberal", Sc = "Conservative"))

pes <- lapply(sc, function(x) pe_of(x$fit))
rows <- list()

# Table 5: network-to-activity paths, both ties
t5 <- list()
for (tie in c("weak", "strong")) {
  nf <- names(net_names[[tie]])
  for (k in nf) {
    t5[[length(t5) + 1]] <- c(paste(tools::toTitleCase(tie), tolower(net_names[[tie]][[k]])),
      sapply(act, function(a) cell_or_dash(pick(pes[[tie]], "~", a, k))))
  }
}
rows$t5 <- t5

# Tables 6 and 7: demographic predictors of network factors
for (tie in c("weak", "strong")) {
  nf <- names(net_names[[tie]])
  rows[[paste0("t", if (tie == "weak") 6 else 7)]] <- lapply(names(demo), function(v)
    c(demo[[v]], sapply(nf, function(k) cell_or_dash(pick(pes[[tie]], "~", k, v)))))
}

# Table 8: demographic predictors of activity factors, weak and strong side by side
rows$t8 <- lapply(names(demo), function(v)
  c(demo[[v]],
    sapply(c("weak", "strong"), function(tie) sapply(act, function(a) cell_or_dash(pick(pes[[tie]], "~", a, v)))) |>
      as.vector()))

# Tables 9 and 10: direct, network-mediated, and total effects
dec_rows <- function(tie) {
  pe <- pe_of(dc[[tie]]$fit)
  pe <- pe[pe$op == ":=" & !grepl("^ip_", pe$lhs), ]
  preds <- c(educ = "education", arts = "childhood arts", poli = "political ideology")
  out <- list()
  for (a in act) for (p in names(preds)) {
    get <- function(pre) {
      r <- pe[pe$lhs == paste0(pre, "_", a, "_", p), ]
      if (nrow(r) == 0) "--" else cell(r$std.all[1], r$pvalue[1])
    }
    out[[length(out) + 1]] <- c(paste0(act_names[[a]], ": ", preds[[p]]), get("dir"), get("ind"), get("tot"))
  }
  out
}
rows$t9 <- dec_rows("weak")
rows$t10 <- dec_rows("strong")

# Table A3: split-half and rotation paths
get_sh <- function(method, half, tie, lhs, rhs) {
  key <- paste(method, half, tie, sep = "_")
  r <- sh[[key]]$paths
  x <- r[r$lhs == lhs & r$rhs == rhs, "std.all"]
  if (length(x)) x else NA
}
get_sh_p <- function(method, tie, lhs, rhs) {
  r <- sh[[paste(method, "full", tie, sep = "_")]]$paths
  x <- r[r$lhs == lhs & r$rhs == rhs, "pvalue"]
  if (length(x)) x else NA_real_
}
stars_for <- function(p) {
  if (is.na(p)) "" else if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else ""
}
a3 <- list()
for (tie in c("weak", "strong")) for (lhs in act) for (rhs in names(net_names[[tie]])) {
  v <- c(get_sh("target", "full", tie, lhs, rhs), get_sh("target", "A", tie, lhs, rhs), get_sh("target", "B", tie, lhs, rhs),
    get_sh("oblimin", "full", tie, lhs, rhs), get_sh("oblimin", "A", tie, lhs, rhs), get_sh("oblimin", "B", tie, lhs, rhs))
  p_full <- c(get_sh_p("target", tie, lhs, rhs), get_sh_p("oblimin", tie, lhs, rhs))
  lbl <- paste0(tools::toTitleCase(tie), " ", tolower(net_names[[tie]][[rhs]]), " $\\to$ ", tolower(act_names[[lhs]]))
  cells <- sapply(seq_along(v), function(j) {
    r <- round(v[j], 2)
    sg <- ifelse(r < 0, "$-$", "\\hphantom{$-$}")
    st <- if (j == 1) stars_for(p_full[1]) else if (j == 4) stars_for(p_full[2]) else ""
    st <- if (nzchar(st)) paste0("\\rlap{", st, "}") else ""
    sprintf("%s%.2f%s", sg, abs(r), st)
  })
  a3[[length(a3) + 1]] <- c(lbl, cells)
}
rows$a3 <- a3

# ---------------------------------------------------------------- Figures 5 to 10 (TikZ, arts outcome)
fig_tikz <- function(tie, pred, num) {
  pe <- pes[[tie]]
  nf <- names(net_names[[tie]])
  pname <- c(educ_ord = "Education", child_arts = "Childhood Arts Exposure", poli = "Political Conservatism")[[pred]]
  ys <- c(2.4, 0, -2.4); ids <- c("wv", "wl", "wc")
  nlab <- net_names[[tie]]
  s <- c(sprintf("\\Needspace{18\\baselineskip}"), sprintf("Figure \\fignum{%s}. Paths from %s to Arts Participation, %s Ties", paste(c(educ_ord = "educ", child_arts = "childarts", poli = "poli")[[pred]], tie, sep = "-"), pname, tools::toTitleCase(tie)), "",
    "\\begin{center}", "\\begin{tikzpicture}[>=Stealth, font=\\footnotesize, scale=0.8, transform shape,",
    "  box/.style={draw, rounded corners, align=center, minimum width=4.4cm, minimum height=0.8cm},",
    "  lat/.style={draw, ellipse, align=center, minimum width=3.4cm, minimum height=0.9cm},",
    "  lab/.style={fill=white, inner sep=0.5pt, font=\\footnotesize}]",
    sprintf("\\node[box] (x) at (0,0) {%s};", pname))
  for (i in seq_along(nf)) s <- c(s, sprintf("\\node[lat] (%s) at (7.4,%.1f) {%s};", ids[i], ys[i],
    if (nlab[[i]] == "Variety") "Network variety" else paste(nlab[[i]], "composition")))
  s <- c(s, "\\node[lat] (act) at (13.2,0) {Arts participation};")
  for (i in seq_along(nf)) {
    r <- pick(pe, "~", nf[i], pred)
    if (!is.null(r) && r$p < .05) s <- c(s, sprintf("\\draw[->] (x.east) -- node[lab, pos=0.5] {%s} (%s.west);", cell(r$b, r$p), ids[i]))
  }
  for (i in seq_along(nf)) {
    r <- pick(pe, "~", "A", nf[i])
    if (!is.null(r) && r$p < .05) s <- c(s, sprintf("\\draw[->] (%s.east) -- node[lab, pos=0.5, above] {%s} (act.west);", ids[i], cell(r$b, r$p)))
  }
  d <- pick(pe, "~", "A", pred)
  if (!is.null(d) && d$p < .05) s <- c(s, sprintf("\\draw[->, dashed] (x.north) .. controls (3,5.5) and (10,5.5) .. node[lab, pos=0.5, above] {%s} (act.north);", cell(d$b, d$p)))
  c(s, "\\end{tikzpicture}", "\\end{center}", "",
    sprintf("Note. Standardized coefficients from the %s-tie structural model with loadings fixed at the target rotation (N = 1,227). Solid arrows are paths through the network factors, dashed arrows are direct paths, and only paths significant at $p < .05$ are drawn. Asterisks follow Table~\\ref{tab:network-activity}.", tie), "")
}
figs <- list()
n <- 5
for (tie in c("weak", "strong")) for (pred in c("educ_ord", "child_arts", "poli")) {
  figs[[length(figs) + 1]] <- fig_tikz(tie, pred, n); n <- n + 1
}
rows$figs <- figs

saveRDS(list(rows = rows), "cache/manuscript/structural_numbers.rds")

# ---------------------------------------------------------------- write LaTeX fragment
rowtex <- function(r) paste(paste(r, collapse = " & "), "\\\\")
tabular <- function(ncols, header, body, lead = 0.36) {
  spec <- paste0(">{\\raggedright\\arraybackslash}p{", sprintf("%.2f", lead), "\\textwidth} ",
    paste(rep(sprintf("@{\\hspace{1pt}}>{\\raggedleft\\arraybackslash}p{%.3f\\textwidth}", (0.94 - lead) / (ncols - 1)), ncols - 1), collapse = ""),
    "@{\\hspace{1.5em}}")
  c(sprintf("\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}%s@{}}", spec), "\\toprule", header, "\\midrule", body,
    "\\bottomrule", "\\end{tabular*}")
}
out <- c("% BEGIN structural tables", "")
out <- c(out, "\\Needspace{14\\baselineskip}", "Table \\tabnum{network-activity}. Standardized Network-to-Activity Paths from the Separate Weak-Tie and Strong-Tie Structural Models", "",
  tabular(4, "Network factor & Arts & Solitary leisure & Residual \\\\", vapply(rows$t5, rowtex, character(1))), "",
  "Note. Standardized coefficients from separate WLSMV structural models for weak ties and strong ties (N = 1,227 each), with loadings fixed at the target rotation of each battery. Each model includes gender, education, childhood arts exposure, income, age, race and ethnicity, and political ideology as predictors of all factors. * p < .05, ** p < .01, *** p < .001.", "")
for (tt in c(6, 7)) {
  tie <- if (tt == 6) "weak" else "strong"
  out <- c(out, "\\Needspace{20\\baselineskip}", sprintf("Table \\tabnum{%s}. Sociodemographic Predictors of %s-Tie Network Factors", paste0("network-predictors-", tie), tools::toTitleCase(tie)), "",
    tabular(4, "Predictor & Variety & Liberal & Conservative \\\\", vapply(rows[[paste0("t", tt)]], rowtex, character(1))), "",
    sprintf("Note. Standardized coefficients from the %s-tie structural model (N = 1,227). Reference categories are shown in parentheses, and woman is the gender reference category. Political ideology ranges from 1 (very liberal) to 7 (very conservative). * p < .05, ** p < .01, *** p < .001.", tie), "")
}
out <- c(out, "\\Needspace{20\\baselineskip}", "Table \\tabnum{activity-predictors}. Sociodemographic Predictors of Weak- and Strong-Tie Activity Factors", "",
  "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}>{\\raggedright\\arraybackslash}p{0.27\\textwidth}" ,
  paste0(paste(rep("@{\\hspace{1pt}}>{\\raggedleft\\arraybackslash}p{0.112\\textwidth}", 6), collapse = ""), "@{}}"),
  "\\toprule", " & \\multicolumn{3}{c}{Weak ties} & \\multicolumn{3}{c}{Strong ties} \\\\", "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
  "Predictor & Arts & Leisure & Residual & Arts & Leisure & Residual \\\\", "\\midrule",
  vapply(rows$t8, rowtex, character(1)), "\\bottomrule", "\\end{tabular*}", "",
  "Note. Standardized coefficients from the weak-tie and strong-tie structural models (N = 1,227 each). Reference categories are shown in parentheses, and woman is the gender reference category. Each activity equation also includes the three network factors of its tie type, whose coefficients are reported in Table~\\ref{tab:network-activity}. * p < .05, ** p < .01, *** p < .001.", "")
out <- c(out, "% BEGIN structural figures", "", unlist(figs), "% END structural figures", "")
for (tt in c(9, 10)) {
  tie <- if (tt == 9) "weak" else "strong"
  out <- c(out, "\\Needspace{18\\baselineskip}", sprintf("Table \\tabnum{%s}. Direct, Network-Mediated, and Total Standardized Effects of Education, Childhood Arts Exposure, and Political Ideology on the Activity Factors, %s Ties", paste0("effects-", tie), tools::toTitleCase(tie)), "",
    tabular(4, "Predictor and activity factor & Direct & Network-mediated & Total \\\\", vapply(rows[[paste0("t", tt)]], rowtex, character(1)), lead = 0.36), "",
    sprintf("Note. Standardized effects from the labeled %s-tie model. Network-mediated effects sum the products of each predictor's path to a network factor and that factor's path to the activity factor, with standard errors from the delta method, a first-order approximation that propagates the uncertainty of the component paths into each product. Total effects equal direct plus network-mediated effects. * p < .05, ** p < .01, *** p < .001.", tie), "")
}
out <- c(out, "% END structural tables", "",
  "% BEGIN table A3", "",
  "\\Needspace{30\\baselineskip}",
  "Table \\apptabnum{rotation-halves}. Standardized Network-to-Activity Paths in the Separate Structural Models Under Two Rotations and in Random Halves", "",
  tabular(7, " & \\multicolumn{3}{c}{Target rotation} & \\multicolumn{3}{c}{Oblimin rotation} \\\\", vapply(rows$a3, rowtex, character(1)), lead = 0.25), "",
  "Note. Standardized paths from separate fixed-loading WLSMV structural models (weak ties and strong ties, each with gender, education, childhood arts exposure, income, age, race and ethnicity, and political ideology as predictors). The full sample has N = 1,227; the random halves (seed 4217) have N = 600 (half A) and N = 627 (half B). The target-rotation columns match the main results in Table~\\ref{tab:network-activity}, and the oblimin columns re-estimate the loadings under an unconstrained oblimin rotation. Significance stars on the full-sample columns come from the WLSMV standard errors of the standardized paths (* p < .05, ** p < .01, *** p < .001); no stars are shown for the half-sample columns because the halves are not designed for inference, and standard errors are in the replication output.", "",
  "% END table A3")
writeLines(out, "cache/manuscript/structural_blocks.tex")
cat("wrote cache/manuscript/structural_blocks.tex\n")
