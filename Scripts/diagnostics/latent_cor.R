suppressPackageStartupMessages(library(lavaan))
fits <- readRDS("/home/omarlizardo/projects/CULTURE/childress-lizardo-culture-network-variety/cache/07_reliability_cfa.rds")$fits
for (nm in c("activities", "weak", "strong")) {
  f <- fits[[nm]]$fit
  pe <- parameterEstimates(f, standardized = TRUE)
  cr <- pe[pe$op == "~~" & pe$lhs != pe$rhs & pe$lhs %in% lavNames(f, "lv") & pe$rhs %in% lavNames(f, "lv"), c("lhs", "rhs", "est", "std.all", "se")]
  cat("\n==", nm, "latent correlations ==\n")
  print(cr, row.names = FALSE, digits = 3)
}
