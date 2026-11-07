# Shared polychoric helpers (project policy, November 2026).
#
# All ordinal survey items enter the measurement models through a polychoric
# correlation matrix. Regression-method scores are computed on the
# standardized items (complete cases define centering and scaling), so rows
# with any missing item receive NA scores. Pearson correlations are not used
# for any item-level or factor-level result.

#' Varimax (or other) PCA on the polychoric matrix of ordinal items.
#' Returns a principal() object with $scores, $poly_R (the polychoric
#' matrix), and $n_complete.
poly_pca <- function(X, nfactors = 3, rotate = "varimax") {
  X <- as.data.frame(lapply(X, function(x) as.numeric(unclass(x))))
  cc <- complete.cases(X)
  R <- psych::polychoric(X[cc, ])$rho
  fa <- psych::principal(R, nfactors = nfactors, rotate = rotate, n.obs = sum(cc))
  center <- colMeans(X[cc, ])
  scl <- apply(X[cc, ], 2, sd)
  W <- solve(R, unclass(fa$loadings))
  fa$scores <- scale(X, center = center, scale = scl) %*% W
  fa$poly_R <- R
  fa$n_complete <- sum(cc)
  fa
}

#' Polyserial correlation (maximum likelihood, polycor) between a continuous
#' score and an ordinal or count variable, on complete pairs. Counts are
#' treated as ordinal categories. psych::polyserial() fails on these inputs.
poly_cor_count <- function(score, count) {
  ok <- stats::complete.cases(score, count)
  polycor::polyserial(score[ok], count[ok], ML = TRUE)
}
