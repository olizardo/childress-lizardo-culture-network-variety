#' Recode the politics scale for regression and SEM models
#'
#' `poli` is the mean of two 1-7 items (economic and social issues), where
#' 1 = very liberal and 7 = very conservative, so values run from 1 to 7 in half-point steps.
#' Cut points are derived from the scale bounds, not hard-coded.
#'
#' @param x Numeric vector of politics scores.
#' @param scheme One of "linear" (unchanged), "three_level" (liberal / centrist /
#'   conservative, split half a scale point either side of the midpoint), or
#'   "seven_level" (rounded to whole scale points, with .5 rounded up).
#' @param scale_min Lowest scale value.
#' @param scale_max Highest scale value.
#' @return A numeric vector (linear) or a factor (categorical schemes), with NA preserved.
code_politics <- function(x, scheme = c("linear", "three_level", "seven_level"),
                          scale_min = 1, scale_max = 7) {
  scheme <- match.arg(scheme)
  stopifnot(is.numeric(x), scale_max > scale_min)
  midpoint <- (scale_min + scale_max) / 2
  switch(scheme,
    linear = x,
    three_level = factor(
      cut(x, breaks = c(-Inf, midpoint - 0.5, midpoint + 0.5, Inf),
          labels = c("liberal", "centrist", "conservative"), right = TRUE),
      levels = c("liberal", "centrist", "conservative")),
    seven_level = factor(
      pmin(pmax(floor(x + 0.5), scale_min), scale_max),
      levels = seq(scale_min, scale_max))
  )
}
