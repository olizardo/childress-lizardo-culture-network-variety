# Unit tests for code_politics(). Run from the project root: Rscript tests/test_politics_coding.R
source("Scripts/diagnostics/politics_coding.R")

x <- c(1, 1.5, 3.5, 3.5001, 4, 4.5, 4.5001, 7, NA)

# linear leaves values unchanged, including NA
stopifnot(identical(code_politics(x, "linear"), x))

# three-level: cuts at midpoint +/- 0.5 (right-closed), so 3.5 is liberal and 4.5 is centrist
three <- as.character(code_politics(x, "three_level"))
stopifnot(identical(three, c("liberal", "liberal", "liberal", "centrist", "centrist",
                              "centrist", "conservative", "conservative", NA)))
stopifnot(identical(levels(code_politics(x, "three_level")), c("liberal", "centrist", "conservative")))

# seven-level: .5 rounds up, output is a factor with levels 1..7
seven <- code_politics(x, "seven_level")
stopifnot(is.factor(seven), identical(levels(seven), as.character(1:7)))
stopifnot(identical(as.character(seven), c("1", "2", "4", "4", "4", "5", "5", "7", NA)))

# cut points follow the scale bounds, not a fixed 1-7 assumption
three_wide <- as.character(code_politics(c(1, 5, 9), "three_level", scale_min = 1, scale_max = 9))
stopifnot(identical(three_wide, c("liberal", "centrist", "conservative")))

# invalid scale bounds are rejected
stopifnot(inherits(try(code_politics(x, "linear", scale_min = 7, scale_max = 1), silent = TRUE), "try-error"))

cat("All politics coding tests passed\n")
