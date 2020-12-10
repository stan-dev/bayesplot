library(testthat)
library(bayesplot)

Sys.unsetenv("R_TESTS")
test_check("bayesplot")

# Alternative interactive tester that doesn't bail after 24 failures
# pr <- testthat::ProgressReporter$new()
# pr$max_fail = 1000
# devtools::test(reporter = pr)

# pass check.environment=FALSE to all.equal to avoid issue due to
# changing behavior of all.equal:
# https://stat.ethz.ch/pipermail/r-devel/2020-December/080172.html
expect_equal <- function(...) {
  testthat::expect_equal(..., check.environment=FALSE)
}


