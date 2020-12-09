library(testthat)
library(bayesplot)

Sys.unsetenv("R_TESTS")
test_check("bayesplot")

# Alternative interactive tester that doesn't bail after 24 failures
# pr <- testthat::ProgressReporter$new()
# pr$max_fail = 1000
# devtools::test(reporter = pr)

expect_equivalent <- function(...) {
  testthat::expect_equivalent(...,
                              ignore_function_env = TRUE,
                              ignore_formula_env = TRUE)
}
expect_equal <- function(...) {
  testthat::expect_equal(...,
                         ignore_function_env = TRUE,
                         ignore_formula_env = TRUE)
}


