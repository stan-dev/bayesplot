expect_gg <- function(x) {
  testthat::expect_s3_class(x, "ggplot")
  invisible(ggplot_build(x))
}
expect_gtable <- function(x) testthat::expect_s3_class(x, "gtable")
expect_mcmc_array <- function(x) testthat::expect_true(is_mcmc_array(x))
expect_bayesplot_grid <- function(x) testthat::expect_s3_class(x, "bayesplot_grid")
