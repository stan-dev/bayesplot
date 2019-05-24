expect_gg <- function(x) {
  testthat::expect_s3_class(x, "ggplot")
  invisible(ggplot_build(x))
}
expect_gtable <- function(x) testthat::expect_s3_class(x, "gtable")
expect_mcmc_array <- function(x) testthat::expect_true(is_mcmc_array(x))
expect_bayesplot_grid <- function(x) testthat::expect_true(is_bayesplot_grid(x))


#' Insert fake divergences for testing purposes
#'
#' @noRd
#' @param np Data frame returned by `nuts_params()`
#' @return `np` with every other iter marked as a divergence
ensure_divergences <- function(np) {
  divs <- rep_len(c(0,1), length.out = sum(np$Parameter=="divergent__"))
  np$Value[np$Parameter=="divergent__"] <- divs
  return(np)
}
