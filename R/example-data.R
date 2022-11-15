#' Example draws to use in demonstrations and tests
#'
#' These functions return various objects containing data used in the examples
#' throughout the **bayesplot** package documentation.
#'
#' @name example-data
#' @keywords internal
#'
#' @return See **Details**.
#'
#' @details
#' Each of these functions returns an object containing data, parameter draws, or
#' predictions corresponding to a basic linear regression model with data
#' `y` (outcome vector) and `X` (predictor matrix), and parameters
#' `alpha` (intercept), `beta` (coefficient vector), and `sigma`
#' (error sd).
#'
#' \describe{
#' \item{`example_mcmc_draws()`}{
#'  If `chains > 1`, a `250` (iterations) by `chains` by
#'  `params` array or, if `chains = 1`, a `250` by `params`
#'  matrix of MCMC draws from the posterior distribution of the parameters in
#'  the linear regression model described above. If `params = 1` then only
#'  the draws for `alpha` are included in the returned object. If
#'  `params >= 2` then draws for `sigma` are also included. And if
#'  `params` is between `3` and the maximum of `6` then draws
#'  for regression coefficients `beta[k]` (`k` in `1:(params-2)`)
#'  are also included.
#' }
#' \item{`example_y_data()`}{
#'  A numeric vector with `434` observations of the outcome variable in the
#'  linear regression model.
#' }
#' \item{`example_x_data()`}{
#'  A numeric vector with `434` observations of one of the predictor
#'  variables in the linear regression model.
#' }
#' \item{`example_group_data()`}{
#'  A factor variable with `434` observations of a grouping variable with
#'  two levels.
#' }
#' \item{`example_yrep_draws()`}{
#'  A `500` (draws) by `434` (data points) matrix of draws from the
#'  posterior predictive distribution. Each row represents a full dataset drawn
#'  from the posterior predictive distribution of the outcome `y` after
#'  fitting the linear regression model mentioned above.
#' }
#' }
#'
#'
NULL

#' @rdname example-data
#' @export
#' @param chains An integer between 1 and 4 indicating the desired number of
#'   chains.
#' @param params An integer between 1 and 6 indicating the desired number of
#'   parameters.
#'
#' @examples
#' draws <- example_mcmc_draws()
#' dim(draws)
#' dimnames(draws)
#'
#' draws <- example_mcmc_draws(1, 2)
#' dim(draws)
#' colnames(draws)
#'
#' draws <- example_mcmc_draws(params = 6)
#' dimnames(draws)[[3]]
#'
example_mcmc_draws <- function(chains = 4, params = 4) {
  stopifnot(
    chains >= 1 && chains <= 4,
    params >= 1 && params <= 6
  )
  x <- .ex_draws[, seq_len(chains), ]
  if (chains > 1) {
    x[, , seq_len(params), drop = FALSE]
  } else {
    x[, seq_len(params), drop = FALSE]
  }
}


#' @rdname example-data
#' @export
#' @examples
#' y <- example_y_data()
#' x <- example_x_data()
#' group <- example_group_data()
#' length(y)
#' length(x)
#' length(group)
#' tail(data.frame(y, x, group), 5)
#'
#' yrep <- example_yrep_draws()
#' dim(yrep) # ncol(yrep) = length(y) = length(x) = length(group)
#'
example_yrep_draws <- function() {
  .ex_yrep
}

#' @rdname example-data
#' @export
example_y_data <- function() {
  .ex_y
}

#' @rdname example-data
#' @export
example_x_data <- function() {
  .ex_x
}

#' @rdname example-data
#' @export
example_group_data <- function() {
  .ex_group
}
