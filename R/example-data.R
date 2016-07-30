#' Example draws to use in demonstrations and tests
#'
#' These functions return various objects containing data used in the examples
#' throughout the \pkg{bayesplot} package documentation.
#'
#' @name example-data
#' @keywords internal
#'
#' @return See \strong{Details}.
#'
#' @details
#' Each of these functions returns an object containing data, parameter draws, or
#' predictions corresponding to a basic linear regression model with data
#' \code{y} (outcome vector) and \code{X} (predictor matrix), and parameters
#' \code{alpha} (intercept), \code{beta} (coefficient vector), and \code{sigma}
#' (error sd).
#'
#' \describe{
#' \item{\code{example_mcmc_draws}}{
#'  If \code{chains > 1}, a \code{250} (iterations) by \code{chains} by
#'  \code{params} array or, if \code{chains = 1}, a \code{250} by \code{params}
#'  matrix of MCMC draws from the posterior distribution of the parameters in
#'  the linear regression model described above. If \code{params = 1} then only
#'  the draws for \code{alpha} are included in the returned object. If
#'  \code{params >= 2} then draws for \code{sigma} are also included. And if
#'  \code{params} is between \code{3} and the maximum of \code{6} then draws
#'  for regression coefficients \code{beta[k]} (\code{k} in \code{1:(params-2)})
#'  are also included.
#' }
#' \item{\code{example_y_data}}{
#'  A numeric vector with \code{434} observations of the outcome variable in the
#'  linear regression model.
#' }
#' \item{\code{example_x_data}}{
#'  A numeric vector with \code{434} observations of one of the predictor
#'  variables in the linear regression model.
#' }
#' \item{\code{example_group_data}}{
#'  A factor variable with \code{434} observations of a grouping variable with
#'  two levels.
#' }
#' \item{\code{example_yrep_draws}}{
#'  A \code{1000} (draws) by \code{434} (data points) matrix of draws from the
#'  posterior predictive distribution. Each row represents a full dataset drawn
#'  from the posterior predictive distribution of the outcome \code{y} after
#'  fitting the linear regression model mentioned above.
#' }
#' }
#'
#'
NULL

#' @rdname example-data
#' @export
#' @param chains An interger between 1 and 4 indicating the desired number of
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
  .example_mcmc_draws(chains, params)
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
  .example_yrep_draws()
}

#' @rdname example-data
#' @export
example_y_data <- function() {
  .example_y_data()
}

#' @rdname example-data
#' @export
example_x_data <- function() {
  .example_x_data()
}

#' @rdname example-data
#' @export
example_group_data <- function() {
  .example_group_data()
}


# internal ----------------------------------------------------------------
.example_y_data <- function() {
  # ex_y is stored internally in R/sysdata.rda
  return(ex_y)
}
.example_x_data <- function() {
  # ex_x is stored internally in R/sysdata.rda
  return(ex_x)
}
.example_group_data <- function() {
  # ex_group is stored internally in R/sysdata.rda
  return(ex_group)
}
.example_yrep_draws <- function() {
  # ex_yrep is stored internally in R/sysdata.rda
  return(ex_yrep)
}
.example_mcmc_draws <- function(chains, params) {
  # ex_draws is stored internally in R/sysdata.rda
  x <- ex_draws[, seq_len(chains), ]
  if (chains > 1)
    x[, , seq_len(params), drop = FALSE]
  else
    x[, seq_len(params), drop = FALSE]
}
