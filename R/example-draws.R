#' Example MCMC draws to use in examples and tests
#'
#' Example MCMC draws to use in examples and tests. The draws are from the
#' posterior distribution of the parameters of a linear regression model.
#'
#' @export
#' @param chains An interger between 1 and 4 indicating the desired number of
#'   chains.
#' @param params An integer between 1 and 6 indicating the desired number of
#'   parameters.
#'
#' @details If \code{params = 1} then only the draws for the intercept
#'   \code{alpha} are included in the returned object. If \code{params >= 2},
#'   then draws for \code{sigma}, the residual standard deviation, are also
#'   included. If \code{params} is between \code{3} and the maximum of \code{6},
#'   then draws for regression coefficients \code{beta[k]} (\code{k} in
#'   \code{1:(params-2)}) are also included.
#'
#' @return A \code{500} (iterations) by \code{chains} by \code{params} array.
#'
#' @examples
#' x <- example_mcmc_draws()
#' dim(x)
#' dimnames(x)
#'
#' x <- example_mcmc_draws(1, 2)
#' dim(x)
#' colnames(x)
#'
#' x <- example_mcmc_draws(params = 6)
#' dimnames(x)[[3]]
#'
example_mcmc_draws <- function(chains = 4, params = 4) {
  stopifnot(
    chains >= 1 && chains <= 4,
    params >= 1 && params <= 6
  )
  .example_mcmc_draws(chains, params)
}

# internal ----------------------------------------------------------------
.example_mcmc_draws <- function(chains, params) {
  # exdraws is stored internally in R/sysdata.rda
  exdraws[ , seq_len(chains), seq_len(params), drop = FALSE]
}

