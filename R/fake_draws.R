#' Simulate "fake" parameter draws for examples
#'
#' Simulate fake ---i.e., Monte Carlo, \emph{not} Markov Chain Monte Carlo---
#' draws to use in examples and tests.
#'
#' @export
#' @param n Number of draws for each parameter (per chain, if \code{chains >
#'   1}).
#' @param chains Number of chains.
#' @param params Number of parameters.
#'
#' @details
#' If \code{params = 1} then the only simulated parameter is
#' \describe{
#'    \item{}{\code{alpha = rnorm(n, -1, 1)}}
#' }
#' If \code{params > 1} then
#' \describe{
#'  \item{}{\code{sigma = rlnorm(n, 0, 1/2)}}
#' }
#' is also included. And if \code{params > 2} then a parameter
#' \describe{
#'  \item{}{\code{beta[k] = rnorm(n, runif(1, -1, 1), 1/2)}}
#' }
#' for each \code{k} in \code{1:(params - 2)} is also included.
#'
#' @return If \code{chains = 1}, an \code{iter} by \code{params} matrix. If
#'   \code{chains > 1}, an \code{iter} by \code{chains} by \code{params} array.
#'
#' @examples
#' x <- fake_draws()
#' dim(x)
#' dimnames(x)
#'
#' x <- fake_draws(100, 1, 2)
#' dim(x)
#' colnames(x)
#'
#' x <- fake_draws(params = 6)
#' dimnames(x)[[3]]
#'
fake_draws <- function(n = 500, chains = 4, params = 4) {
  stopifnot(chains >= 1, n > chains, params >= 1)
  x <- .fake_draws(n * chains, params)
  if (chains == 1)
    return(x)

  array(
    x,
    dim = c(n, chains, params),
    dimnames = list(
      Iteration = NULL,
      Chain = paste0("chain:", 1:chains),
      Parameter = colnames(x)
    )
  )
}


# internal ----------------------------------------------------------------
.fake_draws <- function(n, params) {
  stopifnot(params > 0)

  x <- cbind(alpha = rnorm(n, -1, 1))
  if (params == 1)
    return(x)

  x <- cbind(x, sigma = rlnorm(n, 0, 1/2))
  if (params == 2)
    return(x)

  beta_names <- paste0("beta[", seq_len(params - 2), "]")
  for (j in seq_along(beta_names)) {
    x <- cbind(x, rnorm(n, runif(1, -1, 1), 1/2))
  }
  colnames(x)[3:params] <- beta_names
  return(x)
}

