#' @param y A vector of observed data. See Details.
#' @param yrep An \eqn{S} by \eqn{N} matrix of draws from the posterior
#'   predictive distribution, where \eqn{S} is the size of the posterior sample
#'   (or subset of the posterior sample used to generate \code{yrep}) and
#'   \eqn{N} is the number of observations (the length of \code{y}).
#'   See Details for additional instructions.