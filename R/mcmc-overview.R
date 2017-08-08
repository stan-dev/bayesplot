#' Plots for Markov chain Monte Carlo simulations
#'
#' @name MCMC-overview
#' @aliases MCMC
#' @family MCMC
#'
#' @description
#' The \pkg{bayesplot} MCMC module provides various plotting functions for
#' creating graphical displays of Markov chain Monte Carlo (MCMC) simulations.
#' The \strong{MCMC plotting functions} section, below, provides links to the
#' documentation for various categories of MCMC plots. Currently the MCMC
#' plotting functions accept posterior draws provided in one of the following
#' formats:
#' \itemize{
#'  \item \strong{3-D array}: An \code{\link{array}} with dimensions
#'  \code{[Iteration, Chain, Parameter]} in that order.
#'  \item \strong{list}: A \code{list} of matrices, where each matrix
#'  corresponds to a Markov chain. All of the matrices should have the same
#'  number of iterations (rows) and parameters (columns), and parameters should
#'  have the same names and be in the same order.
#'  \item \strong{matrix}: A \code{\link{matrix}} with one column per parameter.
#'  If using matrix there should only be a single Markov chain or all chains
#'  should already be merged (stacked).
#'  \item \strong{data frame}: There are two types of \link[=data.frame]{data
#'  frames} allowed. Either a data frame with one column per parameter (if only
#'  a single chain or all chains have already been merged), or a data frame with
#'  one column per parameter plus an additional column \code{"Chain"} that
#'  contains the chain number (an integer) corresponding to each row in
#'  the data frame.
#' }
#' \strong{Note}: typically the user should \emph{not} include warmup iterations
#' in the object passed to \pkg{bayesplot} plotting functions, although for
#' certain plots (e.g. traceplots) it can occasionally be useful to include the
#' warmup iterations for diagnostic purposes.
#'
#' @section MCMC plotting functions:
#'
#' \describe{
#'   \item{\strong{\link[=MCMC-distributions]{Posterior distributions}}}{
#'     Histograms and kernel density plots of parameter draws, optionally
#'     showing each Markov chain separately.
#'   }
#'   \item{\strong{\link[=MCMC-intervals]{Uncertainty intervals}}}{
#'     Uncertainty intervals computed from parameter draws.
#'   }
#'   \item{\strong{\link[=MCMC-traces]{Traceplots}}}{
#'     Times series of parameter draws.
#'   }
#'   \item{\strong{\link[=MCMC-scatterplots]{Scatterplots}}}{
#'     Scatterplots, heatmaps, and pairs plots of parameter draws.
#'   }
#'   \item{\strong{\link[=MCMC-combos]{Combinations}}}{
#'     Combination plots (e.g. traceplot + histogram).
#'   }
#'   \item{\strong{\link[=MCMC-diagnostics]{General MCMC diagnostics}}}{
#'     MCMC diagnostic plots including Rhat, effective sample size,
#'     autocorrelation.
#'   }
#'   \item{\strong{\link[=MCMC-nuts]{NUTS diagnostics}}}{
#'     Special diagnostic plots for the No-U-Turn Sampler.
#'   }
#'   \item{\strong{\link[=MCMC-recover]{Comparisons to "true" values}}}{
#'     Plots comparing MCMC estimates to "true" parameter values (e.g.,
#'     values used to simulate data).
#'   }
#' }
#'
NULL
