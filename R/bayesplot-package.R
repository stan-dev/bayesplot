#' Plotting for Bayesian Models
#'
#' @docType package
#' @name bayesplot-package
#' @aliases bayesplot
#'
#' @description
#' \if{html}{\figure{bayesplot.png}{options: width="50\%" alt="Figure: bayesplot.png"}}
#' \if{latex}{\figure{bayesplot.pdf}{options: width=5in}}
#'
#' @section Plotting functionality:
#' \itemize{
#'   \item \strong{\link[=MCMC-overview]{MCMC (general)}}:
#'   Visualizations of Markov chain Monte Carlo (MCMC) simulations generated
#'   by any MCMC algorithm.
#'   \item \strong{\link[=MCMC-nuts]{MCMC (algorithm-specific)}}: Visualizations designed for use
#'   with particular MCMC algorithms. Currently the only algorithm-specific
#'   plots are for \link{NUTS}, but future releases will include others (e.g.
#'   Gibbs, random walk Metropolis, etc.)
#'   \item \strong{Inference/Prediction}:
#'   (Coming soon) Plots designed to assist with inference and prediction.
#'   \item \strong{\link[=PPC-overview]{PPC}}:
#'   Graphical posterior predictive checks (PPC).
#' }
#'
#' @import ggplot2
#' @import stats
#'
NULL
