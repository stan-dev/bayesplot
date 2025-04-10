#' Plots for Markov chain Monte Carlo simulations
#'
#' @name MCMC-overview
#' @aliases MCMC
#' @family MCMC
#'
#' @description
#' The **bayesplot** MCMC module provides various plotting functions for
#' creating graphical displays of Markov chain Monte Carlo (MCMC) simulations.
#' The **MCMC plotting functions** section, below, provides links to the
#' documentation for various categories of MCMC plots. Currently the MCMC
#' plotting functions accept posterior draws provided in one of the following
#' formats:
#' * __3-D array__: An array with dimensions `Iteration, Chain, Parameter` in
#' that order.
#' * __list__: A list of matrices, where each matrix corresponds to a Markov
#' chain. All of the matrices should have the same number of iterations (rows)
#' and parameters (columns), and parameters should have the same names and be in
#' the same order.
#' * __matrix (2-D array)__: A matrix with one column per parameter. If using
#' matrix there should only be a single Markov chain or all chains should
#' already be merged (stacked).
#' * __data frame__: There are two types of data frames allowed. Either a data
#' frame with one column per parameter (if only a single chain or all chains
#' have already been merged), or a data frame with one column per parameter plus
#' an additional column `"Chain"` that contains the chain number (an integer)
#' corresponding to each row in the data frame.
#' * __draws__: Any of the `draws` formats supported by the
#' \pkg{posterior} package.
#'
#' __Note__: typically the user should *not* include warmup iterations
#' in the object passed to **bayesplot** plotting functions, although for
#' certain plots (e.g. trace plots) it can occasionally be useful to include the
#' warmup iterations for diagnostic purposes.
#'
#' @section MCMC plotting functions:
#'
#' * [Posterior distributions][MCMC-distributions]:
#'   Histograms and kernel density plots of parameter draws, optionally
#'   showing each Markov chain separately.
#' * [Uncertainty intervals][MCMC-intervals]: Uncertainty intervals computed
#'   from parameter draws.
#' * [Trace plots][MCMC-traces]: Times series of parameter draws, optionally
#'   including HMC/NUTS diagnostic information.
#' * [Scatterplots][MCMC-scatterplots]: Scatterplots, heatmaps, and pairs
#'   plots of parameter draws, optionally including HMC/NUTS diagnostic
#'   information.
#' * [Parallel coordinates plots][MCMC-parcoord]: Parallel coordinates plot
#'   of MCMC draws (one dimension per parameter), optionally including
#'   HMC/NUTS diagnostic information.
#' * [Combos][MCMC-combos]: Combination plots (e.g. trace plot + histogram).
#' * [General MCMC diagnostics][MCMC-diagnostics]: MCMC diagnostic plots
#'   including R-hat, effective sample size, autocorrelation.
#'   [NUTS diagnostics][MCMC-nuts]: Special diagnostic plots for
#'     the No-U-Turn Sampler.
#' * [Comparisons to "true" values][MCMC-recover]: Plots comparing MCMC
#'     estimates to "true" parameter values (e.g., values used to simulate data).
#'
#' @template reference-vis-paper
#'
NULL
