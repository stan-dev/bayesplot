#' Graphical posterior predictive checking
#'
#' @name PPC-overview
#' @aliases PPC
#' @family PPCs
#'
#' @description The **bayesplot** PPC module provides various plotting functions
#'   for creating graphical displays comparing observed data to simulated data
#'   from the posterior (or prior) predictive distribution. See the sections
#'   below for a brief discussion of the ideas behind posterior predictive
#'   checking, an overview of the available PPC plots, and tips on providing an
#'   interface to **bayesplot** from another package.
#'
#'   For plots of posterior (or prior) predictive distributions that do _not_
#'   include observed data see [PPD-overview] instead.
#'
#' @details The idea behind posterior predictive checking is simple: if a model
#'   is a good fit then we should be able to use it to generate data that looks
#'   a lot like the data we observed.
#'
#' \subsection{Posterior predictive distribution}{
#' To generate the data used for posterior predictive checks we simulate from
#' the *posterior predictive distribution*. The posterior predictive
#' distribution is the distribution of the outcome variable implied by a model
#' after using the observed data \eqn{y} (a vector of outcome values), and
#' typically predictors \eqn{X}, to update our beliefs about the unknown
#' parameters \eqn{\theta} in the model. For each draw of the parameters
#' \eqn{\theta} from the posterior distribution
#' \eqn{p(\theta \,|\, y, X)}{p(\theta | y, X)}
#' we generate an entire vector of outcomes. The result is
#' an \eqn{S \times N}{S x N} matrix of simulations, where \eqn{S} is the the
#' size of the posterior sample (number of draws from the posterior
#' distribution) and \eqn{N} is the number of data points in \eqn{y}. That is,
#' each row of the matrix is an individual "replicated" dataset of \eqn{N}
#' observations.
#' }
#' \subsection{Notation}{
#' When simulating from the posterior predictive distribution we can use either
#' the same values of the predictors \eqn{X} that we used when fitting the model
#' or new observations of those predictors. When we use the same values of
#' \eqn{X} we denote the resulting simulations by \eqn{y^{rep}}{yrep} as they
#' can be thought of as *replications* of the outcome \eqn{y} rather than
#' predictions for future observations. This corresponds to the notation from
#' Gelman et. al. (2013) and is the notation used throughout the documentation
#' for this package.
#' }
#' \subsection{Graphical posterior predictive checking}{
#' Using the datasets \eqn{y^{rep}}{yrep} drawn from the posterior predictive
#' distribution, the functions in the **bayesplot** package produce various
#' graphical displays comparing the observed data \eqn{y} to the replications.
#' For a more thorough discussion of posterior predictive checking see
#' Chapter 6 of Gelman et. al. (2013).
#' }
#' \subsection{Prior predictive checking}{
#' To use **bayesplot** for *prior* predictive checks you can simply use draws
#' from the prior predictive distribution instead of the posterior predictive
#' distribution. See Gabry et al. (2019) for more on prior predictive checking
#' and when it is reasonable to compare the prior predictive distribution to the
#' observed data. If you want to avoid using the observed data for prior
#' predictive checks then you can use the **bayesplot** [PPD] plots instead,
#' which do not take a `y` argument, or you can use the PPC plots but provide
#' plausible or implausible `y` values that you want to compare to the prior
#' predictive realizations.
#' }
#'
#' @section PPC plotting functions: The plotting functions for prior and
#'   posterior predictive checking all have the prefix `ppc_` and all require
#'   the arguments `y`, a vector of observations, and `yrep`, a matrix of
#'   replications (in-sample predictions). The plots are organized into several
#'   categories, each with its own documentation:
#' * [PPC-distributions]: Histograms, kernel density estimates, boxplots, and
#'   other plots comparing the empirical distribution of data `y` to the
#'   distributions of individual simulated datasets (rows) in `yrep`.
#'
#' * [PPC-test-statistics]: The distribution of a statistic, or a pair of
#'   statistics, over the simulated datasets (rows) in `yrep` compared to value of
#'   the statistic(s) computed from `y`.
#'
#' * [PPC-intervals]: Interval estimates of `yrep` with `y`
#'   overlaid. The x-axis variable can be optionally specified by the user
#'   (e.g. to plot against a predictor variable or over time).
#'
#' * [PPC-errors]: Plots of predictive errors (`y - yrep`) computed from `y` and
#'   each of the simulated datasets (rows) in `yrep`. For binomial models binned
#'   error plots are also available.
#'
#' * [PPC-scatterplots]: Scatterplots (and similar visualizations) of the data
#'   `y` vs. individual simulated datasets (rows) in `yrep`, or vs. the average
#'   value of the distributions of each data point (columns) in `yrep`.
#'
#' * [PPC-discrete]: PPC functions that can only be used if `y` and `yrep` are
#'   discrete. For example, rootograms for count outcomes and bar plots for
#'   ordinal, categorical, and multinomial outcomes.
#'
#' * [PPC-loo]: PPC functions for predictive checks based on (approximate)
#'   leave-one-out (LOO) cross-validation.
#''
#' * [PPC-censoring]: PPC functions comparing the empirical
#'   distribution of censored data `y` to the distributions of individual
#'   simulated datasets (rows) in `yrep`.
#'
#' @section Providing an interface for predictive checking from another package:
#'
#' In addition to the various plotting functions, the **bayesplot** package
#' provides the S3 generic [pp_check()]. Authors of \R packages for
#' Bayesian inference are encouraged to define `pp_check()` methods for the
#' fitted model objects created by their packages. See the package vignettes for
#' more details and a simple example, and see the **rstanarm** and **brms**
#' packages for full examples of `pp_check()` methods.
#'
#' @template reference-vis-paper
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#'
NULL
