#' Graphical posterior predictive checking
#'
#' @name PPC-overview
#' @aliases PPC
#' @family PPCs
#'
#' @description
#' The \pkg{bayesplot} PPC module provides various plotting functions for
#' creating graphical displays comparing observed data to simulated data from
#' the posterior predictive distribution. See below for a brief discussion of
#' the ideas behind posterior predictive checking, a description of the
#' structure of this package, and tips on providing an interface to
#' \pkg{bayesplot} from another package.
#'
#' @details
#' The idea behind posterior predictive checking is simple: if a model is a good
#' fit then we should be able to use it to generate data that looks a lot like
#' the data we observed.
#'
#' \subsection{Posterior predictive distribution}{
#' To generate the data used for posterior predictive checks we simulate from
#' the \emph{posterior predictive distribution}. The posterior predictive
#' distribution is the distribution of the outcome variable implied by a model
#' after using the observed data \eqn{y} (a vector of outcome values), and
#' typically predictors \eqn{X}, to update our beliefs about the unknown
#' parameters \eqn{\theta} in the model. For each draw of the parameters
#' \eqn{\theta} from the posterior distribution \eqn{p(\theta \,|\, y,
#' X)}{p(\theta | y, X)} we generate an entire vector of outcomes. The result is
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
#' can be thought of as \emph{replications} of the outcome \eqn{y} rather than
#' predictions for future observations. This corresponds to the notation from
#' Gelman et. al. (2013) and is the notation used throughtout the documentation
#' for this package.
#' }
#' \subsection{Graphical posterior predictive checking}{
#' Using the datasets \eqn{y^{rep}}{yrep} drawn from the posterior predictive
#' distribution, the functions in the \pkg{bayesplot} package produce various
#' graphical displays comparing the observed data \eqn{y} to the replications.
#' For a more thorough discussion of posterior predictive checking see
#' Chapter 6 of Gelman et. al. (2013).
#' }
#'
#' @section PPC plotting functions:
#'
#' The plotting functions for posterior predictive checking in this package are
#' organized into several categories, each with its own documentation:
#'
#' \describe{
#'   \item{\strong{\link[=PPC-distributions]{Distributions}}}{
#'     Histograms and density plots comparing the empirical distribution of the
#'     observed data \code{y} to the distributions of individual replicated
#'     datasets (rows) in \code{yrep}.
#'   }
#'   \item{\strong{\link[=PPC-test-statistics]{Test statistics}}}{
#'     The distribution of a test statistic, or a pair of test statistics, over
#'     the replicated datasets (rows) in \code{yrep} compared to value of the
#'     statistic(s) computed from \code{y}.
#'   }
#'   \item{\strong{\link[=PPC-intervals]{Intervals}}}{
#'     Interval estimates of \code{yrep} with \code{y} overlaid. The x-axis
#'     variable can be optionally specified by the user (e.g. to plot against
#'     against a predictor variable or over time).
#'   }
#'   \item{\strong{\link[=PPC-errors]{Predictive errors}}}{
#'     Plots of predictive errors (\code{y - yrep}) computed from \code{y} and
#'     replicated datasets (rows) in \code{yrep}. For binomial models binned
#'     error plots are also available.
#'   }
#'   \item{\strong{\link[=PPC-scatterplots]{Scatterplots}}}{
#'     Scatterplots of the observed data \code{y} vs. individual replicated
#'     datasets (rows) in \code{yrep}, or vs. the average value of the
#'     distributions of each data point (columns) in \code{yrep}.
#'   }
#' }
#'
#' @section Providing an interface for posterior predictive checking from another package:
#'
#' In addition to the various plotting functions, the \pkg{bayesplot} package
#' provides the S3 generic \code{\link{pp_check}}. Authors of \R packages for
#' Bayesian inference are encouraged to define \code{pp_check} methods for the
#' fitted model objects created by their packages. See the package vignettes for
#' more details and an example.
#'
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#'
NULL
