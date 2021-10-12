#' Plots of posterior or prior predictive distributions
#'
#' @name PPD-overview
#' @aliases PPD
#' @family PPDs
#'
#' @description The **bayesplot** PPD module provides various plotting functions
#'   for creating graphical displays of simulated data from the posterior or
#'   prior predictive distribution. These plots are essentially the same as the
#'   corresponding [PPC] plots but without showing any observed data. Because
#'   these are not "checks" compared to data we use PPD (for prior/posterior
#'   predictive distribution) instead of PPC (for prior/posterior predictive
#'   check).
#'
#' @section PPD plotting functions: The functions for plotting prior and
#'   posterior predictive distributions without observed data each have the
#'   prefix `ppd_` and all have a required argument `ypred` (a matrix of
#'   predictions). The plots are organized into several categories, each with
#'   its own documentation:
#' * [PPD-distributions]: Histograms, kernel density estimates, boxplots, and
#'   other plots of multiple simulated datasets (rows) in `ypred`. These are the
#'   same as the plots in [PPC-distributions] but without including any
#'   comparison to `y`.
#'
#' * [PPD-intervals]: Interval estimates for each predicted observations
#'   (columns) in `ypred`. The x-axis variable can be optionally specified by
#'   the user (e.g. to plot against against a predictor variable or over
#'   time).These are the same as the plots in [PPC-intervals] but without
#'   including any comparison to `y`.
#'
#' * [PPD-test-statistics]: The distribution of a statistic, or a pair of
#'   statistics, over the simulated datasets (rows) in `ypred`. These are the
#'   same as the plots in [PPC-test-statistics] but without including any
#'   comparison to `y`.
#'
#' @template reference-vis-paper
#'
NULL
