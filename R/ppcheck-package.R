#' Graphical posterior predictive checking for Bayesian Models
#'
#' @docType package
#' @name ppcheck-package
#' @aliases ppcheck
#'
#' @description
#' The \pkg{ppcheck} package provides various plotting functions for creating
#' graphical displays comparing observed data to simulated data from the
#' posterior predictive distribution.
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
#' after using the observed data \eqn{y} (a vector of outcome values) and
#' \eqn{X} (a matrix of predictor variables) to update our beliefs about the
#' unknown parameters \eqn{\theta} in the model. For each draw of the parameters
#' \eqn{\theta} from the posterior distribution we generate an entire vector of
#' outcomes, obtaining a matrix of simulations.
#' }
#' \subsection{Notation}{
#' When simulating from the posterior predictive distribution we can use either
#' the same values of the predictors \eqn{X} that we used when fitting the model
#' or new observations of those predictors. When we use the same values of
#' \eqn{X} denote the resulting simulations by \eqn{y^{rep}}{yrep} as they can
#' be thought of as replications of the outcome \eqn{y} rather than predictions
#' for future observations. This corresponds to the notation from Gelman et. al.
#' (2013) and is the notation used throughtout the documentation for this
#' package.
#' }
#' \subsection{Graphical posterior predictive checking}{
#' Using the datasets \eqn{y^{rep}}{yrep} drawn from the posterior predictive
#' distribution, the functions in the \pkg{ppcheck} package create various
#' graphical displays comparing the observed data \eqn{y} to the replications.
#' For a more thorough discussion of posterior predictive checking see
#' Chapter 6 of Gelman et. al. (2013).
#' }
#'
#' @author Jonah Gabry
#'
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#'
#' @import stats
#' @importFrom ggplot2
#'   %+replace%
#'   aes_string
#'   annotate
#'   element_blank element_line element_rect element_text
#'   facet_wrap facet_grid
#'   geom_abline geom_density geom_histogram geom_hline geom_path geom_point geom_vline
#'   ggplot
#'   ggtitle
#'   labs
#'   scale_alpha_manual scale_color_manual scale_fill_manual scale_size_manual
#'   stat_bin
#'   theme theme_classic
#'   xlab
#'
NULL
