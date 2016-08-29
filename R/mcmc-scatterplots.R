#' Scatterplots of MCMC draws
#'
#' Scatterplots of MCMC draws. See the \strong{Plot Descriptions} section,
#' below, for details.
#'
#' @name MCMC-scatterplots
#' @family MCMC
#'
#' @template args-mcmc-x
#' @param pars An optional character vector of parameter names. (\strong{Note}:
#' for \code{mcmc_scatter} only two parameters can be selected.)
#' @template args-regex_pars
#' @template args-transformations
#' @param ... Currently ignored.
#' @param size,alpha Passed to \code{\link[ggplot2]{geom_point}}.
#'
#' @return A ggplot object that can be further customized using the
#'   \pkg{ggplot2} package.
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{\code{mcmc_scatter}}{
#'    Bivariate scatterplot of posterior draws (for two parameters).
#'   }
#'   \item{\code{mcmc_pairs}}{
#'   Coming soon.
#'   }
#' }
#'
#'
#' @template seealso-color-scheme
#'
#'
#' @examples
#' # some parameter draws to use for demonstration
#' x <- example_mcmc_draws(params = 6)
#' dimnames(x)
#'
#' # scatterplot of alpha vs log(sigma)
#' set_color_scheme("teal")
#' (p <- mcmc_scatter(x, pars = c("alpha", "sigma"),
#'                   trans = list(sigma = "log")))
#'
#' # add ellipse
#' p + ggplot2::stat_ellipse(level = 0.9, color = "darkgray", size = 2)
#'
#' # can also add lines/smooths
#' set_color_scheme("pink")
#' (p2 <- mcmc_scatter(x, pars = c("alpha", "beta[3]"), alpha = 0.5, size = 3))
#' p2 + ggplot2::geom_smooth(method = "lm", se = FALSE, color = "gray20")
#'
NULL

#' @rdname MCMC-scatterplots
#' @export
mcmc_scatter <- function(x,
                         pars = character(),
                         regex_pars = character(),
                         transformations = list(),
                         ...,
                         size = 2.5,
                         alpha = 0.8) {

  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  if (dim(x)[3] != 2)
    stop(
      "For 'mcmc_scatter' exactly 2 parameters must be selected. ",
      "'mcmc_pairs' for more than 2 parameters will be available ",
      "in a future release."
    )

  x <- merge_chains(x)
  parnames <- colnames(x)[1:2]
  ggplot(
    data = data.frame(x = c(x[, 1]), y = c(x[, 2])),
    mapping = aes_(x = ~ x, y = ~ y)
  ) +
    geom_point(
      shape = 21,
      color = get_color("dh"),
      fill = get_color("d"),
      size = size,
      alpha = alpha
    ) +
    labs(x = parnames[1], y = parnames[2]) +
    theme_default()
}

#
# #' @rdname MCMC-scatterplots
# #' @export
# mcmc_pairs <- function(x,
#                        pars = character(),
#                        regex_pars = character(),
#                        transformations = list(),
#                        ...,
#                        size = 1.5,
#                        alpha = 0.8) {
#   suggested_package("GGally")
#
#   x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
#   user_pars <- parameter_names(x)
#   dimnames(x)[[3]] <- rename_pairs_parameters(user_pars)
#   if (!has_multiple_params(x))
#     STOP_need_multiple_params()
#
#   data <- merge_chains(x)
#
#   message("Processing... this may take a little while")
#   graph <- GGally::ggpairs(
#     data,
#     upper = list(),
#     lower = list(
#       continuous = GGally::wrap(
#         "points",
#         shape = 21,
#         fill = get_color("d"),
#         color = get_color("dh"),
#         alpha = alpha,
#         size = size
#       )
#     ),
#     diag = list(
#       continuous = GGally::wrap(
#         "barDiag",
#         fill = get_color("l"),
#         color = get_color("lh"),
#         size = .25
#       )
#     )
#   )
#
#   graph <- reset_pairs_parameters(graph, user_pars)
#   graph + theme_default()
# }
#
#
#
# # internal ----------------------------------------------------------------
# # Remove special characters in parameter names so ggpairs doesn't complain
# rename_pairs_parameters <- function(pars) {
#   stopifnot(is.character(pars))
#   pars <- gsub("\\[|\\:", "_", pars)
#   pars <- gsub("\\(|\\)|\\]", "", pars)
#   gsub(" ", "_", pars)
# }
#
# # Reset axis labels to original parameter names
# reset_pairs_parameters <- function(ggmatrix, pars) {
#   ggmatrix$xAxisLabels <- pars
#   ggmatrix$yAxisLabels <- pars
#   ggmatrix
# }
