#' Scatterplots of MCMC draws
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
#'
#' @return For \code{mcmc_scatter}, a ggplot object that can be further
#'   customized using the \pkg{ggplot2} package. For \code{mcmc_pairs}, a
#'   \code{\link[GGally]{ggpairs}} object.
#'
#' @template seealso-color-scheme
#'
#'
#' @examples
#' # Some draws to play with for examples
#' x <- fake_draws(params = 6)
#' dimnames(x)
#'
#' # scatterplot of alpha vs log(sigma)
#' (p <- mcmc_scatter(x, pars = c("alpha", "sigma"),
#'                   trans = list(sigma = "log")))
#'
#' # add ellipse
#' p + ggplot2::stat_ellipse(level = 0.9)
#'
#' mcmc_scatter(x, pars = c("beta[1]", "beta[4]"))
#' mcmc_scatter(x, regex = "beta\\[[1,4]")
#'
#' # pairs plot with histograms along the diagonal
#' mcmc_pairs(x, pars = c("alpha", "sigma", "beta[3]"))
#'
NULL

#' @rdname MCMC-scatterplots
#' @export
#' @param size,alpha Passed to \code{\link[ggplot2]{geom_point}}.
mcmc_scatter <- function(x,
                         pars = character(),
                         regex_pars = character(),
                         transformations = list(),
                         ...,
                         size = 2.5,
                         alpha = 0.75) {

  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  if (dim(x)[3] != 2)
    stop("For 'mcmc_scatter' exactly 2 parameters must be selected.")

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

#' @rdname MCMC-scatterplots
#' @export
mcmc_pairs <- function(x,
                       pars = character(),
                       regex_pars = character(),
                       transformations = list(),
                       ...) {
  suggested_package("GGally")

  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  user_pars <- parameter_names(x)
  dimnames(x)[[3]] <- rename_pairs_parameters(user_pars)
  if (!has_multiple_params(x))
    STOP_need_multiple_params()

  data <- merge_chains(x)

  message("Processing... this may take a little while")
  graph <- GGally::ggpairs(
    data,
    upper = list(),
    lower = list(
      continuous = GGally::wrap(
        "points",
        shape = 21,
        fill = get_color("d"),
        color = get_color("dh"),
        alpha = 0.5
      )
    ),
    diag = list(
      continuous = GGally::wrap(
        "barDiag",
        fill = get_color("m"),
        color = get_color("mh"),
        size = .25
      )
    )
  )

  graph <- reset_pairs_parameters(graph, user_pars)
  graph + theme_default()
}



# internal ----------------------------------------------------------------
# Remove special characters in parameter names so ggpairs doesn't complain
rename_pairs_parameters <- function(pars) {
  stopifnot(is.character(pars))
  pars <- gsub("\\[|\\:", "_", pars)
  pars <- gsub("\\(|\\)|\\]", "", pars)
  gsub(" ", "_", pars)
}

# Reset axis labels to original parameter names
reset_pairs_parameters <- function(ggmatrix, pars) {
  ggmatrix$xAxisLabels <- pars
  ggmatrix$yAxisLabels <- pars
  ggmatrix
}
