#' Pairs plot of MCMC draws
#'
#' @name MCMC-pairs
#' @family MCMC
#'
#' @template args-mcmc-x
#' @template args-pars
#' @template args-regex_pars
#' @param ... Currently ignored.
#'
#' @template return-ggplot
#'
NULL

#' @rdname MCMC-pairs
#' @export
mcmc_pairs <- function(x,
                       pars = NULL,
                       regex_pars = NULL,
                       ...) {
  if (!requireNamespace("GGally", quietly = TRUE))
    stop("Please install the GGally package to use this function.")

  x <- prepare_mcmc_array(x)
  pars <- select_parameters(explicit = pars,
                            patterns = regex_pars,
                            complete = dimnames(x)[[3]])
  x <- x[, , pars, drop = FALSE]
  if (length(transformations))
    x <- transform_draws(x, transformations)

  xdim <- dim(x)
  data <- as.data.frame(array(x, dim = c(prod(xdim[1:2]), xdim[3])))
  colnames(data) <- pars

  message("Processing... this may take a little while")
  graph <- GGally::ggpairs(
    data,
    upper = list(),
    lower = list(
      continuous = GGally::wrap(
        "points",
        shape = 21,
        fill = get_color("dark"),
        color = get_color("dark_highlight"),
        alpha = 0.5
      )
    ),
    diag = list(
      continuous = GGally::wrap(
        "barDiag",
        fill = get_color("mid"),
        color = get_color("mid_highlight"),
        size = .25
      )
    )
  )

  graph + theme_ppc()
}
