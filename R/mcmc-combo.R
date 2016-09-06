#' Combination plots
#'
#' @name MCMC-combos
#' @family MCMC
#'
#' @template args-mcmc-x
#' @param ... Arguments passed to the plotting functions named in \code{combo}.
#' @param combo A character vector with at least two elements. Each element of
#'   \code{combo} corresponds to a column in the resulting graphic and should be
#'   the name of one of the available \link{MCMC} functions (omitting the
#'   \code{mcmc_} prefix).
#' @param widths A numeric vector the same length as \code{combo} specifying
#'   relative column widths. For example, if the plot has two columns, then
#'   \code{widths = c(2, 1)} will allocate more space for the first column by a
#'   factor of 2 (as would \code{widths = c(.3, .15)}, etc.). The default,
#'   \code{NULL}, allocates the same horiztonal space for each column.
#' @param plot A logical value indicating whether or not to display the plots.
#'   If \code{plot} is \code{FALSE} then the object is returned invisibly
#'   without plotting.
#' @param gg_theme Unlike most of the other \pkg{bayesplot} functions,
#'   \code{mcmc_combo} returns a gtable object rather than a ggplot object, and
#'   so theme objects can't be added directly to the returned plot object. The
#'   \code{gg_theme} argument helps get around this problem by accepting a
#'   \pkg{ggplot2} \link[ggplot2]{theme} object that is applied to each of the
#'   plots \emph{before} combining them into the gtable object that is returned.
#'   This can be a theme object created by a call to \code{ggplot2::theme} or
#'   one of the \pkg{bayesplot} convenience functions, e.g.
#'   \code{\link{no_legend}} (see the \strong{Examples} section, below).
#'
#' @return A gtable object (the result of calling
#'   \code{\link[gridExtra]{arrangeGrob}}) with \code{length(combo)} columns and
#'   a row for each parameter.
#'
#' @examples
#' # some parameter draws to use for demonstration
#' x <- example_mcmc_draws()
#' dim(x)
#' dimnames(x)
#'
#' set_color_scheme("blue")
#' mcmc_combo(x, pars = c("alpha", "sigma"))
#' mcmc_combo(x, pars = c("alpha", "sigma"), widths = c(2, 1))
#'
#' # change second plot, show log(sigma) instead of sigma,
#' # and remove the legends
#' set_color_scheme("mix-blue-red")
#' mcmc_combo(
#'  x,
#'  combo = c("trace", "dens_overlay"),
#'  pars = c("alpha", "sigma"),
#'  transformations = list(sigma = "log"),
#'  gg_theme = no_legend()
#' )
#'
#' # same thing but this time also change the entire ggplot theme
#' mcmc_combo(
#'  x,
#'  combo = c("trace", "dens_overlay"),
#'  pars = c("alpha", "sigma"),
#'  transformations = list(sigma = "log"),
#'  gg_theme = ggplot2::theme_gray() + no_legend()
#' )
#'
NULL

#' @rdname MCMC-combos
#' @export
mcmc_combo <-
  function(x,
           combo = c("trace", "dens"),
           widths = NULL,
           plot = TRUE,
           gg_theme = NULL,
           ...) {
    suggested_package("gridExtra")
    plotfuns <- lapply(paste0("mcmc_", combo), function(f) {
      fun <- try(match.fun(f), silent = TRUE)
      if (inherits(fun, "try-error"))
        stop("Function '", f, "' not found.")
      fun
    })
    args <- list(x = x, ...)
    if (is.list(args$facet_args)) {
      args$facet_args[["ncol"]] <- 1
      args$facet_args[["nrow"]] <- NULL
    } else {
      args$facet_args <- list(ncol = 1, nrow = NULL)
    }

    plots <- lapply(plotfuns, function(f) do.call(f, args))
    if (!is.null(gg_theme))
      plots <- lapply(plots, function(x) x + gg_theme)

    combo_plot <-
      gridExtra::arrangeGrob(grobs = plots,
                             ncol = length(combo),
                             widths = widths)

    if (plot)
      gridExtra::grid.arrange(combo_plot)

    invisible(combo_plot)
  }
