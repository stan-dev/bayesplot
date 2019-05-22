#' Combination plots
#'
#' @name MCMC-combos
#' @family MCMC
#'
#' @template args-mcmc-x
#' @param ... Arguments passed to the plotting functions named in `combo`.
#' @param combo A character vector with at least two elements. Each element of
#'   `combo` corresponds to a column in the resulting graphic and should be the
#'   name of one of the available [MCMC][MCMC-overview] functions (omitting the
#'   `mcmc_` prefix).
#' @param widths A numeric vector the same length as `combo` specifying
#'   relative column widths. For example, if the plot has two columns, then
#'   `widths = c(2, 1)` will allocate more space for the first column by a
#'   factor of 2 (as would `widths = c(.3, .15)`, etc.). The default,
#'   `NULL`, allocates the same horizontal space for each column.
#' @param gg_theme Unlike most of the other **bayesplot** functions,
#'   `mcmc_combo` returns a gtable object rather than a ggplot object, and
#'   so theme objects can't be added directly to the returned plot object. The
#'   `gg_theme` argument helps get around this problem by accepting a
#'   **ggplot2** [theme][ggplot2::theme] object that is added to each of the
#'   plots *before* combining them into the gtable object that is returned.
#'   This can be a theme object created by a call to [ggplot2::theme()] or
#'   one of the **bayesplot** convenience functions, e.g.
#'   [legend_none()] (see the **Examples** section, below).
#'
#' @return A gtable object (the result of calling
#'   [gridExtra::arrangeGrob()]) with `length(combo)` columns and
#'   a row for each parameter.
#'
#' @examples
#' # some parameter draws to use for demonstration
#' x <- example_mcmc_draws()
#' dim(x)
#' dimnames(x)
#'
#' mcmc_combo(x, pars = c("alpha", "sigma"))
#' mcmc_combo(x, pars = c("alpha", "sigma"), widths = c(1, 2))
#'
#' \donttest{
#' # change second plot, show log(sigma) instead of sigma,
#' # and remove the legends
#' color_scheme_set("mix-blue-red")
#' mcmc_combo(
#'  x,
#'  combo = c("dens_overlay", "trace"),
#'  pars = c("alpha", "sigma"),
#'  transformations = list(sigma = "log"),
#'  gg_theme = legend_none()
#' )
#'
#' # same thing but this time also change the entire ggplot theme
#' mcmc_combo(
#'  x,
#'  combo = c("dens_overlay", "trace"),
#'  pars = c("alpha", "sigma"),
#'  transformations = list(sigma = "log"),
#'  gg_theme = ggplot2::theme_gray() + legend_none()
#' )
#' }
#'
NULL

#' @rdname MCMC-combos
#' @export
mcmc_combo <-
  function(x,
           combo = c("dens", "trace"),
           ...,
           widths = NULL,
           gg_theme = NULL) {
    suggested_package("gridExtra")

    if (length(combo) < 2) {
      abort("'combo' should have at least two elements.")
    }

    plotfuns <- paste0("mcmc_", combo)
    not_found <- setdiff(plotfuns, available_mcmc())
    if (length(not_found)) {
      abort(paste(
        "The following functions were not found:",
        paste(not_found, collapse = ", ")
      ))
    }
    plotfuns <-
      lapply(plotfuns, function(x)
        get(x, pos = asNamespace("bayesplot"), mode = "function"))

    args <- list(x = x, ...)
    if (is.list(args$facet_args)) {
      args$facet_args[["ncol"]] <- 1
      args$facet_args[["nrow"]] <- NULL
    } else {
      args$facet_args <- list(ncol = 1, nrow = NULL)
    }

    plots <- lapply(plotfuns, function(f) suppressWarnings(do.call(f, args)))
    plots <- lapply(plots, function(x) x + bayesplot_theme_get())

    if (!is.null(gg_theme))
      plots <- lapply(plots, function(x) x + gg_theme)

    bayesplot_grid(
      plots = plots,
      grid_args = list(ncol = length(combo), widths = widths)
    )
  }
