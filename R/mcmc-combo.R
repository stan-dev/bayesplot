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
#' @return A gtable object (the result of calling
#'   \code{\link[gridExtra]{arrangeGrob}}) with \code{length(combo)} columns and
#'   a row for each parameter.
#'
NULL

#' @rdname MCMC-combos
#' @export
mcmc_combo <-
  function(x,
           combo = c("trace", "dens"),
           widths = NULL,
           plot = TRUE,
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
    combo_plot <-
      gridExtra::arrangeGrob(grobs = plots,
                             ncol = length(combo),
                             widths = widths)

    if (plot)
      gridExtra::grid.arrange(combo_plot)

    invisible(combo_plot)
  }
