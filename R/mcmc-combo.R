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
mcmc_combo <- function(x, ..., combo = c("trace", "dense"), plot = TRUE) {
  suggested_package("gridExtra")
  plotfuns <- lapply(paste0("mcmc_", combo), function(f) {
    fun <- try(match.fun(f), silent = TRUE)
    if (inherits(fun, "try-error"))
      stop("Function 'mcmc_", f, "' not found.")
    fun
  })
  plotargs <- list(x = x, ...)
  plotargs$facet_args <- list(ncol = 1)
  plots <- lapply(plotfuns, function(f) do.call(f, plotargs))
  combo_plot <- gridExtra::arrangeGrob(grobs = plots, ncol = length(combo))
  if (plot)
    plot(combo_plot)

  invisible(combo_plot)
}
