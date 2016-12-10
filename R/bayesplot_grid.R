#' Compare plots
#'
#' The \code{bayesplot_grid} function makes it simple to juxtapose plots
#' using common \eqn{x} and/or \eqn{y} axes.
#'
#' @export
#' @param ... One or more ggplot objects.
#' @param plots A list of ggplot objects. Can be used as an alternative to
#'   specifying plot objects via \code{...}.
#' @param grid_args An optional named list of arguments to pass to
#'   \code{\link[gridExtra]{arrangeGrob}} (\code{nrow}, \code{ncol},
#'   \code{widths}, etc.).
#' @param titles An optional character vector of plot titles. If specified,
#'   \code{titles} must must have length equal to the number of plots speficied.
#' @param xlim,ylim Optionally, numeric vectors of length 2 specifying lower and
#'   upper limits for the axes that will be shared across all plots.
#' @param legends If any of the plots have legends should they be displayed?
#'   Defaults to \code{TRUE}.
#'
#' @return An object of class "bayesplot_grid", which has a \code{plot} method.
#'
#' @examples
#' y <- example_y_data()
#' yrep <- example_yrep_draws()
#' stats <- c("sd", "median", "max", "min")
#'
#' color_scheme_set("pink")
#' bayesplot_grid(
#'  plots = lapply(stats, function(s) ppc_stat(y, yrep, stat = s)),
#'  titles = stats,
#'  legends = FALSE,
#'  grid_args = list(ncol = 1)
#' )
#'
#' \dontrun{
#' library(rstanarm)
#' mtcars$log_mpg <- log(mtcars$mpg)
#' fit1 <- stan_glm(mpg ~ wt, data = mtcars)
#' fit2 <- stan_glm(log_mpg ~ wt, data = mtcars)
#'
#' y <- mtcars$mpg
#' yrep1 <- posterior_predict(fit1, draws = 50)
#' yrep2 <- posterior_predict(fit2, fun = exp, draws = 50)
#'
#' color_scheme_set("blue")
#' ppc1 <- ppc_dens_overlay(y, yrep1)
#' ppc1
#' ppc1 + yaxis_text()
#'
#' color_scheme_set("red")
#' ppc2 <- ppc_dens_overlay(y, yrep2)
#' bayesplot_grid(ppc1, ppc2)
#' bayesplot_grid(ppc1, ppc2, xlim = c(-5, 60), ylim = c(0, 0.15))
#' }
#'
bayesplot_grid <-
  function(...,
           plots = list(),
           grid_args = list(),
           titles = character(),
           xlim = NULL,
           ylim = NULL,
           legends = TRUE) {

    suggested_package("gridExtra")
    dots <- list(...)
    if (length(dots) && length(plots)) {
      stop("Arguments '...' and 'plots' can't both be specified.")
    } else if (length(plots)) {
      if (!is.list(plots) || !all_ggplot(plots))
        stop("'plots' must be a list of ggplot objects.")
    } else if (length(dots)) {
      if (!all_ggplot(dots))
        stop("All objects in '...' must be ggplot objects.")
      plots <- dots
    } else {
      stop("No plots specified.")
    }

    if (length(titles)) {
      stopifnot(is.character(titles), length(titles) == length(plots))
      plots <- lapply(seq_along(plots), function(j)
        plots[[j]] + ggtitle(titles[j]))
    }
    if (!legends)
      plots <- lapply(plots, function(p)
        p + legend_none())
    if (!is.null(xlim))
      plots <- lapply(plots, function(p)
        p + ggplot2::xlim(xlim))
    if (!is.null(ylim))
      plots <- lapply(plots, function(p)
        p + ggplot2::ylim(ylim))

    grid_args$grobs <- plots
    g <- do.call(gridExtra::arrangeGrob, args = grid_args)
    as_bayesplot_grid(g)
  }

as_bayesplot_grid <- function(x) {
  structure(x, class = unique(c("bayesplot_grid", class(x))))
}

all_ggplot <- function(x) {
  all(sapply(x, "inherits", what = "ggplot"))
}

#' @export
print.bayesplot_grid <- function(x, ...) {
  gridExtra::grid.arrange(x, ...)
}
#' @export
plot.bayesplot_grid <- print.bayesplot_grid

