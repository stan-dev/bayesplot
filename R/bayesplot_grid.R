#' Arrange plots in a grid
#'
#' The `bayesplot_grid` function makes it simple to juxtapose plots using
#' common \eqn{x} and/or \eqn{y} axes.
#'
#' @export
#' @param ... One or more ggplot objects.
#' @param plots A list of ggplot objects. Can be used as an alternative to
#'   specifying plot objects via `...`.
#' @param grid_args An optional named list of arguments to pass to
#'   [gridExtra::arrangeGrob()] (`nrow`, `ncol`,
#'   `widths`, etc.).
#' @param titles,subtitles Optional character vectors of plot titles and
#'   subtitles. If specified, `titles` and `subtitles` must must have
#'   length equal to the number of plots specified.
#' @param xlim,ylim Optionally, numeric vectors of length 2 specifying lower and
#'   upper limits for the axes that will be shared across all plots.
#' @param legends If any of the plots have legends should they be displayed?
#'   Defaults to `TRUE`.
#' @param save_gg_objects If `TRUE`, the default, then the ggplot objects
#'   specified in `...` or via the `plots` argument are saved in a
#'   list in the `"bayesplots"` component of the returned object.
#'   Setting this to `FALSE` will make the returned object smaller but
#'   these individual plot objects will not be available.
#'
#' @return An object of class `"bayesplot_grid"` (essentially a gtable object
#'   from [gridExtra::arrangeGrob()]), which has a `plot` method.
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
#' fit1 <- stan_glm(mpg ~ wt, data = mtcars, refresh = 0)
#' fit2 <- stan_glm(log_mpg ~ wt, data = mtcars, refresh = 0)
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
#'
#' # make sure the plots use the same limits for the axes
#' bayesplot_grid(ppc1, ppc2, xlim = c(-5, 60), ylim = c(0, 0.2))
#'
#' # remove the legends and add text
#' bayesplot_grid(ppc1, ppc2, xlim = c(-5, 60), ylim = c(0, 0.2),
#'                legends = FALSE, subtitles = rep("Predicted MPG", 2))
#' }
#'
bayesplot_grid <-
  function(...,
           plots = list(),
           xlim = NULL,
           ylim = NULL,
           grid_args = list(),
           titles = character(),
           subtitles = character(),
           legends = TRUE,
           save_gg_objects = TRUE) {

    suggested_package("gridExtra")
    dots <- list(...)
    if (length(dots) && length(plots)) {
      abort("Arguments '...' and 'plots' can't both be specified.")
    } else if (length(plots)) {
      if (!is.list(plots) || !all_ggplot(plots))
        abort("'plots' must be a list of ggplot objects.")
    } else if (length(dots)) {
      if (!all_ggplot(dots))
        abort("All objects in '...' must be ggplot objects.")
      plots <- dots
    } else {
      abort("No plots specified.")
    }

    if (length(titles)) {
      stopifnot(is.character(titles), length(titles) == length(plots))
      plots <- lapply(seq_along(plots), function(j)
        plots[[j]] + ggtitle(titles[j]))
    }
    if (length(subtitles)) {
      stopifnot(is.character(subtitles), length(subtitles) == length(plots))
      plots <- lapply(seq_along(plots), function(j)
        plots[[j]] + labs(subtitle = subtitles[j]))
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
    if (save_gg_objects) {
      g$bayesplots <- plots
    }
    as_bayesplot_grid(g)
  }


# internal ----------------------------------------------------------------
as_bayesplot_grid <- function(x) {
  structure(x, class = unique(c("bayesplot_grid", class(x))))
}

is_bayesplot_grid <- function(x) {
  inherits(x, "bayesplot_grid")
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
