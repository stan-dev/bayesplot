#' PPC vs. \eqn{x}
#'
#' Medians and central interval estimates of \code{yrep} by value of \code{x},
#' with \code{y} overlaid.
#'
#' @name PPC-vs-x
#' @family PPCs
#'
#' @template args-y-yrep
#' @param x A numeric vector the same length as \code{y}.
#' @param facet_args An optional list of  arguments (other than \code{facets})
#'   passed to \code{\link[ggplot2]{facet_wrap}} to control faceting.
#' @param prob A value between 0 and 1 indicating the desired probability mass
#'   to include in the \code{yrep} intervals. The default is 0.8.
#' @param y_style Should \code{y} be plotted as points connected by lines, only
#'   the points, or only the lines?
#' @param ... Currently unused.
#'
#' @template return-ggplot
#'
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#' @template seealso-color-scheme
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{\code{ppc_vs_x}}{
#'    \code{100*prob}\% central intervals for \code{yrep} at each \code{x} value,
#'    with a line through the median of \code{yrep}. Values of
#'    \code{y} are overlaid as points connected by lines, just points, or just
#'    lines (depending on the value \code{y_style}).
#'   }
#'   \item{\code{ppc_vs_x_grouped}}{
#'    Same as \code{ppc_vs_x} but a separate plot is generated for each level of a
#'    grouping variable.
#'   }
#' }
#'
#'
#' @examples
#' y <- mtcars$mpg
#' x <- mtcars$wt
#' yrep <- t(replicate(50, {
#'  xb <- c(37, -5) %*% rbind(1, x)
#'  rnorm(length(xb), xb, 3)
#' }))
#'
#' xy_labs <- labs(x = "wt", y = expression(mpg^rep))
#' ppc_vs_x(y, yrep, x) +
#'  axis_ticksize(0.25) +
#'  xy_labs
#'
#' # by group
#' set_color_scheme("green")
#' group <- mtcars$gear
#' ppc_vs_x_grouped(y, yrep, x, group) +
#'  ggplot2::geom_rug(sides = "b") +
#'  xy_labs
#'
#' # force all facets to have same y axis scale (only x is "free")
#' ppc_vs_x_grouped(y, yrep, x, group,
#'                  facet_args = list(scales = "free_x")) +
#'  axis_ticksize(0.25) +
#'  xy_labs
#'
NULL

#' @rdname PPC-vs-x
#' @export
ppc_vs_x <- function(y,
                     yrep,
                     x,
                     ...,
                     prob = 0.8,
                     y_style = c("both", "points", "lines")) {
  y <- validate_y(y)
  plot_data <- ppc_ts_data(
    y = y,
    yrep = validate_yrep(yrep, y),
    time = validate_x(x, y),
    group = NULL,
    prob = prob
  )
  ppc_ts_plotter(
    plot_data,
    y_style = match.arg(y_style)
  ) +
    xlab("x")
}


#' @export
#' @rdname PPC-vs-x
#' @template args-group
#'
ppc_vs_x_grouped <-
  function(y,
           yrep,
           x,
           group,
           ...,
           facet_args = list(),
           prob = 0.8,
           y_style = c("both", "points", "lines")) {
    y <- validate_y(y)
    if (is.null(facet_args[["scales"]]))
      facet_args[["scales"]] <- "free"

    ppc_ts_grouped(
      y = y,
      yrep = validate_yrep(yrep, y),
      time = validate_x(x, y),
      group = validate_group(group, y),
      prob = prob,
      y_style = y_style,
      facet_args = facet_args,
      ...
    ) +
      xlab("x")
  }
