#' PPC vs. \eqn{x}
#'
#' Medians and central interval estimates of \code{yrep} by value of \code{x},
#' with \code{y} overlaid. See the \strong{Plot Descriptions} section, below.
#'
#' @name PPC-vs-x
#' @family PPCs
#'
#' @inheritParams ppc_ts
#' @param x A numeric vector the same length as \code{y}.
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
#'    \code{100*prob}\% central intervals for \code{yrep} at each \code{x}
#'    value, with a line through the median of \code{yrep}. Values of \code{y}
#'    are overlaid as points connected by lines, just points, or just lines
#'    (depending on the value \code{y_style}).
#'   }
#'   \item{\code{ppc_vs_x_grouped}}{
#'    Same as \code{ppc_vs_x} but a separate plot is generated for each level of
#'    a grouping variable.
#'   }
#' }
#'
#'
#' @examples
#' y <- mtcars$mpg
#' x <- mtcars$wt
#' # some fake yrep data
#' yrep <- t(replicate(50, {
#'  xb <- c(37, -5) %*% rbind(1, x)
#'  rnorm(length(xb), xb, 3)
#' }))
#'
#' set_color_scheme("teal")
#' ppc_vs_x(y, yrep, x)
#' (p <- ppc_vs_x(y, yrep, x, y_style = "points"))
#'
#' xy_labs <- ggplot2::labs(x = "wt", y = expression(mpg^rep))
#' p +
#'  xaxis_ticks(size = 0.25) +
#'  yaxis_ticks(size = 0.25) +
#'  xy_labs
#'
#' \dontrun{
#' # example using rstanarm model
#' library(rstanarm)
#' fit <- stan_glmer(mpg ~ disp + am + (1|cyl),
#'                   data = mtcars, iter = 1000)
#'
#'
#' set_color_scheme("purple")
#' with(model.frame(fit),
#'  ppc_vs_x_grouped(
#'    y = mpg,
#'    yrep = posterior_predict(fit),
#'    x = disp,
#'    group = cyl, # grouping by level of cyl
#'    y_style = "points"
#'  )
#' )
#'
#' # same plot but forcing all facets to have
#' # same y axis scale (only x is "free")
#' with(model.frame(fit),
#'  ppc_vs_x_grouped(
#'    y = mpg,
#'    yrep = posterior_predict(fit),
#'    x = disp,
#'    group = cyl, # grouping by level of cyl
#'    y_style = "points",
#'    facet_args = list(scales = "free_x")
#'  )
#' )
#' }
#'
NULL

#' @rdname PPC-vs-x
#' @export
ppc_vs_x <- function(y,
                     yrep,
                     x,
                     ...,
                     prob = 0.8,
                     alpha = 0.33,
                     size = 1,
                     y_style = c("points", "lines")) {
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
    y_style = y_style,
    alpha = alpha,
    size = size
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
           alpha = 0.33,
           size = 1,
           y_style = c("points", "lines")) {
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
      alpha = alpha,
      size = size,
      ...
    ) +
      xlab("x")
  }

