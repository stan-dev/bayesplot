#' PPCs for time series
#'
#' Interval estimates of \eqn{y^{rep}}{yrep} by time and, optionally, levels of
#' a grouping variable, with \eqn{y} points overlaid.
#'
#' @name time-series
#' @family PPCs
#'
#' @template args-y-yrep
#' @param ... Currently unused.
#'
#' @template return-ggplot
#'
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#' @template seealso-color-scheme
#'
#' @examples
#' y <- rnorm(50)
#' yrep <- matrix(rnorm(5000), ncol = 50)
#' ppc_ts(y, yrep)
#'
#' group <- gl(5, 10)
#' ppc_ts_grouped(y, yrep, group)
#'
NULL

#' @export
#' @rdname time-series
#'
ppc_ts <- function(y, yrep, ...) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  ppc_time_series(y, yrep, size = 2)
}

#' @rdname time-series
#' @export
#' @template args-group
#'
ppc_ts_grouped <- function(y, yrep, group, ...) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  group <- validate_group(group, y)
  ppc_time_series(y, yrep, group, size = 1) +
    facet_wrap("group", scales = "free", labeller = label_both)
}


ppc_time_series <- function(y, yrep, group, ...) {
  d <- melt_yrep(yrep)
  if (missing(group)) {
    dots <- list(~y_id)
  } else {
    d$group <- group
    dots <- list(~y_id, ~group)
  }
  molten_d <- dplyr::group_by_(d, .dots = dots)
  plot_data <- dplyr::summarise_(
    .data = molten_d,
    .dots = list(
      median = ~median(value),
      lower = ~quantile(value, prob = 0.1),
      upper = ~quantile(value, prob = 0.9)
    )
  )

  scheme <- get_color_scheme()
  ggplot(
    data = plot_data,
    mapping = aes_string(
      x = "y_id",
      y = "median",
      ymin = "lower",
      ymax = "upper"
    )
  ) +
    geom_smooth(
      stat = "identity",
      fill = scheme[["light"]],
      color = scheme[["light_highlight"]],
      alpha = 0.25
    ) +
    geom_line(color = scheme[["mid"]]) +
    geom_point(
      shape = 21,
      fill = scheme[["dark"]],
      color = scheme[["dark_highlight"]],
      ...
    ) +
    labs(x = "Time", y = yrep_label()) +
    coord_cartesian(expand = FALSE) +
    theme_ppc()
}
