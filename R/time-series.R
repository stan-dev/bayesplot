#' PPCs for time series
#'
#' Central interval estimates (quantiles) of \eqn{y^{rep}}{yrep} by time and,
#' optionally, levels of a grouping variable, with \eqn{y} points overlaid.
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
#' group <- gl(5, 10)
#' ppc_ts(y, yrep)
#' ppc_ts_grouped(y, yrep, group)
#'
NULL

#' @rdname time-series
#' @export
#' @param prob A value between 0 and 1 indicating the desired probability mass
#'   to include in the \code{yrep} intervals. The default is 0.8.
#'
ppc_ts <- function(y, yrep, ..., prob = 0.8) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  ppc_time_series(y, yrep, prob = prob, size = 2)
}

#' @rdname time-series
#' @export
#' @template args-group
#'
ppc_ts_grouped <- function(y, yrep, group, ..., prob = 0.8) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  group <- validate_group(group, y)
  ppc_time_series(y, yrep, group, prob = prob, size = 1) +
    facet_wrap("group", scales = "free", labeller = label_both)
}


ppc_time_series <- function(y, yrep, group, prob = 0.8, ...) {
  d <- melt_yrep(yrep)
  if (missing(group)) {
    dots <- list(~y_id)
  } else {
    d$group <- group
    dots <- list(~y_id, ~group)
  }
  molten_d <- dplyr::group_by_(d, .dots = dots)
  alpha <- (1 - prob) / 2
  probs <- c(alpha, 1 - alpha)
  plot_data <- dplyr::summarise_(
    .data = molten_d,
    .dots = list(
      median = ~median(value),
      lower = ~quantile(value, prob = probs[1]),
      upper = ~quantile(value, prob = probs[2])
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
      color = scheme[["light_highlight"]]
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
