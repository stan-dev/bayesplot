#' PPCs for time series
#'
#' Medians and central interval estimates of \eqn{y^{rep}}{yrep} by time, with
#' \eqn{y} overlaid.
#'
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
#' yrep <- matrix(rnorm(5000, 0, 2), ncol = 50)
#' ppc_ts(y, yrep)
#'
#' time <- sample(200, size = 50)
#' ppc_ts(y, yrep, time)
#'
#'
NULL

#' @rdname time-series
#' @export
#' @param time An optional numeric vector, the same length as \eqn{y}, of
#'   \emph{unique} time values. If \code{time} is missing then it is set to
#'   \code{1:length(y)}.
#' @param prob A value between 0 and 1 indicating the desired probability mass
#'   to include in the \code{yrep} intervals. The default is 0.8.
#' @param y_style Should \code{y} be plotted as points connected by lines, only
#'   the points, or only the lines?
#'
ppc_ts <- function(y,
                   yrep,
                   time,
                   ...,
                   prob = 0.8,
                   y_style = c("both", "points", "lines")) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  time <- validate_time(time, y)

  y_style <- match.arg(y_style)

  molten_d <- ppc_dist_data(y, yrep)
  molten_d <- dplyr::rename_(molten_d, .dots = setNames(list(~y_id), "time"))
  molten_d <- dplyr::arrange_(molten_d, .dots = list(~rep_id, ~time))
  molten_d$time <- time
  grouped_d <- dplyr::group_by_(molten_d, .dots = list(~time, ~is_y))
  alpha <- (1 - prob) / 2
  probs <- c(alpha, 1 - alpha)
  plot_data <- dplyr::summarise_(
    .data = grouped_d,
    .dots = list(
      median = ~ median(value),
      lower = ~ quantile(value, prob = probs[1]),
      upper = ~ quantile(value, prob = probs[2])
    )
  )
  pd_y <- plot_data[plot_data$is_y, , drop = FALSE]
  pd_yrep <- plot_data[!plot_data$is_y, , drop = FALSE]

  scheme <- get_color_scheme()
  graph <- ggplot(
    data = pd_yrep,
    mapping = aes_string(
      x = "time",
      y = "median",
      ymin = "lower",
      ymax = "upper"
    )
  ) +
    geom_smooth(stat = "identity",
                fill = scheme[["light"]],
                color = scheme[["light_highlight"]])

  if (y_style %in% c("both", "lines"))
    graph <- graph +
      geom_line(
        data = pd_y,
        color = "black", # scheme[["mid"]],
        size = 0.25
      )

  if (y_style %in% c("both", "points"))
    graph <- graph +
    geom_point(
      data = pd_y,
      shape = 21,
      fill = scheme[["dark"]],
      color = scheme[["dark_highlight"]],
      size = 1
    )

  graph +
    labs(x = "Time", y = yrep_label()) +
    coord_cartesian(expand = FALSE) +
    theme_ppc()
}

validate_time <- function(time, y) {
  if (missing(time)) {
    time <- 1:length(y)
  } else {
    stopifnot(is.numeric(time),
              identical(length(time), length(y)),
              identical(length(time), length(unique(time))))
  }
  time
}


# @rdname time-series
# @export
# @template args-group
#
# ppc_ts_grouped <- function(y, yrep, group, ..., prob = 0.8) {
#   y <- validate_y(y)
#   yrep <- validate_yrep(yrep, y)
#   group <- validate_group(group, y)
#   ppc_time_series(y, yrep, group, prob = prob, size = 1) +
#     facet_wrap("group", scales = "free", labeller = label_both)
# }
