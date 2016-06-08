#' Time Series
#'
#' PPCs for time series. Medians and central interval estimates of \code{yrep}
#' by time, with \code{y} overlaid.
#'
#'
#' @name TimeSeries
#' @family PPCs
#'
#' @template args-y-yrep
#' @param time An optional numeric vector, the same length as \code{y}. If
#'   \code{time} is not provided then it is set to \code{1:length(y)}. For
#'   \code{ppc_ts} the values in \code{time} must be unique. For
#'   \code{ppc_ts_grouped} times can be repeated.
#' @param ... Currently unused.
#' @param prob A value between 0 and 1 indicating the desired probability mass
#'   to include in the \code{yrep} intervals. The default is 0.8.
#' @param y_style Should \code{y} be plotted as points connected by lines, only
#'   the points, or only the lines?
#'
#' @template return-ggplot
#'
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#' @template seealso-color-scheme
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{\code{ppc_ts}}{
#'    \code{100*prob}\% central intervals for \code{yrep} at each time point,
#'    with a line through the median of \code{yrep} at each time. Values of
#'    \code{y} are overlaid as points connected by lines, just points, or just
#'    lines.
#'   }
#'   \item{\code{ppc_ts_grouped}}{
#'    Same as \code{ppc_ts} but a separate plot is generated for each level of a
#'    grouping variable.
#'   }
#' }
#'
#' @examples
#' y <- rnorm(50)
#' yrep <- matrix(rnorm(5000, 0, 2), ncol = 50)
#' ppc_ts(y, yrep)
#'
#' time <- rep(1:10, each = 5)
#' group <- gl(5, 1, length = 50, labels = LETTERS[1:5])
#' ppc_ts_grouped(y, yrep, time, group)
#'
NULL

#' @export
#' @rdname TimeSeries
#'
ppc_ts <-
  function(y,
           yrep,
           time,
           ...,
           prob = 0.8,
           y_style = c("both", "points", "lines")) {
    y <- validate_y(y)
    plot_data <- ppc_ts_data(
      y = y,
      yrep = validate_yrep(yrep, y),
      time = validate_time(time, y),
      group = NULL,
      prob = prob
    )
    ppc_ts_plotter(plot_data, y_style = match.arg(y_style))
  }


#' @export
#' @rdname TimeSeries
#' @template args-group
#'
ppc_ts_grouped <-
  function(y,
           yrep,
           time,
           group,
           ...,
           prob = 0.8,
           y_style = c("both", "points", "lines")) {
    y <- validate_y(y)
    plot_data <- ppc_ts_data(
      y = y,
      yrep = validate_yrep(yrep, y),
      time = validate_time(time, y, unique_times = FALSE),
      group = validate_group(group, y),
      prob = prob
    )
    ppc_ts_plotter(plot_data, y_style = match.arg(y_style))
  }




# helpers -----------------------------------------------------------------

# Prepare data for time series plots
#
# @param y,yrep,time,group Validated user inputs
# @param prob Same as above
# @return A grouped_df (dplyr) with columns: time, group (if not NULL), is_y,
#   median, lower, upper
#
ppc_ts_data <-
  function(y,
           yrep,
           time,
           group = NULL,
           prob = 0.8) {

    grouped <- !is.null(group)
    stopifnot(prob > 0 && prob < 1)

    molten_d <- dplyr::rename_(.data = melt_and_stack(y, yrep),
                               .dots = setNames(list( ~ y_id), "time"))
    molten_d <- dplyr::arrange_(.data = molten_d,
                                .dots = list( ~ rep_id, ~ time))
    molten_d$time <- time
    if (grouped) {
      molten_d$group <- group
      dots <- list( ~ time, ~ group, ~ is_y)
    } else {
      dots <- list( ~ time, ~ is_y)
    }
    grouped_d <- dplyr::group_by_(molten_d, .dots = dots)
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
  }


# Make time series plot
#
# @param data The object returned by ppc_ts_data
# @param y_style Same as above
# @return A ggplot object
#
ppc_ts_plotter <- function(data, y_style = "both") {
  grouped <- isTRUE("group" %in% colnames(data))
  yrep_data <- data[!data$is_y, , drop = FALSE]
  y_data <- data[data$is_y, , drop = FALSE]

  graph <- ggplot(
    data = yrep_data,
    mapping = aes_(
      x = ~ time,
      y = ~ median,
      ymin = ~ lower,
      ymax = ~ upper
    )
  ) +
    geom_smooth(
      stat = "identity",
      fill = ppc_color("light"),
      color = ppc_color("light_highlight")
    )

  if (y_style %in% c("both", "lines"))
    graph <- graph +
      geom_line(
        data = y_data,
        color = "black",
        size = 0.25
      )

  if (y_style %in% c("both", "points"))
    graph <- graph +
      geom_point(
        data = y_data,
        shape = 21,
        fill = ppc_color("dark"),
        color = ppc_color("dark_highlight"),
        size = 1
      )

  if (grouped)
    graph <- graph + facet_wrap(facets = "group", scales = "free_y")

  graph +
    labs(x = "Time", y = yrep_label()) +
    dont_expand_x_axis() +
    theme_ppc()
}
