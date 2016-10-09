#' PPC time series
#'
#' PPCs for time series. Medians and central interval estimates of \code{yrep}
#' by time, with \code{y} overlaid. See the \strong{Plot Descriptions} section,
#' below.
#'
#'
#' @name PPC-time-series
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
#' @param alpha,size Arguments passed to \code{\link[ggplot2]{geom_ribbon}}. See
#'   Examples.
#' @param y_style A character vector of length 1 or 2 indicating how \code{y}
#'   should be plotted. If \code{c("points","lines")}, the default, then
#'   \code{y} is plotted as connected points. Use only \code{"points"} to omit
#'   the connecting lines, and use only \code{"lines"} to show the lines
#'   without the points.
#'
#' @template details-binomial
#' @template return-ggplot
#'
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{\code{ppc_ts}}{
#'    \code{100*prob}\% central intervals for \code{yrep} at each time point,
#'    with a line through the median of \code{yrep} at each time. Values of
#'    \code{y} are overlaid as points connected by lines, just points, or just
#'    lines (depending on the value \code{y_style}).
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
#' ppc_ts(y, yrep, y_style = "lines")
#'
#' set_color_scheme("mix-green-blue")
#' time <- seq(1, 100, by = 2)
#' ppc_ts(y, yrep, time, alpha = 1, size = .5)
#' ppc_ts(y, yrep, time, size = .5, y_style = "lines")
#' ppc_ts(y, yrep, time, alpha = 1, size = .25, y_style = "lines")
#'
#' set_color_scheme("pink")
#' time <- rep(1:10, each = 5)
#' group <- gl(5, 1, length = 50, labels = LETTERS[1:5])
#' ppc_ts_grouped(y, yrep, time, group) + xaxis_text(FALSE)
#'
#' ppc_ts_grouped(
#'  y, yrep, time, group,
#'  facet_args = list(scales = "fixed"),
#'  alpha = 1,
#'  size = 2
#' ) +
#'  xaxis_text(FALSE) +
#'  plot_bg(fill = "gray20")
#'
NULL

#' @export
#' @rdname PPC-time-series
#'
ppc_ts <-
  function(y,
           yrep,
           time,
           ...,
           prob = 0.8,
           alpha = 0.33,
           size = 1,
           y_style = c("points", "lines")) {
    y <- validate_y(y)
    plot_data <- ppc_ts_data(
      y = y,
      yrep = validate_yrep(yrep, y),
      time = validate_time(time, y),
      group = NULL,
      prob = prob
    )
    ppc_ts_plotter(
      plot_data,
      y_style = match.arg(y_style, several.ok = TRUE),
      alpha = alpha,
      size = size
    )
  }


#' @export
#' @rdname PPC-time-series
#' @template args-group
#' @param facet_args An optional list of  arguments (other than \code{facets})
#'   passed to \code{\link[ggplot2]{facet_wrap}} to control faceting.
#'
ppc_ts_grouped <-
  function(y,
           yrep,
           time,
           group,
           facet_args = list(),
           ...,
           prob = 0.8,
           alpha = 0.33,
           size = 1,
           y_style = c("points", "lines")) {

    y <- validate_y(y)
    plot_data <- ppc_ts_data(
      y = y,
      yrep = validate_yrep(yrep, y),
      time = validate_time(time, y, unique_times = FALSE),
      group = validate_group(group, y),
      prob = prob
    )

    ppc_ts_plotter(
      plot_data,
      y_style = match.arg(y_style, several.ok = TRUE),
      facet_args = facet_args,
      alpha = alpha,
      size = size
    )
  }




# internal -----------------------------------------------------------------

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
ppc_ts_plotter <-
  function(data,
           y_style = c("points", "lines"),
           facet_args = list(),
           alpha = 0.33,
           size = 1) {
  y_style <- match.arg(y_style, several.ok = TRUE)
  grouped <- isTRUE("group" %in% colnames(data))
  data[["is_y"]] <- as.logical(data[["is_y"]])
  yrep_data <- dplyr::filter_(data, ~ !is_y)
  y_data <- dplyr::filter_(data, ~ is_y)

  graph <- ggplot(
    data = yrep_data,
    mapping = aes_(
      x = ~ time,
      y = ~ median,
      ymin = ~ lower,
      ymax = ~ upper
    )
  ) +
    geom_ribbon(
      stat = "identity",
      fill = get_color("l"),
      color = get_color("lh"),
      alpha = alpha,
      size = size
    ) +
    geom_line(
      size = size,
      color = get_color("lh")
    )

  y_style <- if (length(y_style) == 2) "both" else y_style
  if (y_style %in% c("both", "lines"))
    graph <- graph +
      geom_line(
        data = y_data,
        color = get_color("dh"), # "black",
        size = ifelse(y_style == "both", 0.25, 0.5)
      )

  if (y_style %in% c("both", "points"))
    graph <- graph +
      geom_point(
        data = y_data,
        shape = 21,
        fill = get_color("d"),
        color = get_color("dh"),
        size = ifelse(y_style == "both", 1, 1.5)
      )

  if (grouped) {
    facet_args[["facets"]] <- "group"
    if (is.null(facet_args[["scales"]]))
      facet_args[["scales"]] <- "free"
    graph <- graph +
      do.call("facet_wrap", facet_args)
  }

  graph +
    labs(x = "Time", y = yrep_label()) +
    theme_default()
}
