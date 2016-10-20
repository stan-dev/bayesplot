#' PPC intervals
#'
#' Medians and central interval estimates of \code{yrep} with \code{y} overlaid.
#' See the \strong{Plot Descriptions} section, below. This functionality  will
#' greatly expand in future releases of the package.
#'
#' @name PPC-intervals
#' @family PPCs
#'
#' @template args-y-yrep
#' @param x A numeric vector the same length as \code{y} to use as the x-axis
#'   variable. If \code{x} is missing then \code{1:length(y)} is used for
#'   the x-axis.
#' @param ... Currently unused.
#' @param prob A value between 0 and 1 indicating the desired probability mass
#'   to include in the \code{yrep} intervals. The default is 0.9.
#' @param alpha,size,fatten Arguments passed to geoms. For ribbon plots
#'   \code{alpha} and \code{size} are passed to
#'   \code{\link[ggplot2]{geom_ribbon}}. For interval plots \code{size} and
#'   \code{fatten} are passed to \code{\link[ggplot2]{geom_pointrange}}.
#'
#' @template args-group
#' @param facet_args An optional list of  arguments (other than \code{facets})
#'   passed to \code{\link[ggplot2]{facet_wrap}} to control faceting.
#'
#'
#' @template return-ggplot
#'
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#'
#' @examples
#' y <- rnorm(50)
#' yrep <- matrix(rnorm(5000, 0, 2), ncol = 50)
#' ppc_ribbon(y, yrep)
#' ppc_intervals(y, yrep)
#'
#' color_scheme_set("mix-green-blue")
#' year <- 1950:1999
#' ppc_ribbon(y, yrep, x = year, alpha = 0, size = 0.5) + ggplot2::xlab("Year")
#'
#' color_scheme_set("pink")
#' year <- rep(2000:2009, each = 5)
#' group <- gl(5, 1, length = 50, labels = LETTERS[1:5])
#' ppc_ribbon_grouped(y, yrep, x = year, group) +
#'   ggplot2::scale_x_continuous(breaks = pretty)
#'
#' ppc_ribbon_grouped(
#'  y, yrep, x = time, group,
#'  facet_args = list(scales = "fixed"),
#'  alpha = 1,
#'  size = 2
#' ) +
#'  xaxis_text(FALSE) +
#'  xaxis_ticks(FALSE) +
#'  panel_bg(fill = "gray20")
#'
#'
NULL

#' @rdname PPC-intervals
#' @export
ppc_intervals <- function(y,
                          yrep,
                          x,
                          ...,
                          prob = 0.9,
                          size = 1,
                          fatten = 3) {
  check_ignored_arguments(...)
  y <- validate_y(y)
  plot_data <- ppc_ts_data(
    y = y,
    yrep = validate_yrep(yrep, y),
    time = validate_x(x, y),
    group = NULL,
    prob = prob
  )
  .ppc_intervals(
    plot_data,
    fatten = fatten,
    size = size,
    grouped = FALSE
  ) +
    label_x(x)
}

#' @rdname PPC-intervals
#' @export
ppc_ribbon <- function(y,
                     yrep,
                     x,
                     ...,
                     prob = 0.9,
                     alpha = 0.33,
                     size = 1) {
  check_ignored_arguments(...)

  y <- validate_y(y)
  .ppc_ribbon(
    data = ppc_ts_data(
      y = y,
      yrep = validate_yrep(yrep, y),
      time = validate_x(x, y),
      group = NULL,
      prob = prob
    ),
    alpha = alpha,
    size = size,
    grouped = FALSE
  ) +
    label_x(x)
}

#' @export
#' @rdname PPC-intervals
#' @template args-group
#'
ppc_ribbon_grouped <-
  function(y,
           yrep,
           x,
           group,
           facet_args = list(),
           ...,
           prob = 0.9,
           alpha = 0.33,
           size = 1) {
    check_ignored_arguments(...)

    y <- validate_y(y)
    if (is.null(facet_args[["scales"]]))
      facet_args[["scales"]] <- "free"

    .ppc_ribbon(
      data = ppc_ts_data(
        y = y,
        yrep = validate_yrep(yrep, y),
        time = validate_x(x, y, unique_x = FALSE),
        group = validate_group(group, y),
        prob = prob
      ),
      facet_args = facet_args,
      alpha = alpha,
      size = size,
      grouped = TRUE
    ) +
      label_x(x)
  }


# internal ----------------------------------------------------------------
label_x <- function(x) {
  xlab(if (missing(x)) "Index" else expression(italic(x)))
}

# Make ribbon plot
#
# @param data The object returned by ppc_ts_data
# @return A ggplot object
#
.ppc_ribbon <-
  function(data,
           facet_args = list(),
           alpha = 0.33,
           size = 1,
           grouped = FALSE) {
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
        mapping = aes_(color = "yrep"),
        fill = get_color("l"),
        alpha = alpha,
        size = size
      ) +
      geom_line(
        mapping = aes_(color = "yrep"),
        size = size,
        color = get_color("lh")
      ) +
      geom_line(
        data = y_data,
        mapping = aes_(color = "y"),
        size = 0.5
      ) +
      scale_color_manual(
        name = "",
        values = setNames(get_color(c("lh", "dh")), c("yrep", "y")),
        labels = c(yrep = yrep_label(), y = y_label())
      )

    if (grouped) {
      facet_args[["facets"]] <- "group"
      if (is.null(facet_args[["scales"]]))
        facet_args[["scales"]] <- "free"
      graph <- graph +
        do.call("facet_wrap", facet_args)
    }

    graph +
      labs(x = "x", y = NULL) +
      theme_default()
  }


.ppc_intervals <-
  function(data,
           facet_args = list(),
           size = 1,
           fatten = 3,
           grouped = FALSE) {
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
      geom_pointrange(
        mapping = aes_(color = "yrep", fill = "yrep"),
        shape = 21,
        size = size,
        fatten = fatten
      ) +
      geom_point(
        data = y_data,
        aes_(color = "y", fill = "y"),
        shape = 21,
        size = 1.5
      ) +
      scale_color_manual(
        name = "",
        values = setNames(get_color(c("lh", "dh")), c("yrep", "y")),
        labels = c(yrep = yrep_label(), y = y_label())
      ) +
      scale_fill_manual(
        name = "",
        values = setNames(get_color(c("l", "d")), c("yrep", "y")),
        labels = c(yrep = yrep_label(), y = y_label())
      )

    if (grouped) {
      facet_args[["facets"]] <- "group"
      if (is.null(facet_args[["scales"]]))
        facet_args[["scales"]] <- "free"
      graph <- graph +
        do.call("facet_wrap", facet_args)
    }

    graph +
      labs(x = "x", y = NULL) +
      theme_default()
  }

