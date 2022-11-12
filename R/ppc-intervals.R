

#' PPC intervals
#'
#' Medians and central interval estimates of `yrep` with `y` overlaid.
#' See the **Plot Descriptions** section, below.
#'
#' @name PPC-intervals
#' @family PPCs
#'
#' @template args-y-yrep
#' @template args-group
#' @template args-facet_args
#' @template args-prob-prob_outer
#' @param x A numeric vector to use as the x-axis
#'   variable. For example, `x` could be a predictor variable from a
#'   regression model, a time variable for time-series models, etc. If `x`
#'   is missing or `NULL` then the observation index is used for the x-axis.
#' @param alpha,size,fatten,linewidth Arguments passed to geoms. For ribbon
#'   plots `alpha` is passed to [ggplot2::geom_ribbon()] to control the opacity
#'   of the outer ribbon and `size` is passed to [ggplot2::geom_line()] to
#'   control the size of the line representing the median prediction (`size=0`
#'   will remove the line). For interval plots `alpha`, `size`, `fatten`, and
#'   `linewidth` are passed to [ggplot2::geom_pointrange()] (`fatten=0` will
#'   remove the point estimates).
#' @param ... Currently unused.
#'
#' @template return-ggplot-or-data
#'
#' @template reference-vis-paper
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{`ppc_intervals(), ppc_ribbon()`}{
#'    `100*prob`% central intervals for `yrep` at each `x`
#'    value. `ppc_intervals()` plots intervals as vertical bars with points
#'    indicating `yrep` medians and darker points indicating observed
#'    `y` values. `ppc_ribbon()` plots a ribbon of connected intervals
#'    with a line through the median of `yrep` and a darker line connecting
#'    observed `y` values. In both cases an optional `x` variable can
#'    also be specified for the x-axis variable.
#'
#'    Depending on the number of observations and the variability in the
#'    predictions at different values of `x`, one of these plots may be easier
#'    to read than the other.
#'   }
#'   \item{`ppc_intervals_grouped(), ppc_ribbon_grouped()`}{
#'    Same as `ppc_intervals()` and `ppc_ribbon()`, respectively, but a
#'    separate plot (facet) is generated for each level of a grouping variable.
#'   }
#' }
#'
#' @examples
#' y <- rnorm(50)
#' yrep <- matrix(rnorm(5000, 0, 2), ncol = 50)
#'
#' color_scheme_set("brightblue")
#' ppc_intervals(y, yrep)
#' ppc_ribbon(y, yrep)
#' ppc_ribbon(y, yrep, y_draw = "points")
#' \dontrun{
#' ppc_ribbon(y, yrep, y_draw = "both")
#' }
#'
#' ppc_intervals(y, yrep, size = 1.5, fatten = 0) # remove the yrep point estimates
#'
#' color_scheme_set("teal")
#' year <- 1950:1999
#' ppc_intervals(y, yrep, x = year, fatten = 1) + ggplot2::xlab("Year")
#' ppc_ribbon(y, yrep, x = year) + ggplot2::xlab("Year")
#'
#' color_scheme_set("pink")
#' year <- rep(2000:2009, each = 5)
#' group <- gl(5, 1, length = 50, labels = LETTERS[1:5])
#' ppc_ribbon_grouped(y, yrep, x = year, group, y_draw = "both") +
#'   ggplot2::scale_x_continuous(breaks = pretty)
#'
#' ppc_ribbon_grouped(y, yrep, x = year, group,
#'                    facet_args = list(scales = "fixed")) +
#'  xaxis_text(FALSE) +
#'  xaxis_ticks(FALSE) +
#'  panel_bg(fill = "gray20")
#'
#' # get the data frames used to make the ggplots
#' ppc_dat <- ppc_intervals_data(y, yrep, x = year, prob = 0.5)
#' ppc_group_dat <- ppc_intervals_data(y, yrep, x = year, group = group, prob = 0.5)
#'
#' \dontrun{
#' library("rstanarm")
#' fit <- stan_glmer(mpg ~ wt + (1|cyl), data = mtcars, refresh = 0)
#' yrep <- posterior_predict(fit)
#'
#' color_scheme_set("purple")
#' ppc_intervals(y = mtcars$mpg, yrep = yrep, x = mtcars$wt, prob = 0.8) +
#'  panel_bg(fill="gray90", color = NA) +
#'  grid_lines(color = "white")
#'
#' ppc_ribbon(y = mtcars$mpg, yrep = yrep, x = mtcars$wt,
#'            prob = 0.6, prob_outer = 0.8)
#'
#' ppc_ribbon_grouped(y = mtcars$mpg, yrep = yrep, x = mtcars$wt,
#'                    group = mtcars$cyl)
#'
#'
#' color_scheme_set("gray")
#' ppc_intervals(mtcars$mpg, yrep, prob = 0.5) +
#'  ggplot2::scale_x_continuous(
#'    labels = rownames(mtcars),
#'    breaks = 1:nrow(mtcars)
#'  ) +
#'  xaxis_text(angle = -70, vjust = 1, hjust = 0) +
#'  xaxis_title(FALSE)
#'
#' }
#'
#'
NULL

#' @rdname PPC-intervals
#' @export
ppc_intervals <-
  function(y,
           yrep,
           x = NULL,
           ...,
           prob = 0.5,
           prob_outer = 0.9,
           alpha = 0.33,
           size = 1,
           fatten = 2.5,
           linewidth = 1) {

    dots <- list(...)
    if (!from_grouped(dots)) {
      check_ignored_arguments(...)
      dots$group <- NULL
    }

    data <-
      ppc_intervals_data(
        y = y,
        yrep = yrep,
        x = x,
        group = dots$group,
        prob = prob,
        prob_outer = prob_outer
      )

    ggplot(data) +
      intervals_inner_aes(
        needs_y = TRUE,
        color = "yrep",
        fill = "yrep"
      ) +
      geom_linerange(
        mapping = intervals_outer_aes(color = "yrep"),
        alpha = alpha,
        size = size,
        linewidth = linewidth
      ) +
      geom_pointrange(
        shape = 21,
        stroke = 0.5,
        size = size,
        fatten = fatten,
        linewidth = linewidth
      ) +
      geom_point(
        mapping = aes(
          y = .data$y_obs,
          color = "y",
          fill = "y"
        ),
        shape = 21,
        stroke = 0.5,
        size = 1
      ) +
      scale_color_ppc() +
      scale_fill_ppc() +
      intervals_axis_labels(has_x = !is.null(x)) +
      bayesplot_theme_get()
  }


#' @rdname PPC-intervals
#' @export
ppc_intervals_grouped <-
  function(y,
           yrep,
           x = NULL,
           group,
           ...,
           facet_args = list(),
           prob = 0.5,
           prob_outer = 0.9,
           alpha = 0.33,
           size = 1,
           fatten = 2.5,
           linewidth = 1) {
    check_ignored_arguments(...)
    call <- match.call(expand.dots = FALSE)
    g <- eval(ungroup_call("ppc_intervals", call), parent.frame())
    g +
      intervals_group_facets(facet_args) +
      force_axes_in_facets()
  }


#' @rdname PPC-intervals
#' @export
#' @param y_draw For ribbon plots only, a string specifying how to draw `y`. Can
#'   be `"line"` (the default), `"points"`, or `"both"`.
ppc_ribbon <-
  function(y,
           yrep,
           x = NULL,
           ...,
           prob = 0.5,
           prob_outer = 0.9,
           alpha = 0.33,
           size = 0.25,
           y_draw = c("line", "points", "both")) {

    y_draw <- match.arg(y_draw)
    dots <- list(...)
    if (!from_grouped(dots)) {
      check_ignored_arguments(...)
      dots$group <- NULL
    }

    data <-
      ppc_intervals_data(
        y = y,
        yrep = yrep,
        x = x,
        group = dots$group,
        prob = prob,
        prob_outer = prob_outer
      )

    g <- ggplot(data) +
      intervals_inner_aes(fill = "yrep", color = "yrep") +
      geom_ribbon(
        mapping = intervals_outer_aes(fill = "yrep", color = "yrep"),
        color = NA,
        linewidth = 0.2 * size,
        alpha = alpha
      ) +
      geom_ribbon(
        mapping = intervals_outer_aes(),
        fill = NA,
        color = get_color("m"),
        linewidth = 0.2 * size,
        alpha = 1
      ) +
      geom_ribbon(linewidth = 0.5 * size) +
      geom_line(
        mapping = aes(y = .data$m),
        color = get_color("m"),
        linewidth = size
      ) +
      geom_blank(aes(fill = "y"))

    if (y_draw == "line" || y_draw == "both") {
      g <- g + geom_line(
        aes(y = .data$y_obs, color = "y"),
        linewidth = 0.5
      )
    }

    if (y_draw == "points" || y_draw == "both") {
      g <- g + geom_point(
        mapping = aes(y = .data$y_obs, color = "y", fill = "y"),
        shape = 21,
        size = 1.5
      )
    }
    g +
      scale_color_ppc() +
      scale_fill_ppc(values = c(NA, get_color("l")), na.value = NA) +
      intervals_axis_labels(has_x = !is.null(x)) +
      bayesplot_theme_get()
  }


#' @export
#' @rdname PPC-intervals
ppc_ribbon_grouped <-
  function(y,
           yrep,
           x = NULL,
           group,
           ...,
           facet_args = list(),
           prob = 0.5,
           prob_outer = 0.9,
           alpha = 0.33,
           size = 0.25,
           y_draw = c("line", "points", "both")) {
    check_ignored_arguments(...)
    call <- match.call(expand.dots = FALSE)
    g <- eval(ungroup_call("ppc_ribbon", call), parent.frame())
    g +
      intervals_group_facets(facet_args) +
      force_axes_in_facets()
  }


#' @rdname PPC-intervals
#' @export
ppc_intervals_data <-
  function(y,
           yrep,
           x = NULL,
           group = NULL,
           ...,
           prob = 0.5,
           prob_outer = 0.9) {
    check_ignored_arguments(...)

    y <- validate_y(y)
    yrep <- validate_predictions(yrep, length(y))
    x <- validate_x(x, y)
    if (!is.null(group)) {
      group <- validate_group(group, length(y))
    }
    .ppd_intervals_data(
      predictions = yrep,
      y = y,
      x = x,
      group = group,
      prob = prob,
      prob_outer = prob_outer
    )
  }


#' @rdname PPC-intervals
#' @export
ppc_ribbon_data <- ppc_intervals_data
