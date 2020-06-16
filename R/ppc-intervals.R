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
#' @param alpha,size,fatten Arguments passed to geoms. For ribbon plots `alpha`
#'   is passed to [ggplot2::geom_ribbon()] to control the opacity of the outer
#'   ribbon and `size` is passed to [ggplot2::geom_line()] to control the size
#'   of the line representing the median prediction (`size=0` will remove the
#'   line). For interval plots `alpha`, `size` and `fatten` are passed to
#'   [ggplot2::geom_pointrange()] (`fatten=0` will remove the point estimates).
#' @param jitter For intervals plots, either `NULL` (the default) or a value to
#'   pass to the `width` argument of [ggplot2::position_jitter()]. This is most
#'   useful if values in `x` are not unique.
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
#'    `100*prob`\% central intervals for `yrep` at each `x`
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
#' ppc_ribbon_grouped(y, yrep, x = year, group) +
#'   ggplot2::scale_x_continuous(breaks = pretty)
#'
#' ppc_ribbon_grouped(y, yrep, x = year, group,
#'                    facet_args = list(scales = "fixed")) +
#'  xaxis_text(FALSE) +
#'  xaxis_ticks(FALSE) +
#'  panel_bg(fill = "gray20")
#'
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
           jitter = NULL) {

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

    # create object for reproducible jitter
    position <- intervals_position(jitter)

    ggplot(data) +
      intervals_inner_aes(
        needs_y = TRUE,
        color = "yrep",
        fill = "yrep"
      ) +
      geom_linerange(
        mapping = intervals_outer_aes(color = "yrep"),
        position = position,
        alpha = alpha,
        size = size
      ) +
      geom_pointrange(
        position = position,
        shape = 21,
        stroke = 0.5,
        size = size,
        fatten = fatten
      ) +
      geom_point(
        mapping = aes_(
          y = ~ y_obs,
          color = "y",
          fill = "y"
        ),
        position = position,
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
           jitter = NULL) {
    check_ignored_arguments(...)
    call <- match.call(expand.dots = FALSE)
    g <- eval(ungroup_call("ppc_intervals", call), parent.frame())
    g +
      intervals_group_facets(facet_args) +
      force_axes_in_facets()
  }


#' @rdname PPC-intervals
#' @export
ppc_ribbon <-
  function(y,
           yrep,
           x = NULL,
           ...,
           prob = 0.5,
           prob_outer = 0.9,
           alpha = 0.33,
           size = 0.25) {

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
      intervals_inner_aes(fill = "yrep", color = "yrep") +
      geom_ribbon(
        mapping = intervals_outer_aes(fill = "yrep", color = "yrep"),
        color = NA,
        size = 0.2 * size,
        alpha = alpha
      ) +
      geom_ribbon(
        mapping = intervals_outer_aes(),
        fill = NA,
        color = get_color("m"),
        size = 0.2 * size,
        alpha = 1
      ) +
      geom_ribbon(size = 0.5 * size) +
      geom_line(
        mapping = aes_(y = ~ m),
        color = get_color("m"),
        size = size
      ) +
      geom_blank(aes_(fill = "y")) +
      geom_line(
        aes_(y = ~ y_obs, color = "y"),
        size = 0.5,
        alpha = 0.5
      ) +
      scale_color_ppc() +
      scale_fill_ppc(values = c(NA, get_color("l"))) +
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
           size = 0.25) {
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

