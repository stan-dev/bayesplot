#' PPD intervals
#'
#' Medians and central interval estimates of posterior or prior predictive
#' distributions. Each of these functions makes the same plot as the
#' corresponding [`ppc_`][PPC-intervals] function but without plotting any
#' observed data `y`. The **Plot Descriptions** section at [PPC-intervals] has
#' details on the individual plots.
#'
#' @name PPD-intervals
#' @family PPDs
#'
#' @template args-ypred
#' @inheritParams PPC-intervals
#'
#' @template return-ggplot-or-data
#'
#' @template reference-vis-paper
#'
#' @examples
#' color_scheme_set("brightblue")
#' ypred <- example_yrep_draws()
#' x <- example_x_data()
#' group <- example_group_data()
#'
#' ppd_intervals(ypred[, 1:50])
#' ppd_intervals(ypred[, 1:50], fatten = 0)
#' ppd_intervals(ypred[, 1:50], fatten = 0, linewidth = 2)
#' ppd_intervals(ypred[, 1:50], prob_outer = 0.75, fatten = 0, linewidth = 2)
#'
#' # put a predictor variable on the x-axis
#' ppd_intervals(ypred[, 1:100], x = x[1:100], fatten = 1) +
#'   ggplot2::labs(y = "Prediction", x = "Some variable of interest")
#'
#' # with a grouping variable too
#' ppd_intervals_grouped(
#'   ypred = ypred[, 1:100],
#'   x = x[1:100],
#'   group = group[1:100],
#'   size = 2,
#'   fatten = 0,
#'   facet_args = list(nrow = 2)
#' )
#'
#' # even reducing size, ppd_intervals is too cluttered when there are many
#' # observations included (ppd_ribbon is better)
#' ppd_intervals(ypred, size = 0.5, fatten = 0.1, linewidth = 0.5)
#' ppd_ribbon(ypred)
#' ppd_ribbon(ypred, size = 0) # remove line showing median prediction
#'
NULL

#' @rdname PPD-intervals
#' @export
ppd_intervals <-
  function(ypred,
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


    data <- ppd_intervals_data(
      ypred = ypred,
      x = x,
      group = dots$group,
      prob = prob,
      prob_outer = prob_outer
    )
    ggplot(data, mapping = intervals_inner_aes(
      needs_y = TRUE,
      color = "ypred",
      fill = "ypred"
    )) +
      geom_linerange(
        mapping = intervals_outer_aes(color = "ypred"),
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
      scale_color_ppd() +
      scale_fill_ppd() +
      intervals_axis_labels(has_x = !is.null(x)) +
      bayesplot_theme_get() +
      legend_none()
  }


#' @rdname PPD-intervals
#' @export
ppd_intervals_grouped <-
  function(ypred,
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
    g <- eval(ungroup_call("ppd_intervals", call), parent.frame())
    g +
      intervals_group_facets(facet_args) +
      force_axes_in_facets()
  }


#' @rdname PPD-intervals
#' @export
ppd_ribbon <-
  function(ypred,
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

    data <- ppd_intervals_data(
      ypred = ypred,
      x = x,
      group = dots$group,
      prob = prob,
      prob_outer = prob_outer
    )
    ggplot(data, mapping = intervals_inner_aes(color = "ypred", fill = "ypred")) +
      geom_ribbon(
        mapping = intervals_outer_aes(fill = "ypred", color = "ypred"),
        color = NA,
        size = 0.2 * size,
        alpha = alpha
      ) +
      geom_ribbon(
        mapping = intervals_outer_aes(),
        fill = NA,
        color = get_color("mh"),
        size = 0.2 * size,
        alpha = 1
      ) +
      geom_ribbon(size = 0.5 * size) +
      geom_line(
        mapping = aes(y = .data$m),
        color = get_color("d"),
        linewidth = size
      ) +
      scale_color_ppd() +
      scale_fill_ppd() +
      intervals_axis_labels(has_x = !is.null(x)) +
      bayesplot_theme_get() +
      legend_none()
  }


#' @export
#' @rdname PPD-intervals
ppd_ribbon_grouped <-
  function(ypred,
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
    g <- eval(ungroup_call("ppd_ribbon", call), parent.frame())
    g +
      intervals_group_facets(facet_args) +
      force_axes_in_facets()
  }


#' @rdname PPD-intervals
#' @export
ppd_intervals_data <-
  function(ypred,
           x = NULL,
           group = NULL,
           ...,
           prob = 0.5,
           prob_outer = 0.9) {
    check_ignored_arguments(...)

    ypred <- validate_predictions(ypred)
    x <- validate_x(x, ypred[1,])
    if (!is.null(group)) {
      group <- validate_group(group, ncol(ypred))
    }
    .ppd_intervals_data(
      predictions = ypred,
      y = NULL,
      x = x,
      group = group,
      prob = prob,
      prob_outer = prob_outer
    )
  }


#' @rdname PPD-intervals
#' @export
ppd_ribbon_data <- ppd_intervals_data



# internal ----------------------------------------------------------------

#' Back end for both `ppd_intervals_data()` and `ppc_intervals_data()`
#'
#' @noRd
#' @param predictions SxN matrix of predictions (`ypred` or `yrep`) already validated.
#' @param y `NULL` or user's `y` argument already validated.
#' @param group `NULL` or user's `group` argument, already validated.
#' @param x User's `x` argument, already validated.
#' @return A molten data frame of prediction intervals, possibly including `y`.
#'
#' @importFrom dplyr group_by ungroup summarise
.ppd_intervals_data <-
  function(predictions,
           y = NULL,
           x = NULL,
           group = NULL,
           prob,
           prob_outer) {
    stopifnot(prob > 0 && prob < 1)
    stopifnot(prob_outer > 0 && prob_outer <= 1)
    probs <- sort(c(prob, prob_outer))
    prob <- probs[1]
    prob_outer <- probs[2]
    alpha <- (1 - probs) / 2
    probs <- sort(c(alpha, 0.5, 1 - alpha))

    has_group <- !is.null(group)
    has_y <- !is.null(y)
    has_x <- !is.null(x)

    long_d <- melt_predictions(predictions)
    if (has_y) {
      long_d$y_obs <- y[long_d$y_id]
    }

    if (!has_x) {
      x <- seq_len(ncol(predictions))
    }
    long_d$x <- x[long_d$y_id]

    if (has_group) {
      long_d$group <- group[long_d$y_id]
    }
    group_by_vars <- syms(c("y_id", if (has_y) "y_obs",
                         if (has_group) "group", "x"))

    long_d %>%
      group_by(!!!group_by_vars) %>%
      summarise(
        outer_width = prob_outer,
        inner_width = prob,
        ll = unname(quantile(.data$value, probs = probs[1])),
        l  = unname(quantile(.data$value, probs = probs[2])),
        m  = unname(quantile(.data$value, probs = probs[3])),
        h  = unname(quantile(.data$value, probs = probs[4])),
        hh = unname(quantile(.data$value, probs = probs[5]))
      ) %>%
      ungroup()
  }


#' Aesthetic mapping for interval and ribbon plots
#'
#' @param needs_y Whether to include `y` in call to `aes()`. Needed for
#'   `geom_pointrange()`.
#' @param ... Aguments to pass to `aes()` other than `x`,`y`,`ymin`,`ymax`.
#' @return Object returned by `aes()`. Always sets at least `x`, `ymin`, `ymax`.
#' @noRd
intervals_inner_aes <- function(needs_y = FALSE, ...) {
  mapping <- aes(
    x = .data$x,
    ymin = .data$l,
    ymax = .data$h,
    ...
  )
  if (!needs_y) {
    return(mapping)
  }
  modify_aes(mapping, y = .data$m)
}
intervals_outer_aes <- function(needs_y = FALSE, ...) {
  mapping <- aes(
    x = .data$x,
    ymin = .data$ll,
    ymax = .data$hh,
    ...
  )
  if (!needs_y) {
    return(mapping)
  }
  modify_aes(mapping, y = .data$m)
}

#' Create the facet layer for grouped interval and ribbon plots
#'
#' @param facet_args User's `facet_args` argument.
#' @param scales_default String to use for `scales` argument to `facet_wrap()`
#'   if not specified by user. Defaults to `"free"`, unlike `facet_wrap()`.
#' @return Object returned by `facet_wrap()`.
#' @noRd
intervals_group_facets <- function(facet_args, scales_default = "free") {
  facet_args[["facets"]] <- "group"
  facet_args[["scales"]] <- facet_args[["scales"]] %||% scales_default
  do.call("facet_wrap", facet_args)
}

#' Set the axis labels for interval and ribbon plots
#'
#' @param has_x Did the user provide an `x` argument (T/F)?
#' @return Object returned by `labs()`. The y-axis label is `NULL` and x-axis
#'   label is either 'x' or 'Index' depending on whether the user supplied `x`.
#' @noRd
intervals_axis_labels <- function(has_x) {
  labs(
    x = if (has_x) expression(italic(x)) else "Data point (index)",
    y = NULL
  )
}
