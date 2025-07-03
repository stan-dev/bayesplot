#' PPD distributions
#'
#' Plot posterior or prior predictive distributions. Each of these functions
#' makes the same plot as the corresponding [`ppc_`][PPC-distributions] function
#' but without plotting any observed data `y`. The **Plot Descriptions** section
#' at [PPC-distributions] has details on the individual plots.
#'
#' @name PPD-distributions
#' @family PPDs
#'
#' @template args-ypred
#' @inheritParams PPC-distributions
#'
#' @template details-binomial
#' @template return-ggplot-or-data
#'
#' @examples
#' # difference between ppd_dens_overlay() and ppc_dens_overlay()
#' color_scheme_set("brightblue")
#' preds <- example_yrep_draws()
#' ppd_dens_overlay(ypred = preds[1:50, ])
#' ppc_dens_overlay(y = example_y_data(), yrep = preds[1:50, ])
#'
NULL

#' @rdname PPD-distributions
#' @export
ppd_data <- function(ypred, group = NULL) {
  ypred <- validate_predictions(ypred)
  if (!is.null(group)) {
    group <- validate_group(group, n_obs = ncol(ypred))
  }
  .ppd_data(predictions = ypred, y = NULL, group = group)
}


#' @rdname PPD-distributions
#' @export
ppd_dens_overlay <-
  function(ypred,
           ...,
           size = 0.25,
           alpha = 0.7,
           trim = FALSE,
           bw = "nrd0",
           adjust = 1,
           kernel = "gaussian",
           n_dens = 1024) {
    check_ignored_arguments(...)

    data <- ppd_data(ypred)
    ggplot(data, mapping = aes(x = .data$value)) +
      overlay_ppd_densities(
        mapping = aes(group = .data$rep_id, color = "ypred"),
        linewidth = size,
        alpha = alpha,
        trim = trim,
        bw = bw,
        adjust = adjust,
        kernel = kernel,
        n = n_dens
      ) +
      scale_color_ppd(
        values = get_color("m"),
        guide = guide_legend( # in case user turns legend back on
          override.aes = list(size = 2 * size, alpha = 1))
      ) +
      bayesplot_theme_get() +
      dont_expand_axes() +
      yaxis_title(FALSE) +
      xaxis_title(FALSE) +
      yaxis_text(FALSE) +
      yaxis_ticks(FALSE) +
      legend_none()
  }


#' @rdname PPD-distributions
#' @export
ppd_ecdf_overlay <-
  function(ypred,
           ...,
           discrete = FALSE,
           pad = TRUE,
           size = 0.25,
           alpha = 0.7) {
    check_ignored_arguments(...)

    data <- ppd_data(ypred)
    ggplot(data, mapping = aes(x = .data$value)) +
      hline_at(
        c(0, 0.5, 1),
        linewidth = c(0.2, 0.1, 0.2),
        linetype = 2,
        color = get_color("dh")
      ) +
      stat_ecdf(
        mapping = aes(group = .data$rep_id, color = "ypred"),
        geom = if (discrete) "step" else "line",
        linewidth = size,
        alpha = alpha,
        pad = pad
      ) +
      scale_color_ppd(
        values = get_color("m"),
        guide = guide_legend( # in case user turns legend back on
          override.aes = list(linewidth = 2 * size, alpha = 1))
      ) +
      scale_y_continuous(breaks = c(0, 0.5, 1)) +
      bayesplot_theme_get() +
      yaxis_title(FALSE) +
      xaxis_title(FALSE) +
      legend_none()
  }


#' @rdname PPD-distributions
#' @export
ppd_dens <-
  function(ypred,
           ...,
           trim = FALSE,
           size = 0.5,
           alpha = 1) {
    check_ignored_arguments(...)

    data <- ppd_data(ypred)
    ggplot(data, mapping = aes(
      x = .data$value,
      color = "ypred",
      fill = "ypred"
    )) +
      geom_density(
        linewidth = size,
        alpha = alpha,
        trim = trim
      ) +
      scale_color_ppd() +
      scale_fill_ppd() +
      bayesplot_theme_get() +
      facet_wrap_parsed("rep_label") +
      force_axes_in_facets() +
      dont_expand_y_axis() +
      legend_none() +
      yaxis_text(FALSE) +
      yaxis_title(FALSE) +
      yaxis_ticks(FALSE) +
      xaxis_title(FALSE) +
      facet_text(FALSE)
  }


#' @rdname PPD-distributions
#' @export
ppd_hist <-
  function(ypred,
           ...,
           binwidth = NULL,
           bins = NULL,
           breaks = NULL,
           freq = TRUE) {
    check_ignored_arguments(...)

    data <- ppd_data(ypred)
    ggplot(data, mapping = set_hist_aes(
      freq,
      color = "ypred",
      fill = "ypred"
    )) +
      geom_histogram(
        linewidth = 0.25,
        binwidth = binwidth,
        bins = bins,
        breaks = breaks
      ) +
      scale_color_ppd() +
      scale_fill_ppd() +
      bayesplot_theme_get() +
      facet_wrap_parsed("rep_label") +
      force_axes_in_facets() +
      dont_expand_y_axis() +
      legend_none() +
      yaxis_text(FALSE) +
      yaxis_title(FALSE) +
      yaxis_ticks(FALSE) +
      xaxis_title(FALSE) +
      facet_text(FALSE)
  }

#' @rdname PPD-distributions
#' @export
ppd_dots <-
  function(ypred,
           ...,
           binwidth = NA,
           quantiles = NA,
           freq = TRUE) {
    check_ignored_arguments(..., ok_args = c("dotsize", "layout", "stackratio", "overflow"))

    suggested_package("ggdist")

    data <- ppd_data(ypred)
    ggplot(data, mapping = set_hist_aes(
      freq,
      color = "ypred",
      fill = "ypred"
    )) +
      ggdist::stat_dots(
        binwidth = binwidth,
        quantiles = quantiles,
        ...
      ) +
      scale_color_ppd() +
      scale_fill_ppd() +
      bayesplot_theme_get() +
      facet_wrap_parsed("rep_label") +
      force_axes_in_facets() +
      dont_expand_y_axis() +
      legend_none() +
      yaxis_text(FALSE) +
      yaxis_title(FALSE) +
      yaxis_ticks(FALSE) +
      xaxis_title(FALSE) +
      facet_text(FALSE)
  }


#' @rdname PPD-distributions
#' @export
ppd_freqpoly <-
  function(ypred,
           ...,
           binwidth = NULL,
           bins = NULL,
           freq = TRUE,
           size = 0.5,
           alpha = 1) {

    dots <- list(...)
    if (!from_grouped(dots)) {
      check_ignored_arguments(...)
      dots$group <- NULL
    }

    data <- ppd_data(ypred, group = dots$group)
    ggplot(data, mapping = set_hist_aes(
      freq,
      color = "ypred",
      fill = "ypred"
    )) +
      geom_area(
        stat = "bin",
        binwidth = binwidth,
        bins = bins,
        size = size,
        alpha = alpha
      ) +
      facet_wrap_parsed("rep_label") +
      scale_color_ppd() +
      scale_fill_ppd() +
      bayesplot_theme_get() +
      force_axes_in_facets() +
      dont_expand_y_axis() +
      yaxis_text(FALSE) +
      yaxis_title(FALSE) +
      yaxis_ticks(FALSE) +
      xaxis_title(FALSE) +
      facet_text(FALSE) +
      legend_none()
  }


#' @rdname PPD-distributions
#' @export
ppd_freqpoly_grouped <-
  function(ypred,
           group,
           ...,
           binwidth = NULL,
           bins = NULL,
           freq = TRUE,
           size = 0.5,
           alpha = 1) {

    check_ignored_arguments(...)
    call <- match.call(expand.dots = FALSE)
    g <- eval(ungroup_call("ppd_freqpoly", call), parent.frame())
    g +
      facet_grid(
        rep_label ~ group,
        scales = "free",
        labeller = label_parsed
      ) +
      force_axes_in_facets() +
      facet_text() +
      theme(strip.text.y = element_blank())
  }


#' @rdname PPD-distributions
#' @export
ppd_boxplot <-
  function(ypred,
           ...,
           notch = TRUE,
           size = 0.5,
           alpha = 1) {
    check_ignored_arguments(...)

    data <- ppd_data(ypred)
    ggplot(data, mapping = aes(
      x = .data$rep_label,
      y = .data$value,
      color = "ypred",
      fill = "ypred"
    )) +
      geom_boxplot(
        notch = notch,
        linewidth = size,
        alpha = alpha,
        outlier.color = get_color("lh"),
        outlier.alpha = 2/3,
        outlier.size = 1
      ) +
      scale_color_ppd() +
      scale_fill_ppd() +
      scale_x_discrete(labels = function(x) parse(text=x)) +
      bayesplot_theme_get() +
      yaxis_title(FALSE) +
      xaxis_ticks(FALSE) +
      xaxis_text(FALSE) +
      xaxis_title(FALSE) +
      legend_none()
  }


# internal ----------------------------------------------------------------

#' Back end for both `ppd_data()` and `ppc_data()`
#'
#' @noRd
#' @param predictions SxN matrix of predictions (`ypred` or `yrep`) already validated.
#' @param y User's `y` argument (if applicable), already validated.
#' @param group User's `group` argument, already validated.
#' @return A molten data frame of predictions, possible including `y`.
#' @importFrom dplyr left_join select
.ppd_data <- function(predictions, y = NULL, group = NULL) {
  if (!is.null(y)) {
    data <- melt_and_stack(y, predictions)
  } else {
    data <- melt_predictions(predictions)
    levels(data$rep_label) <- gsub("rep", "pred", levels(data$rep_label))
  }
  if (!is.null(group)) {
    group_indices <- tibble::tibble(group, y_id = seq_along(group))
    data <- data %>%
      left_join(group_indices, by = "y_id") %>%
      select("group", tidyselect::everything())
  }
  data
}

#' Wrapper for stat_density with some argument defaults changed
#'
#' This function is called internally by `ppd_dens_overlay()` and
#' `ppc_dens_overlay()`.
#'
#' @param geom,position Arguments passed to [ggplot2::stat_density()] but
#'   with different defaults.
#' @param ... All arguments other than `geom` and `position` to pass to
#'   `stat_density()`. The defaults will be the same as for `stat_density()`.
#' @return Object returned by `stat_density()`.
#' @noRd
overlay_ppd_densities <-
  function(...,
           geom = "line",
           position = "identity") {
    stat_density(..., geom = geom, position = position)
  }

