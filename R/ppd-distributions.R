#' PPD distributions
#'
#' Plot posterior (or prior) predictive distributions. Each of these functions
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
ppd_dens <-
  function(ypred,
           ...,
           trim = FALSE,
           size = 0.5,
           alpha = 1) {
    check_ignored_arguments(...)
    ypred %>%
      ppd_data() %>%
      ggplot(mapping = aes_(x = ~ value)) +
      geom_density(
        fill = get_color("m"),
        color = get_color("mh"),
        size = size,
        alpha = alpha,
        trim = trim
      ) +
      bayesplot_theme_get() +
      facet_wrap_parsed("rep_label") +
      force_axes_in_facets() +
      dont_expand_y_axis() +
      legend_none() +
      yaxis_text(FALSE) +
      yaxis_title(FALSE) +
      yaxis_ticks(FALSE) +
      xaxis_title(FALSE) +
      facet_text(FALSE) +
      facet_bg(FALSE)
  }

#' @rdname PPD-distributions
#' @export
ppd_hist <-
  function(ypred,
           ...,
           binwidth = NULL,
           breaks = NULL,
           freq = TRUE) {
    check_ignored_arguments(...)
    ypred %>%
      ppd_data() %>%
      ggplot(mapping = set_hist_aes(freq)) +
      geom_histogram(
        fill = get_color("m"),
        color = get_color("mh"),
        size = 0.25,
        binwidth = binwidth,
        breaks = breaks
      ) +
      bayesplot_theme_get() +
      facet_wrap_parsed("rep_label") +
      force_axes_in_facets() +
      dont_expand_y_axis() +
      legend_none() +
      yaxis_text(FALSE) +
      yaxis_title(FALSE) +
      yaxis_ticks(FALSE) +
      xaxis_title(FALSE) +
      facet_text(FALSE) +
      facet_bg(FALSE)
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
    ypred %>%
      ppd_data() %>%
      ggplot(mapping = aes_(x = ~ value)) +
      overlay_ppd_densities(
        mapping = aes_(group = ~ rep_id),
        color = get_color("m"),
        size = size,
        alpha = alpha,
        trim = trim,
        bw = bw,
        adjust = adjust,
        kernel = kernel,
        n = n_dens
      ) +
      bayesplot_theme_get() +
      dont_expand_axes() +
      legend_none() +
      yaxis_title(FALSE) +
      xaxis_title(FALSE) +
      yaxis_text(FALSE) +
      yaxis_ticks(FALSE)
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

    ypred %>%
      ppd_data() %>%
      ggplot(mapping = aes_(x = ~ value)) +
      hline_at(
        c(0, 0.5, 1),
        size = c(0.2, 0.1, 0.2),
        linetype = 2,
        color = get_color("dh")
      ) +
      stat_ecdf(
        mapping = aes_(group = ~ rep_id),
        color = get_color("m"),
        geom = if (discrete) "step" else "line",
        size = size,
        alpha = alpha,
        pad = pad
      ) +
      scale_y_continuous(breaks = c(0, 0.5, 1)) +
      bayesplot_theme_get() +
      yaxis_title(FALSE) +
      xaxis_title(FALSE) +
      yaxis_ticks(FALSE)
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

    ypred %>%
      ppd_data() %>%
      ggplot(mapping = aes_(x = ~ rep_label, y = ~ value)) +
      geom_boxplot(
        fill = get_color("m"),
        color = get_color("mh"),
        notch = notch,
        size = size,
        alpha = alpha,
        outlier.color = get_color("lh")
      ) +
      bayesplot_theme_get() +
      yaxis_title(FALSE) +
      xaxis_ticks(FALSE) +
      xaxis_text(FALSE) +
      xaxis_title(FALSE)
  }

#' @rdname PPD-distributions
#' @export
ppd_freqpoly <-
  function(ypred,
           ...,
           binwidth = NULL,
           freq = TRUE,
           size = 0.5,
           alpha = 1) {
    check_ignored_arguments(...)
    ypred %>%
      ppd_data() %>%
      ggplot(mapping = set_hist_aes(freq)) +
      geom_area(
        stat = "bin",
        color = get_color("mh"),
        fill = get_color("m"),
        binwidth = binwidth,
        size = size,
        alpha = alpha
      ) +
      facet_wrap_parsed("rep_label") +
      bayesplot_theme_get() +
      force_axes_in_facets() +
      dont_expand_y_axis() +
      space_legend_keys() +
      yaxis_text(FALSE) +
      yaxis_title(FALSE) +
      yaxis_ticks(FALSE) +
      xaxis_title(FALSE) +
      facet_text(FALSE) +
      facet_bg(FALSE)
  }

#' @rdname PPD-distributions
#' @export
#' @template args-group
#'
ppd_freqpoly_grouped <-
  function(ypred,
           group,
           ...,
           binwidth = NULL,
           freq = TRUE,
           size = 0.5,
           alpha = 1) {
    check_ignored_arguments(...)
    ypred %>%
      ppd_data(group = group) %>%
      ggplot(mapping = set_hist_aes(freq)) +
      geom_area(
        stat = "bin",
        color = get_color("m"),
        fill = get_color("mh"),
        size = size,
        alpha = alpha,
        binwidth = binwidth,
        na.rm = TRUE
      ) +
      facet_grid(rep_label ~ group, scales = "free") +
      scale_fill_ppc_dist() +
      scale_color_ppc_dist() +
      dont_expand_y_axis(c(0.005, 0)) +
      bayesplot_theme_get() +
      force_axes_in_facets() +
      space_legend_keys() +
      xaxis_title(FALSE) +
      yaxis_text(FALSE) +
      yaxis_ticks(FALSE) +
      yaxis_title(FALSE) +
      facet_bg(FALSE) +
      theme(strip.text.y = element_blank())
  }


# internal ----------------------------------------------------------------

#' Back end for both `ppd_data()` and `ppc_data()`
#'
#' @noRd
#' @param predictions SxN matrix of predictions (`ypred` or `yrep`) already validated.
#' @param y User's `y` argument (if applicable), already validated.
#' @param group User's `group` argument, already validated.
#' @return A molten data frame of predictions, possible including `y`.
#'
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
      select(.data$group, dplyr::everything())
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
#' @noRd
overlay_ppd_densities <-
  function(...,
           geom = "line",
           position = "identity") {
    stat_density(..., geom = geom, position = position)
  }

