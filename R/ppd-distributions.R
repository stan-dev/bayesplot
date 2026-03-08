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
#' @param show_marginal Plot the marginal PPD along with the yreps.
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
           show_marginal = FALSE,
           ...,
           size = 0.25,
           alpha = 0.7,
           trim = FALSE,
           bw = "nrd0",
           adjust = 1,
           kernel = "gaussian",
           bounds = NULL,
           n_dens = 1024) {
    check_ignored_arguments(...)
    bounds <- validate_density_bounds(bounds)

    data <- ppd_data(ypred)
    p <- ggplot(data, mapping = aes(x = .data$value)) +
      overlay_ppd_densities(
        mapping = aes(group = .data$rep_id, color = "ypred"),
        linewidth = size,
        alpha = alpha,
        trim = trim,
        bw = bw,
        adjust = adjust,
        kernel = kernel,
        bounds = bounds,
        n = n_dens
      ) +
      bayesplot_theme_get() +
      dont_expand_axes() +
      yaxis_title(FALSE) +
      xaxis_title(FALSE) +
      yaxis_text(FALSE) +
      yaxis_ticks(FALSE)

    if (isTRUE(show_marginal)) {
      p +
        overlay_ppd_densities(
          mapping = aes(color = "marginal"),
          linewidth = 1,
          trim = trim,
          bw = bw,
          adjust = adjust,
          kernel = kernel,
          bounds = bounds,
          n = n_dens
        ) +
        scale_color_ppd(
          labels = ypred_label(show_marginal = TRUE),
          values = get_color(c("d", "m")),
          guide = guide_legend(
            override.aes = list(size = 2 * size, alpha = 1))
        )
    } else {
      p + scale_color_ppd(
        values = get_color("m"),
        # in case user turns legend back on
        guide = guide_legend(
          override.aes = list(size = 2 * size, alpha = 1))
      ) +
        legend_none()
    }

  }


#' @rdname PPD-distributions
#' @export
ppd_ecdf_overlay <-
  function(ypred,
           show_marginal= FALSE,
           ...,
           discrete = FALSE,
           pad = TRUE,
           size = 0.25,
           alpha = 0.7) {
    check_ignored_arguments(...)

    data <- ppd_data(ypred)
    p <- ggplot(data, mapping = aes(x = .data$value)) +
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
      scale_y_continuous(breaks = c(0, 0.5, 1)) +
      bayesplot_theme_get() +
      yaxis_title(FALSE) +
      xaxis_title(FALSE)

    if (isTRUE(show_marginal)) {
      p +
        stat_ecdf(
          mapping = aes(color = "PPD"),
          geom = if (discrete) "step" else "line",
          linewidth = 1,
          pad = pad
        ) +
        scale_color_ppd(
          labels = ypred_label(show_marginal = TRUE),
          values = get_color(c("d", "m")),
          guide = guide_legend(
            override.aes = list(size = 2 * size, alpha = 1))
        )
    } else {
      p +
        scale_color_ppd(
          values = get_color("m"),
          guide = guide_legend( # in case user turns legend back on
            override.aes = list(linewidth = 2 * size, alpha = 1))
        ) +
        legend_none()
    }
  }


#' @rdname PPD-distributions
#' @export
ppd_dens <-
    function(ypred,
             show_marginal = FALSE,
             ...,
             trim = FALSE,
             size = 0.5,
             alpha = 1,
             bounds = NULL) {
      check_ignored_arguments(...)
      bounds <- validate_density_bounds(bounds)

      data <- ppd_data(ypred)
      p <- ggplot(data, mapping = aes(.data$value)) +
        geom_density(
          aes(color = "ypred",
              fill = "ypred"),
          linewidth = size,
          alpha = alpha,
          trim = trim,
          bounds = bounds
        ) +
        bayesplot_theme_get() +
        facet_wrap_parsed("rep_label") +
        force_axes_in_facets() +
        dont_expand_y_axis() +
        yaxis_text(FALSE) +
        yaxis_title(FALSE) +
        yaxis_ticks(FALSE) +
        xaxis_title(FALSE) +
        facet_text(FALSE)

      if (isTRUE(show_marginal)) {
        data2 <- transform(data, rep_label = "PPD")

        p +
          geom_density(
            aes(color = "PPD",
                fill = "PPD"),
            linewidth = 1,
            trim = trim,
            bounds = bounds,
            data = data2
          ) +
          scale_color_ppd(
            labels = ypred_label(show_marginal = TRUE),
            values = get_color(c("d", "m")),
            guide = guide_legend(
              override.aes = list(size = 2 * size, alpha = 1))
          ) +
          scale_fill_ppd(
            labels = ypred_label(show_marginal = TRUE),
            values = get_color(c("d", "m")),
            guide = guide_legend(
              override.aes = list(size = 2 * size, alpha = 1))
          )
      } else {
        p +
          scale_color_ppd() +
          scale_fill_ppd() +
          legend_none()
      }
    }


#' @rdname PPD-distributions
#' @export
ppd_hist <-
  function(ypred,
           show_marginal = FALSE,
           ...,
           binwidth = NULL,
           bins = NULL,
           breaks = NULL,
           freq = !show_marginal) {
    check_ignored_arguments(...)

    data <- ppd_data(ypred)
    p <- ggplot(data, mapping = set_hist_aes(freq)) +
      geom_histogram(
        aes(color = "ypred",
            fill = "ypred"),
        linewidth = 0.25,
        binwidth = binwidth,
        bins = bins,
        breaks = breaks
      ) +
      bayesplot_theme_get() +
      facet_wrap_parsed("rep_label") +
      force_axes_in_facets() +
      dont_expand_y_axis() +
      yaxis_text(FALSE) +
      yaxis_title(FALSE) +
      yaxis_ticks(FALSE) +
      xaxis_title(FALSE) +
      facet_text(FALSE)

    if (isTRUE(show_marginal)) {
      data2 <- transform(data, rep_label = "PPD")

      p +
        geom_histogram(
          aes(color = "PPD",
              fill = "PPD"),
          linewidth = 1,
          binwidth = binwidth,
          bins = bins,
          breaks = breaks,
          data = data2
        ) +
        scale_color_ppd(
          labels = ypred_label(show_marginal = TRUE),
          values = get_color(c("d", "m"))
        ) +
        scale_fill_ppd(
          labels = ypred_label(show_marginal = TRUE),
          values = get_color(c("d", "m"))
        )

    } else {
      p +
        scale_color_ppd() +
        scale_fill_ppd() +
        legend_none()
    }
  }

#' @rdname PPD-distributions
#' @export
ppd_dots <-
  function(ypred,
           show_marginal = FALSE,
           ...,
           binwidth = NA,
           quantiles = 100,
           freq = TRUE) {
    check_ignored_arguments(..., ok_args = c("dotsize", "layout", "stackratio", "overflow"))

    suggested_package("ggdist")

    data <- ppd_data(ypred)

    p <- ggplot(data, mapping = set_hist_aes(freq)) +
      ggdist::stat_dots(
        aes(color = "ypred",
            fill = "ypred"),
        binwidth = binwidth,
        quantiles = quantiles,
        ...
      ) +
      bayesplot_theme_get() +
      facet_wrap_parsed("rep_label") +
      force_axes_in_facets() +
      dont_expand_y_axis() +
      yaxis_text(FALSE) +
      yaxis_title(FALSE) +
      yaxis_ticks(FALSE) +
      xaxis_title(FALSE) +
      facet_text(FALSE)

    if (isTRUE(show_marginal)) {
      data2 <- transform(data, rep_label = "PPD")

      p +
        ggdist::stat_dots(
          aes(color = "PPD",
              fill = "PPD"),
          data = data2,
          binwidth = binwidth,
          quantiles = quantiles,
          ...
        ) +
        scale_color_ppd(
          labels = ypred_label(show_marginal = TRUE),
          values = get_color(c("d", "m"))
        ) +
        scale_fill_ppd(
          labels = ypred_label(show_marginal = TRUE),
          values = get_color(c("d", "m"))
        )

    } else {
      p +
        scale_color_ppd() +
        scale_fill_ppd() +
        legend_none()
    }
  }


#' @rdname PPD-distributions
#' @export
ppd_freqpoly <-
  function(ypred,
           show_marginal = FALSE,
           ...,
           binwidth = NULL,
           bins = NULL,
           freq = !show_marginal,
           size = 0.5,
           alpha = 1) {

    dots <- list(...)
    if (!from_grouped(dots)) {
      check_ignored_arguments(...)
      dots$group <- NULL
    }

    data <- ppd_data(ypred, group = dots$group)
    p <- ggplot(data, mapping = set_hist_aes(freq)) +
      geom_area(
        aes(color = "ypred",
            fill = "ypred"),
        stat = "bin",
        binwidth = binwidth,
        bins = bins,
        linewidth = size,
        alpha = alpha
      ) +
      facet_wrap_parsed("rep_label") +
      bayesplot_theme_get() +
      force_axes_in_facets() +
      dont_expand_y_axis() +
      yaxis_text(FALSE) +
      yaxis_title(FALSE) +
      yaxis_ticks(FALSE) +
      xaxis_title(FALSE) +
      facet_text(FALSE)


    if (isTRUE(show_marginal)) {
      data2 <- transform(data, rep_label = "PPD")

      p +
        geom_area(
          aes(color = "PPD",
              fill = "PPD"),
          data = data2,
          stat = "bin",
          binwidth = binwidth,
          bins = bins,
          linewidth = 1,
        ) +
        scale_color_ppd(
          labels = ypred_label(show_marginal = TRUE),
          values = get_color(c("d", "m")),
          guide = guide_legend(override.aes = list(size = 2 * size, alpha = 1))
        ) +
        scale_fill_ppd(
          labels = ypred_label(show_marginal = TRUE),
          values = get_color(c("d", "m")),
          guide = guide_legend(override.aes = list(size = 2 * size, alpha = 1))
        )

    } else {
      p +
        scale_color_ppd() +
        scale_fill_ppd() +
        legend_none()
    }
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

