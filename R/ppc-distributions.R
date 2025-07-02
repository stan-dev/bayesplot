#' PPC distributions
#'
#' Compare the empirical distribution of the data `y` to the distributions of
#' simulated/replicated data `yrep` from the posterior predictive distribution.
#' See the **Plot Descriptions** section, below, for details.
#'
#' @name PPC-distributions
#' @family PPCs
#'
#' @template args-y-yrep
#' @template args-group
#' @template args-hist
#' @template args-hist-freq
#' @template args-dens
#' @template args-pit-ecdf
#' @param size,alpha Passed to the appropriate geom to control the appearance of
#'   the predictive distributions.
#' @param ... For dot plots, optional additional arguments to pass to [ggdist::stat_dots()].
#'
#' @template details-binomial
#' @template return-ggplot-or-data
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{`ppc_hist(), ppc_freqpoly(), ppc_dens(), ppc_boxplot()`}{
#'    A separate histogram, shaded frequency polygon, smoothed kernel density
#'    estimate, or box and whiskers plot is displayed for `y` and each
#'    dataset (row) in `yrep`. For these plots `yrep` should therefore
#'    contain only a small number of rows. See the **Examples** section.
#'   }
#'   \item{`ppc_dots()`}{
#'    A dot plot plot is displayed for `y` and each dataset (row) in `yrep`.
#'    For these plots `yrep` should therefore contain only a small number of rows.
#'    See the **Examples** section. This function requires [ggdist::stat_dots] to be installed.
#'    }
#'   \item{`ppc_freqpoly_grouped()`}{
#'    A separate frequency polygon is plotted for each level of a grouping
#'    variable for `y` and each dataset (row) in `yrep`. For this plot
#'    `yrep` should therefore contain only a small number of rows. See the
#'    **Examples** section.
#'   }
#'   \item{`ppc_ecdf_overlay()`, `ppc_dens_overlay()`,
#'          `ppc_ecdf_overlay_grouped()`, `ppc_dens_overlay_grouped()`}{
#'    Kernel density or empirical CDF estimates of each dataset (row) in
#'    `yrep` are overlaid, with the distribution of `y` itself on top
#'    (and in a darker shade). When using `ppc_ecdf_overlay()` with discrete
#'    data, set the `discrete` argument to `TRUE` for better results.
#'    For an example of `ppc_dens_overlay()` also see Gabry et al. (2019).
#'   }
#'   \item{`ppc_violin_grouped()`}{
#'    The density estimate of `yrep` within each level of a grouping
#'    variable is plotted as a violin with horizontal lines at notable
#'    quantiles. `y` is overlaid on the plot either as a violin, points, or
#'    both, depending on the `y_draw` argument.
#'   }
#'   \item{`ppc_pit_ecdf()`, `ppc_pit_ecdf_grouped()`}{
#'    The PIT-ECDF of the empirical PIT values of `y` computed with respect to
#'    the corresponding `yrep` values. `100 * prob`% central simultaneous
#'    confidence intervals are provided to asses if `y` and `yrep` originate
#'    from the same distribution. The PIT values can also be provided directly
#'    as `pit`.
#'    See Säilynoja et al. (2021) for more details.}
#' }
#'
#' @template reference-vis-paper
#' @template reference-uniformity-test
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#'
#' @examples
#' color_scheme_set("brightblue")
#' y <- example_y_data()
#' yrep <- example_yrep_draws()
#' group <- example_group_data()
#' dim(yrep)
#'
#' ppc_dens_overlay(y, yrep[1:25, ])
#' \donttest{
#' # ppc_ecdf_overlay with continuous data (set discrete=TRUE if discrete data)
#' ppc_ecdf_overlay(y, yrep[sample(nrow(yrep), 25), ])
#'
#' # PIT-ECDF and PIT-ECDF difference plot of the PIT values of y compared to
#' # yrep with 99% simultaneous confidence bands.
#' ppc_pit_ecdf(y, yrep, prob = 0.99, plot_diff = FALSE)
#' ppc_pit_ecdf(y, yrep, prob = 0.99, plot_diff = TRUE)
#' }
#'
#' # for ppc_hist,dens,freqpoly,boxplot,dots definitely use a subset yrep rows so
#' # only a few (instead of nrow(yrep)) histograms are plotted
#' ppc_hist(y, yrep[1:8, ])
#' \donttest{
#' color_scheme_set("red")
#' ppc_boxplot(y, yrep[1:8, ])
#'
#' # wizard hat plot
#' color_scheme_set("blue")
#' ppc_dens(y, yrep[200:202, ])
#'
#' # dot plot
#' ppc_dots(y, yrep[1:8, ])
#' }
#'
#' \donttest{
#' # frequency polygons
#' ppc_freqpoly(y, yrep[1:3, ], alpha = 0.1, size = 1, binwidth = 5)
#'
#' ppc_freqpoly_grouped(y, yrep[1:3, ], group) + yaxis_text()
#'
#' # if groups are different sizes then the 'freq' argument can be useful
#' ppc_freqpoly_grouped(y, yrep[1:3, ], group, freq = FALSE) + yaxis_text()
#' }
#'
#' # density and distribution overlays by group
#' ppc_dens_overlay_grouped(y, yrep[1:25, ], group = group)
#'
#' ppc_ecdf_overlay_grouped(y, yrep[1:25, ], group = group)
#'
#' \donttest{
#' # PIT-ECDF plots of the PIT values by group
#' # with 99% simultaneous confidence bands.
#' ppc_pit_ecdf_grouped(y, yrep, group=group, prob=0.99)
#' }
#'
#' \donttest{
#' # don't need to only use small number of rows for ppc_violin_grouped
#' # (as it pools yrep draws within groups)
#' color_scheme_set("gray")
#' ppc_violin_grouped(y, yrep, group, size = 1.5)
#' ppc_violin_grouped(y, yrep, group, alpha = 0)
#'
#' # change how y is drawn
#' ppc_violin_grouped(y, yrep, group, alpha = 0, y_draw = "points", y_size = 1.5)
#' ppc_violin_grouped(y, yrep, group,
#'   alpha = 0, y_draw = "both",
#'   y_size = 1.5, y_alpha = 0.5, y_jitter = 0.33
#' )
#' }
NULL


#' @rdname PPC-distributions
#' @export
ppc_data <- function(y, yrep, group = NULL) {
  y <- validate_y(y)
  N <- length(y)
  yrep <- validate_predictions(yrep, N)
  if (!is.null(group)) {
    group <- validate_group(group, N)
  }
  # see R/ppd-distributions.R
  .ppd_data(predictions = yrep, y = y, group = group)
}


#' @rdname PPC-distributions
#' @export
#' @template args-density-controls
ppc_dens_overlay <-
  function(y,
           yrep,
           ...,
           size = 0.25,
           alpha = 0.7,
           trim = FALSE,
           bw = "nrd0",
           adjust = 1,
           kernel = "gaussian",
           n_dens = 1024) {
    check_ignored_arguments(...)

    data <- ppc_data(y, yrep)
    ggplot(data, mapping = aes(x = .data$value)) +
      overlay_ppd_densities(
        mapping = aes(group = .data$rep_id, color = "yrep"),
        data = function(x) dplyr::filter(x, !.data$is_y),
        linewidth = size,
        alpha = alpha,
        trim = trim,
        bw = bw,
        adjust = adjust,
        kernel = kernel,
        n = n_dens
      ) +
      overlay_ppd_densities(
        mapping = aes(color = "y"),
        data = function(x) dplyr::filter(x, .data$is_y),
        lineend = "round",
        linewidth = 1,
        trim = trim,
        bw = bw,
        adjust = adjust,
        kernel = kernel,
        n = n_dens
      ) +
      scale_color_ppc() +
      bayesplot_theme_get() +
      dont_expand_axes() +
      yaxis_title(FALSE) +
      xaxis_title(FALSE) +
      yaxis_text(FALSE) +
      yaxis_ticks(FALSE)
  }


#' @rdname PPC-distributions
#' @export
#' @template args-density-controls
ppc_dens_overlay_grouped <- function(y,
                                     yrep,
                                     group,
                                     ...,
                                     size = 0.25,
                                     alpha = 0.7,
                                     trim = FALSE,
                                     bw = "nrd0",
                                     adjust = 1,
                                     kernel = "gaussian",
                                     n_dens = 1024) {
  check_ignored_arguments(...)

  p_overlay <- ppc_dens_overlay(
    y = y,
    yrep = yrep,
    ...,
    size = size,
    alpha = alpha,
    trim = trim,
    bw = bw,
    adjust = adjust,
    kernel = kernel,
    n_dens = n_dens
  )
  # Use + list(data) trick to replace the data in the plot. The layer-specific
  # data in the y and yrep layers should be safe because they are
  # specified using a function on the main plot data.
  data <- ppc_data(y, yrep, group = group)
  p_overlay <- p_overlay + list(data)

  p_overlay +
    facet_wrap("group") +
    force_axes_in_facets()
}

#' @export
#' @rdname PPC-distributions
#' @param discrete For `ppc_ecdf_overlay()`, should the data be treated as
#'   discrete? The default is `FALSE`, in which case `geom="line"` is
#'   passed to [ggplot2::stat_ecdf()]. If `discrete` is set to
#'   `TRUE` then `geom="step"` is used.
#' @param pad A logical scalar passed to [ggplot2::stat_ecdf()].
#'
ppc_ecdf_overlay <- function(y,
                             yrep,
                             ...,
                             discrete = FALSE,
                             pad = TRUE,
                             size = 0.25,
                             alpha = 0.7) {
  check_ignored_arguments(...)
  data <- ppc_data(y, yrep)

  ggplot(data) +
    aes(x = .data$value) +
    hline_at(
      0.5,
      linewidth = 0.1,
      linetype = 2,
      color = get_color("dh")
    ) +
    hline_at(
      c(0, 1),
      linewidth = 0.2,
      linetype = 2,
      color = get_color("dh")
    ) +
    stat_ecdf(
      data = function(x) dplyr::filter(x, !.data$is_y),
      mapping = aes(group = .data$rep_id, color = "yrep"),
      geom = if (discrete) "step" else "line",
      linewidth = size,
      alpha = alpha,
      pad = pad
    ) +
    stat_ecdf(
      data = function(x) dplyr::filter(x, .data$is_y),
      mapping = aes(color = "y"),
      geom = if (discrete) "step" else "line",
      linewidth = 1,
      pad = pad
    ) +
    scale_color_ppc() +
    scale_y_continuous(breaks = c(0, 0.5, 1)) +
    bayesplot_theme_get() +
    yaxis_title(FALSE) +
    xaxis_title(FALSE)
}

#' @export
#' @rdname PPC-distributions
ppc_ecdf_overlay_grouped <- function(y,
                                     yrep,
                                     group,
                                     ...,
                                     discrete = FALSE,
                                     pad = TRUE,
                                     size = 0.25,
                                     alpha = 0.7) {
  check_ignored_arguments(...)

  p_overlay <- ppc_ecdf_overlay(
    y = y,
    yrep = yrep,
    ...,
    discrete = discrete,
    pad = pad,
    size = size,
    alpha = alpha
  )

  # Use + list(data) trick to replace the data in the plot
  data <- ppc_data(y, yrep, group = group)
  p_overlay <- p_overlay + list(data)

  p_overlay +
    facet_wrap("group") +
    force_axes_in_facets()
}


#' @rdname PPC-distributions
#' @export
ppc_dens <-
  function(y,
           yrep,
           ...,
           trim = FALSE,
           size = 0.5,
           alpha = 1) {
    check_ignored_arguments(...)
    data <- ppc_data(y, yrep)
    ggplot(data, mapping = aes(
      x = .data$value,
      fill = .data$is_y_label,
      color = .data$is_y_label
    )) +
      geom_density(
        linewidth = size,
        alpha = alpha,
        trim = trim
      ) +
      scale_fill_ppc() +
      scale_color_ppc() +
      bayesplot_theme_get() +
      facet_wrap_parsed("rep_label") +
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


#' @rdname PPC-distributions
#' @export
ppc_hist <-
  function(y,
           yrep,
           ...,
           binwidth = NULL,
           bins = NULL,
           breaks = NULL,
           freq = TRUE) {
    check_ignored_arguments(...)

    data <- ppc_data(y, yrep)
    ggplot(data, mapping = set_hist_aes(
      freq = freq,
      fill = !!quote(is_y_label),
      color = !!quote(is_y_label)
    )) +
      geom_histogram(
        linewidth = 0.25,
        binwidth = binwidth,
        bins = bins,
        breaks = breaks
      ) +
      scale_fill_ppc() +
      scale_color_ppc() +
      facet_wrap_parsed("rep_label") +
      force_axes_in_facets() +
      dont_expand_y_axis() +
      bayesplot_theme_get() +
      space_legend_keys() +
      yaxis_text(FALSE) +
      yaxis_title(FALSE) +
      yaxis_ticks(FALSE) +
      xaxis_title(FALSE) +
      facet_text(FALSE) +
      facet_bg(FALSE)
  }


#' @rdname PPC-distributions
#' @export
ppc_freqpoly <-
  function(y,
           yrep,
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

    data <- ppc_data(y, yrep, group = dots$group)
    ggplot(data, mapping = set_hist_aes(
      freq = freq,
      fill = !!quote(is_y_label),
      color = !!quote(is_y_label)
    )) +
      geom_area(
        stat = "bin",
        binwidth = binwidth,
        bins = bins,
        linewidth = size,
        alpha = alpha
      ) +
      scale_fill_ppc() +
      scale_color_ppc() +
      facet_wrap_parsed("rep_label") +
      bayesplot_theme_get() +
      force_axes_in_facets() +
      dont_expand_y_axis() +
      space_legend_keys() +
      yaxis_text(FALSE) +
      yaxis_title(FALSE) +
      yaxis_ticks(FALSE) +
      xaxis_title(FALSE) +
      facet_text(FALSE)
  }


#' @rdname PPC-distributions
#' @export
ppc_freqpoly_grouped <-
  function(y,
           yrep,
           group,
           ...,
           binwidth = NULL,
           bins = NULL,
           freq = TRUE,
           size = 0.5,
           alpha = 1) {
    check_ignored_arguments(...)
    call <- match.call(expand.dots = FALSE)
    g <- eval(ungroup_call("ppc_freqpoly", call), parent.frame())
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


#' @rdname PPC-distributions
#' @export
#' @param notch For the box plot, a logical scalar passed to
#'   [ggplot2::geom_boxplot()]. Note: unlike `geom_boxplot()`, the default is
#'   `notch=TRUE`.
#'
ppc_boxplot <-
  function(y,
           yrep,
           ...,
           notch = TRUE,
           size = 0.5,
           alpha = 1) {
    check_ignored_arguments(...)

    data <- ppc_data(y, yrep)
    ggplot(data, mapping = aes(
      x = .data$rep_label,
      y = .data$value,
      fill = .data$is_y_label,
      color = .data$is_y_label
    )) +
      geom_boxplot(
        notch = notch,
        linewidth = size,
        alpha = alpha,
        outlier.alpha = 2 / 3,
        outlier.size = 1
      ) +
      scale_x_discrete(labels = function(x) parse(text = x)) +
      scale_fill_ppc() +
      scale_color_ppc() +
      bayesplot_theme_get() +
      yaxis_title(FALSE) +
      xaxis_ticks(FALSE) +
      xaxis_text(FALSE) +
      xaxis_title(FALSE)
  }

#' @rdname PPC-distributions
#' @export
#' @template args-dots
ppc_dots <-
  function(y,
           yrep,
           ...,
           binwidth = NA,
           quantiles = NA,
           freq = TRUE) {
    check_ignored_arguments(..., ok_args = c("dotsize", "layout", "stackratio", "overflow"))

    suggested_package("ggdist")

    data <- ppc_data(y, yrep)

    ggplot(data, mapping = set_hist_aes(
      freq = freq,
      fill = .data$is_y_label,
      color = .data$is_y_label
    )) +
      ggdist::stat_dots(
        binwidth = binwidth,
        quantiles = quantiles,
        ...
      ) +
      scale_fill_ppc() +
      scale_color_ppc() +
      facet_wrap_parsed("rep_label") +
      force_axes_in_facets() +
      bayesplot_theme_get() +
      space_legend_keys() +
      yaxis_text(FALSE) +
      yaxis_title(FALSE) +
      yaxis_ticks(FALSE) +
      xaxis_title(FALSE) +
      facet_text(FALSE) +
      facet_bg(FALSE)
  }

#' @rdname PPC-distributions
#' @export
#' @param probs A numeric vector passed to [ggplot2::geom_violin()]'s
#'   `draw_quantiles` argument to specify at which quantiles to draw
#'   horizontal lines. Set to `NULL` to remove the lines.
#' @param y_draw For `ppc_violin_grouped()`, a string specifying how to draw
#'   `y`: `"violin"` (default), `"points"` (jittered points), or `"both"`.
#' @param y_jitter,y_size,y_alpha For `ppc_violin_grouped()`, if `y_draw` is
#'   `"points"` or `"both"` then `y_size`, `y_alpha`, and `y_jitter` are passed
#'   to to the `size`, `alpha`, and `width` arguments of [ggplot2::geom_jitter()]
#'   to control the appearance of `y` points. The default of `y_jitter=NULL`
#'   will let **ggplot2** determine the amount of jitter.
#'
ppc_violin_grouped <-
  function(y,
           yrep,
           group,
           ...,
           probs = c(0.1, 0.5, 0.9),
           size = 1,
           alpha = 1,
           y_draw = c("violin", "points", "both"),
           y_size = 1,
           y_alpha = 1,
           y_jitter = 0.1) {
    check_ignored_arguments(...)

    y_draw <- match.arg(y_draw)
    y_violin <- y_draw %in% c("violin", "both")
    y_points <- y_draw %in% c("points", "both")

    args_violin_yrep <- list(
      data = function(x) dplyr::filter(x, !.data$is_y),
      aes(fill = "yrep", color = "yrep"),
      draw_quantiles = probs,
      alpha = alpha,
      linewidth = size
    )

    args_violin_y <- list(
      data = function(x) dplyr::filter(x, .data$is_y),
      aes(fill = "y", color = "y"),
      alpha = 0
    )

    args_jitter_y <- list(
      data = function(x) dplyr::filter(x, .data$is_y),
      aes(fill = "y", color = "y"),
      shape = 21,
      alpha = y_alpha,
      size = y_size,
      width = y_jitter,
      height = 0
    )

    violin_y_func <- if (y_violin) geom_violin else geom_ignore
    jitter_y_func <- if (y_points) geom_jitter else geom_ignore

    layer_violin_yrep <- do.call(geom_violin, args_violin_yrep)
    layer_violin_y <- do.call(violin_y_func, args_violin_y)
    layer_jitter_y <- do.call(jitter_y_func, args_jitter_y)

    data <- ppc_data(y, yrep, group)

    ggplot(data, mapping = aes(x = .data$group, y = .data$value)) +
      layer_violin_yrep +
      layer_violin_y +
      layer_jitter_y +
      scale_fill_ppc(values = c(NA, get_color("l"))) +
      scale_color_ppc() +
      yaxis_title(FALSE) +
      xaxis_title(FALSE) +
      bayesplot_theme_get()
  }


#' @export
#' @param pit An optional vector of probability integral transformed values for
#'   which the ECDF is to be drawn. If NULL, PIT values are computed to `y` with
#'   respect to the corresponding values in `yrep`.
#' @rdname PPC-distributions
#'
ppc_pit_ecdf <- function(y,
                         yrep,
                         ...,
                         pit = NULL,
                         K = NULL,
                         prob = .99,
                         plot_diff = FALSE,
                         interpolate_adj = NULL) {
  check_ignored_arguments(...,
    ok_args = c("K", "pit", "prob", "plot_diff", "interpolate_adj")
  )

  if (is.null(pit)) {
    pit <- ppc_data(y, yrep) %>%
      group_by(.data$y_id) %>%
      dplyr::group_map(
        ~ mean(.x$value[.x$is_y] > .x$value[!.x$is_y]) +
        runif(1, max = mean(.x$value[.x$is_y] == .x$value[!.x$is_y]))
        ) %>%
      unlist()
    if (is.null(K)) {
      K <- min(nrow(yrep) + 1, 1000)
    }
  } else {
    inform("'pit' specified so ignoring 'y', and 'yrep' if specified.")
    pit <- validate_pit(pit)
    if (is.null(K)) {
      K <- length(pit)
    }
  }
  N <- length(pit)
  gamma <- adjust_gamma(
    N = N,
    K = K,
    prob = prob,
    interpolate_adj = interpolate_adj
  )
  lims <- ecdf_intervals(gamma = gamma, N = N, K = K)
  ggplot() +
    aes(
      x = seq(0,1,length.out = K),
      y = ecdf(pit)(seq(0, 1, length.out = K)) -
          (plot_diff == TRUE) * seq(0, 1, length.out = K),
      color = "y"
    ) +
    geom_step(show.legend = FALSE) +
    geom_step(aes(
      y = lims$upper[-1] / N - (plot_diff == TRUE) * seq(0, 1, length.out = K),
      color = "yrep"
    ),
    linetype = 2, show.legend = FALSE) +
    geom_step(aes(
      y = lims$lower[-1] / N - (plot_diff == TRUE) * seq(0, 1, length.out = K),
      color = "yrep"
    ),
    linetype = 2, show.legend = FALSE) +
    labs(y = ifelse(plot_diff,"ECDF - difference","ECDF"), x = "PIT") +
    yaxis_ticks(FALSE) +
    scale_color_ppc() +
    bayesplot_theme_get()
}

#' @export
#' @rdname PPC-distributions
#'
ppc_pit_ecdf_grouped <-
  function(y,
           yrep,
           group,
           ...,
           K = NULL,
           pit = NULL,
           prob = .99,
           plot_diff = FALSE,
           interpolate_adj = NULL) {
    check_ignored_arguments(...,
      ok_args = c("K", "pit", "prob", "plot_diff", "interpolate_adj")
    )

    if (is.null(pit)) {
      pit <- ppc_data(y, yrep, group) %>%
        group_by(.data$y_id) %>%
        dplyr::group_map(
          ~ mean(.x$value[.x$is_y] > .x$value[!.x$is_y]) +
          runif(1, max = mean(.x$value[.x$is_y] == .x$value[!.x$is_y]))
          ) %>%
        unlist()
      if (is.null(K)) {
        K <- min(nrow(yrep) + 1, 1000)
      }
    } else {
      inform("'pit' specified so ignoring 'y' and 'yrep' if specified.")
      pit <- validate_pit(pit)
    }
    N <- length(pit)

    gammas <- lapply(unique(group), function(g) {
      N_g <- sum(group == g)
      adjust_gamma(
        N = N_g,
        K = ifelse(is.null(K), N_g, K),
        prob = prob,
        interpolate_adj = interpolate_adj
      )
    })
    names(gammas) <- unique(group)

    data <- data.frame(pit = pit, group = group) %>%
      group_by(group) %>%
      dplyr::group_map(
        ~ data.frame(
          ecdf_value = ecdf(.x$pit)(seq(0, 1, length.out = ifelse(is.null(K), nrow(.x), K))),
          group = .y[1],
          lims_upper = ecdf_intervals(
            gamma = gammas[[unlist(.y[1])]],
            N = nrow(.x),
            K = ifelse(is.null(K), nrow(.x), K)
          )$upper[-1] / nrow(.x),
          lims_lower = ecdf_intervals(
            gamma = gammas[[unlist(.y[1])]],
            N = nrow(.x),
            K = ifelse(is.null(K), nrow(.x), K)
          )$lower[-1] / nrow(.x),
          x = seq(0, 1, length.out = ifelse(is.null(K), nrow(.x), K))
        )
      ) %>%
      dplyr::bind_rows()

    ggplot(data) +
      aes(
        x = .data$x,
        y = .data$ecdf_value - (plot_diff == TRUE) * .data$x,
        group = .data$group,
        color = "y"
      ) +
      geom_step(show.legend = FALSE) +
      geom_step(aes(
        y = .data$lims_upper - (plot_diff == TRUE) * .data$x,
        color = "yrep"
      ),
      linetype = 2, show.legend = FALSE) +
      geom_step(aes(
        y = .data$lims_lower - (plot_diff == TRUE) * .data$x,
        color = "yrep"
      ),
      linetype = 2, show.legend = FALSE) +
      labs(y = ifelse(plot_diff,"ECDF - difference","ECDF"), x = "PIT") +
      yaxis_ticks(FALSE) +
      bayesplot_theme_get() +
      facet_wrap("group") +
      scale_color_ppc() +
      force_axes_in_facets()
  }
