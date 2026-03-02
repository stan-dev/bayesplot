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
           bounds = NULL,
           n_dens = 1024) {
    check_ignored_arguments(...)
    bounds <- validate_density_bounds(bounds)

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
        bounds = bounds,
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
        bounds = bounds,
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
                                     bounds = NULL,
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
    bounds = bounds,
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
           alpha = 1,
           bounds = NULL) {
    check_ignored_arguments(...)
    bounds <- validate_density_bounds(bounds)
    data <- ppc_data(y, yrep)
    ggplot(data, mapping = aes(
      x = .data$value,
      fill = .data$is_y_label,
      color = .data$is_y_label
    )) +
      geom_density(
        linewidth = size,
        alpha = alpha,
        trim = trim,
        bounds = bounds
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
           quantiles = 100,
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
#' @param probs A numeric vector of probabilities controlling where quantile
#'   lines are drawn. Set to `NULL` to remove the lines.
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
    if (utils::packageVersion("ggplot2") >= "4.0.0") {
      args_violin_yrep$draw_quantiles <- NULL
      args_violin_yrep$quantiles <- probs
      args_violin_yrep$quantile.linetype <- 1
    }

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
#' @param interpolate_adj For `ppc_loo_pit_ecdf()` when `method = "independent"`,
#'   a boolean defining if the simultaneous confidence bands should be 
#'   interpolated based on precomputed values rather than computed exactly. 
#'   Computing the bands may be computationally intensive and the approximation 
#'   gives a fast method for assessing the ECDF trajectory. The default is to use
#'   interpolation if `K` is greater than 200.
#' @param method For `ppc_loo_pit_ecdf()`, the method used to calculate the
#'   uniformity test:
#'   * `"independent"`: (default) Assumes independence (Säilynoja et al., 2022).
#'   * `"correlated"`: Accounts for correlation (Tesso & Vehtari, 2026).
#' @param test For `ppc_loo_pit_ecdf()` when `method = "correlated"`, which
#'   dependence-aware test to use: `"POT"`, `"PRIT"`, or `"PIET"`.
#'   Defaults to `"POT"`.
#' @param gamma For `ppc_loo_pit_ecdf()` when `method = "correlated"`, tolerance
#'   threshold controlling how strongly suspicious points are flagged. Larger
#'   values (gamma > 0) emphasizes points with larger deviations. If `NULL`, automatically
#'   determined based on p-value.
#' @param color For `ppc_loo_pit_ecdf()` when `method = "correlated"`, a vector
#'   with base color and highlight color for the ECDF plot. Defaults to
#'   `c(ecdf = "black", highlight = "red")`. The first element is used for
#'   the main ECDF line, the second for highlighted suspicious regions.
#' @rdname PPC-distributions
#'
ppc_pit_ecdf <- function(y,
                         yrep,
                         ...,
                         pit = NULL,
                         K = NULL,
                         prob = .99,
                         plot_diff = FALSE,
                         interpolate_adj = NULL,
                         method = "independent",
                         test = NULL,
                         gamma = NULL,
                         linewidth = NULL,
                         color = NULL
                        ) {
  check_ignored_arguments(...,
    ok_args = c("K", "pit", "prob", "plot_diff", "interpolate_adj",
    "method", "test", "gamma", "linewidth", "color")
  )

  method <- match.arg(method, choices = c("independent", "correlated"))

  .warn_ignored <- function(method, args) {
    msg <- paste0("As method = ", method, " specified; ignoring: ",
    paste(args, collapse = ", "), ".")
    inform(msg)
  }

  switch(method,
    "correlated" = {
      if (!is.null(interpolate_adj)) .warn_ignored("'correlated'", "interpolate_adj")
      test <- match.arg(test %||% "POT", choices = c("POT", "PRIT", "PIET"))
      alpha     <- 1 - prob
      gamma     <- gamma %||% 0
      linewidth <- linewidth %||% 0.3
      color     <- color %||% c(ecdf = "black", highlight = "red")
    },
    "independent" = {
      ignored <- character(0)
      if (!is.null(test)) ignored <- c(ignored, "test")
      if (!is.null(gamma)) ignored <- c(ignored, "gamma")
      if (length(ignored) > 0) .warn_ignored("'independent'", ignored)
    }
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

  n_obs <- length(pit)
  unit_interval <- seq(0, 1, length.out = K)
  .ecdf_pit_fn <- ecdf(pit)

  # Correlated method --------------------------------------------------
  if (method == "correlated") {
    # Compute test p-value and Cauchy-transformed values
    if (test == "POT") {
      std_cauchy_values <- .compute_cauchy(.pot_test(sort(pit)))
      p_value_CCT <- .cauchy_combination_test(.pot_test(pit), truncate = TRUE)
    } else if (test == "PIET") {
      std_cauchy_values <- .compute_cauchy(.piet_test(sort(pit)))
      p_value_CCT <- .cauchy_combination_test(.piet_test(pit), truncate = FALSE)
    } else { # PRIT
      std_cauchy_values <- .compute_cauchy(.prit_test(sort(pit)))
      p_value_CCT <- .cauchy_combination_test(.prit_test(pit), truncate = TRUE)
    }

    pointwise_contribution <- .compute_shapley_values(std_cauchy_values)

    if (gamma < 0 || gamma > max(pointwise_contribution)) {
      stop(sprintf(
        "gamma must be in the interval [0, %.2f], but gamma = %s was provided",
        max(pointwise_contribution), gamma
      ))
    }
    x_axis_combined <- sort(unique(c(unit_interval, pit)))
    
    # Evaluate at 0-1 interval b´values
    df_main <- tibble::tibble(
      x = x_axis_combined,
      ecdf_pit = .ecdf_pit_fn(x_axis_combined) - plot_diff * x_axis_combined
    )
    
    # Evaluate at pit values (used for highlighing)
    df_pit <- tibble::tibble(
      pit = pit,
      ecdf_pit = .ecdf_pit_fn(pit) - plot_diff * pit
    )
    df_pit <- df_pit[order(df_pit$pit), ]

    # Plot ECDF
    p <- ggplot() +
      geom_step(
        data = df_main, aes(x = .data$x, y = .data$ecdf_pit),
        show.legend = FALSE, linewidth = linewidth, color = color[1]) +
      labs(
        y = dplyr::if_else(plot_diff, "ECDF difference", "ECDF"),
        x = "PIT"
      )
    
    # Add reference line
    p <- p + geom_segment(
      aes(
        x = 0, y = 0, xend = 1, 
        yend = dplyr::if_else(plot_diff, 0, 1)
      ),
    linetype = 2, color = "darkgrey"
    )
  
    # Identify and highlight suspecious points (regions) of the ECDF
    if (p_value_CCT < alpha) {
      red_idx <- which(pointwise_contribution > gamma)
      
      if (length(red_idx) > 0) {
        df_red <- df_pit[red_idx, ]
        
        # Groups of consecutive suspicious points
        df_red$segment <- cumsum(c(1, diff(red_idx) != 1))
        
        # Separate isolated vs grouped points
        segment_lengths <- stats::ave(df_red$pit, df_red$segment, FUN = length)
        df_isolated <- df_red[segment_lengths == 1, ]
        df_grouped <- df_red[segment_lengths > 1, ]
        
        # Create segments based on x_combined values for grouped points
        if (nrow(df_grouped) > 0) {
          segments_list <- lapply(
            split(df_grouped, df_grouped$segment), function(group) {
              group_indices <- match(group$pit, x_axis_combined)
              idx_range <- min(group_indices):max(group_indices)
            
              tibble::tibble(
                x = df_main$x[idx_range],
                ecdf_pit = df_main$ecdf_pit[idx_range],
                segment = group$segment[1]
              )
            }
          )
          df_segments <- do.call(rbind, segments_list)
          
          p <- p + geom_step(
            data = df_segments,
            aes(x = .data$x, y = .data$ecdf_pit, group = .data$segment),
            color = color[2],
            linewidth = linewidth + 0.8
          )
        }
        
        if (nrow(df_isolated) > 0) {
          p <- p + geom_point(
            data = df_isolated,
            aes(x = .data$pit, y = .data$ecdf_pit),
            color = color[2],
            size = linewidth + 1
          )
        }
      }
    }
    
    # Apply bayesplot theme and styling
    p <- p +
      yaxis_ticks(FALSE) +
      scale_color_ppc() +
      annotate(
        "text",
        x = -Inf, y = Inf,
        label = sprintf("Uniformity p-value = %.3f", p_value_CCT),
        hjust = -0.1, vjust = 1.5,
        size = 6, color = "black"
      ) +
      bayesplot::theme_default(base_family = "sans", base_size = 16)
    
    if (plot_diff) {
      epsilon = max(
        sqrt(log(2 / (1 - prob)) / (2 * length(pit))),
        max(abs(df_main$ecdf_pit))
      )

      p <- p + scale_y_continuous(limits = c(-epsilon, epsilon))
    }

    return(p)
  }

  # Independent method --------------------------------------------------
  gamma_indep <- adjust_gamma(
    N = n_obs,
    K = K,
    prob = prob,
    interpolate_adj = interpolate_adj
  )

  lims <- ecdf_intervals(gamma = gamma_indep, N = n_obs, K = K)
  ecdf_eval <- .ecdf_pit_fn(unit_interval) - plot_diff * unit_interval

  lims_upper_scaled <- lims$upper[-1] * (1 / n_obs) - plot_diff * unit_interval
  lims_lower_scaled <- lims$lower[-1] * (1 / n_obs) - plot_diff * unit_interval

  p <- ggplot() +
    geom_step(
      aes(x = unit_interval, y = lims_upper_scaled, color = "yrep"),
      linetype = 2, show.legend = FALSE
    ) +
    geom_step(
      aes(x = unit_interval, y = lims_lower_scaled, color = "yrep"),
      linetype = 2, show.legend = FALSE
    ) +
    geom_step(
      aes(x = unit_interval, y = ecdf_eval, color = "y", linewidth = linewidth),
      show.legend = FALSE
    ) +
    labs(
      y = dplyr::if_else(plot_diff, "ECDF difference", "ECDF"),
      x = "PIT"
    ) +
    yaxis_ticks(FALSE) +
    scale_color_ppc() +
    bayesplot::theme_default(base_family = "sans", base_size = 16)

  return(p)
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
