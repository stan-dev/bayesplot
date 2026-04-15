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
#'    See the **Examples** section.
#'    }
#'   \item{`ppc_freqpoly_grouped()`}{
#'    A separate frequency polygon is plotted for each level of a grouping
#'    variable for `y` and each dataset (row) in `yrep`. For this plot
#'    `yrep` should therefore contain only a small number of rows. See the
#'    **Examples** section.
#'   }
#'   \item{`ppc_ecdf_overlay()`, `ppc_dens_overlay()`,
#'          `ppc_ecdf_overlay_grouped()`, `ppc_dens_overlay_grouped()`}{
#'    Kernel density or empirical CDF estimates of each dataset (row) in `yrep`
#'    are overlaid, with the distribution of `y` itself on top (and in a darker
#'    shade). For an example of `ppc_dens_overlay()` also see Gabry et al.
#'    (2019).
#'   }
#'   \item{`ppc_violin_grouped()`}{
#'    The density estimate of `yrep` within each level of a grouping
#'    variable is plotted as a violin with horizontal lines at notable
#'    quantiles. `y` is overlaid on the plot either as a violin, points, or
#'    both, depending on the `y_draw` argument.
#'   }
#'   \item{`ppc_pit_ecdf()`, `ppc_pit_ecdf_grouped()`}{
#'    The PIT-ECDF of empirical PIT values for `y` relative to corresponding
#'    draws in `yrep` (or precomputed values supplied via `pit`).
#'    With `method = "independent"`, the plot shows `100 * prob`% central
#'    simultaneous confidence intervals under an independence assumption.
#'    With `method = "correlated"`, the plot uses a dependence-aware
#'    uniformity assessment and can highlight suspicious regions.
#'    See Säilynoja et al. (2025) and Tesso & Vehtari (2026) for details.
#'   }
#'   \item{`ppc_data()`}{
#'    This function prepares data for plotting with **ggplot2** and doesn't
#'    itself make any plots. Users can call it directly to obtain the underlying
#'    data frame that (in most cases) is passed to **ggplot2**. This is useful
#'    when you want to customize the appearance of PPC plots beyond what the
#'    built-in plotting functions allow, or when you want to construct new types
#'    of PPC visualizations based on the same underlying data.
#'   }
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
#' # ppc_ecdf_overlay
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
#' @param discrete `r lifecycle::badge("deprecated")` The `discrete` argument is
#'   deprecated. The ECDF is a step function by definition, so `geom_step()` is
#'   now always used.
#' @param pad A logical scalar passed to [ggplot2::stat_ecdf()].
#'
ppc_ecdf_overlay <- function(y,
                             yrep,
                             ...,
                             discrete = deprecated(),
                             pad = TRUE,
                             size = 0.25,
                             alpha = 0.7) {
  check_ignored_arguments(...)

  if (is_present(discrete)) {
    deprecate_warn(
      "1.16.0",
      "ppc_ecdf_overlay(discrete)",
      details = "The ECDF is now always plotted as a step function."
    )
  }

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
      geom = "step",
      linewidth = size,
      alpha = alpha,
      pad = pad
    ) +
    stat_ecdf(
      data = function(x) dplyr::filter(x, .data$is_y),
      mapping = aes(color = "y"),
      geom = "step",
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
                                     discrete = deprecated(),
                                     pad = TRUE,
                                     size = 0.25,
                                     alpha = 0.7) {
  check_ignored_arguments(...)

  if (is_present(discrete)) {
    deprecate_warn(
      "1.16.0",
      "ppc_ecdf_overlay_grouped(discrete)",
      details = "The ECDF is now always plotted as a step function."
    )
  }

  p_overlay <- ppc_ecdf_overlay(
    y = y,
    yrep = yrep,
    ...,
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

#' @rdname PPC-distributions
#' @export
#' @template args-pit-ecdf
#' @param K An optional integer defining the number of equally spaced evaluation
#'   points for the PIT-ECDF. Reducing K when using `interpolate_adj = FALSE`
#'   makes computing the confidence bands faster. For `ppc_pit_ecdf()` and
#'   `ppc_pit_ecdf_grouped()` when `method = 'independent'`. If `pit` is
#'   supplied, defaults to `length(pit)`, otherwise `yrep` determines the
#'   maximum accuracy of the estimated PIT values and `K` is set to
#'   `min(nrow(yrep) + 1, 1000)`. For `mcmc_rank_ecdf()`, defaults to the number
#'   of iterations per chain in `x`.
#' @param prob The desired simultaneous coverage level of the bands around the
#'   ECDF. A value in (0,1). For `ppc_pit_ecdf()` and `ppc_pit_ecdf_grouped()`.
#' @param plot_diff A boolean defining whether to plot the difference between
#'   the observed PIT-ECDF and the theoretical expectation for uniform PIT
#'   values rather than plotting the regular ECDF. For `ppc_pit_ecdf()` and
#'   `ppc_pit_ecdf_grouped()` when `method = 'independent'`. The default is
#'   `FALSE`, but for large samples we recommend setting `plot_diff = TRUE` to
#'   better use the plot area.
#' @param interpolate_adj A boolean defining if the simultaneous confidence
#'   bands should be interpolated based on precomputed values rather than
#'   computed exactly. Computing the bands may be computationally intensive and
#'   the approximation gives a fast method for assessing the ECDF trajectory.
#'   For `ppc_pit_ecdf()` and `ppc_pit_ecdf_grouped()` when
#'   `method = 'independent'` and for `mcmc_rank_ecdf()`. The default is to use
#'   interpolation if `K` is greater than 200.
#' @param pit An optional vector of probability integral transformed values for
#'   which the ECDF is to be drawn. For `ppc_pit_ecdf()` and
#'   `ppc_pit_ecdf_grouped()`. If `NULL`, PIT values are computed to `y` with
#'   respect to the corresponding values in `yrep`.
#' @note
#' Note that the default "independent" method is **superseded** by
#' the "correlated" method (Tesso & Vehtari, 2026) which accounts for dependent
#' PIT values.
ppc_pit_ecdf <- function(y,
                         yrep,
                         ...,
                         pit = NULL,
                         K = NULL,
                         prob = .99,
                         plot_diff = FALSE,
                         interpolate_adj = NULL,
                         method = NULL,
                         test = NULL,
                         gamma = NULL,
                         linewidth = NULL,
                         color = NULL,
                         help_text = NULL,
                         pareto_pit = NULL,
                         help_text_shrinkage = NULL
                        ) {
  check_ignored_arguments(...,
    ok_args = c(
      "K", "pareto_pit", "pit", "prob", "plot_diff",
      "interpolate_adj", "method", "test", "gamma", "linewidth",
      "color", "help_text", "help_text_shrinkage"
    )
  )

  method_args <- .pit_ecdf_resolve_method_args(
    method = method,
    pit = pit,
    prob = prob,
    interpolate_adj = interpolate_adj,
    test = test,
    gamma = gamma,
    linewidth = linewidth,
    color = color,
    help_text = help_text,
    pareto_pit = pareto_pit,
    help_text_shrinkage = help_text_shrinkage
  )
  method <- method_args$method
  test <- method_args$test
  gamma <- method_args$gamma
  linewidth <- method_args$linewidth
  color <- method_args$color
  help_text <- method_args$help_text
  pareto_pit <- method_args$pareto_pit
  help_text_shrinkage <- method_args$help_text_shrinkage

  pit_data <- .compute_pit_values(y = y, yrep = yrep, lw = NULL,
    psis_object = NULL, group = NULL, K = K, pareto_pit = pareto_pit,
    pit = pit, loo_cv = FALSE)
  pit <- pit_data$pit
  K <- pit_data$K

  .pit_ecdf_plot_single(
    pit = pit,
    K = K,
    prob = prob,
    plot_diff = plot_diff,
    interpolate_adj = interpolate_adj,
    method = method,
    test = test,
    gamma = gamma,
    linewidth = linewidth,
    color = color,
    help_text = help_text,
    x_label = "PIT",
    help_text_shrinkage = help_text_shrinkage
  )
}

#' @rdname PPC-distributions
#' @export
#' @template args-pit-ecdf
#' @param K An optional integer defining the number of equally spaced evaluation
#'   points for the PIT-ECDF. Reducing K when using `interpolate_adj = FALSE`
#'   makes computing the confidence bands faster. For `ppc_pit_ecdf()` and
#'   `ppc_pit_ecdf_grouped()` when `method = 'independent'`. If `pit` is
#'   supplied, defaults to `length(pit)`, otherwise `yrep` determines the
#'   maximum accuracy of the estimated PIT values and `K` is set to
#'   `min(nrow(yrep) + 1, 1000)`. For `mcmc_rank_ecdf()`, defaults to the number
#'   of iterations per chain in `x`.
#' @param prob The desired simultaneous coverage level of the bands around the
#'   ECDF. A value in (0,1). For `ppc_pit_ecdf()` and `ppc_pit_ecdf_grouped()`.
#' @param plot_diff A boolean defining whether to plot the difference between
#'   the observed PIT-ECDF and the theoretical expectation for uniform PIT
#'   values rather than plotting the regular ECDF. For `ppc_pit_ecdf()` and
#'   `ppc_pit_ecdf_grouped()` when `method = 'independent'`. The default is
#'   `FALSE`, but for large samples we recommend setting `plot_diff = TRUE` to
#'   better use the plot area.
#' @param interpolate_adj A boolean defining if the simultaneous confidence
#'   bands should be interpolated based on precomputed values rather than
#'   computed exactly. Computing the bands may be computationally intensive and
#'   the approximation gives a fast method for assessing the ECDF trajectory.
#'   For `ppc_pit_ecdf()` and `ppc_pit_ecdf_grouped()` when
#'   `method = 'independent'` and for `mcmc_rank_ecdf()`. The default is to use
#'   interpolation if `K` is greater than 200.
#' @param pit An optional vector of probability integral transformed values for
#'   which the ECDF is to be drawn. For `ppc_pit_ecdf()` and
#'   `ppc_pit_ecdf_grouped()`. If `NULL`, PIT values are computed to `y` with
#'   respect to the corresponding values in `yrep`.
#' @note
#' Note that the default "independent" method is **superseded** by
#' the "correlated" method (Tesso & Vehtari, 2026) which accounts for dependent
#' PIT values.
ppc_pit_ecdf_grouped <-
  function(y,
           yrep,
           group,
           ...,
           K = NULL,
           pit = NULL,
           prob = .99,
           plot_diff = FALSE,
           interpolate_adj = NULL,
           method = NULL,
           test = NULL,
           gamma = NULL,
           linewidth = NULL,
           color = NULL,
           help_text = NULL,
           pareto_pit = NULL,
           help_text_shrinkage = NULL) {
    check_ignored_arguments(...,
      ok_args = c("K", "pareto_pit", "pit", "prob", "plot_diff",
                  "interpolate_adj", "method", "test", "gamma",
                  "linewidth", "color", "help_text", "help_text_shrinkage")
    )

    method_args <- .pit_ecdf_resolve_method_args(
      method = method,
      pit = pit,
      prob = prob,
      interpolate_adj = interpolate_adj,
      test = test,
      gamma = gamma,
      linewidth = linewidth,
      color = color,
      help_text = help_text,
      pareto_pit = pareto_pit,
      help_text_shrinkage = help_text_shrinkage
    )
    method <- method_args$method
    alpha <- method_args$alpha
    test <- method_args$test
    gamma <- method_args$gamma
    linewidth <- method_args$linewidth
    color <- method_args$color
    help_text <- method_args$help_text
    pareto_pit <- method_args$pareto_pit
    help_text_shrinkage <- method_args$help_text_shrinkage

    pit_data <- .compute_pit_values(y = y, yrep = yrep, lw = NULL,
      psis_object = NULL, group = group, K = K, pareto_pit = pareto_pit,
      pit = pit, loo_cv = FALSE)
    group <- pit_data$group
    pit <- pit_data$pit
    K <- pit_data$K

    data <- data.frame(pit = pit, group = group, stringsAsFactors = FALSE)
    group_levels <- unique(data$group)

    if (method == "correlated") {
      data_cor <- dplyr::group_by(data, .data$group) %>%
        dplyr::group_map(function(.x, .y) {
          n_obs <- nrow(.x)
          K_g <- K %||% n_obs
          correlated <- .pit_ecdf_correlated_data(
            pit = .x$pit,
            K = K_g,
            plot_diff = plot_diff,
            test = test,
            alpha = alpha,
            gamma = gamma
          )
          df_main <- data.frame(
            x = correlated$main$x,
            ecdf_value = correlated$main$ecdf_val,
            group = .y[[1]],
            stringsAsFactors = FALSE
          )
          red <- NULL
          if (nrow(correlated$segments) > 0) {
            red <- data.frame(
              x = correlated$segments$x,
              ecdf_value = correlated$segments$ecdf_val,
              segment = correlated$segments$segment,
              group = .y[[1]],
              stringsAsFactors = FALSE
            )
          }
          red_points <- NULL
          if (nrow(correlated$isolated) > 0) {
            red_points <- data.frame(
              x = correlated$isolated$pit,
              ecdf_value = correlated$isolated$ecdf_val,
              group = .y[[1]],
              stringsAsFactors = FALSE
            )
          }

          ann <- NULL
          if (isTRUE(help_text)) {
            ann <- data.frame(
              group = .y[[1]],
              x = -Inf,
              y = Inf,
              label = sprintf(
                "p[unif]^{%s} == '%s' ~ (alpha == '%.2f')",
                test, fmt_p(correlated$p_value), alpha
              ),
              stringsAsFactors = FALSE
            )
          }

          list(main = df_main, red = red, red_points = red_points, ann = ann)
        })

      main_df <- dplyr::bind_rows(lapply(data_cor, `[[`, "main"))
      red_df <- dplyr::bind_rows(lapply(data_cor, `[[`, "red"))
      red_points_df <- dplyr::bind_rows(lapply(data_cor, `[[`, "red_points"))
      ann_df <- dplyr::bind_rows(lapply(data_cor, `[[`, "ann"))
      ref_df <- data.frame(
        group = group_levels,
        x = 0,
        y = 0,
        xend = 1,
        yend = if (plot_diff) 0 else 1,
        stringsAsFactors = FALSE
      )

      p <- ggplot() +
        geom_step(
          data = main_df,
          mapping = aes(x = .data$x, y = .data$ecdf_value, group = .data$group),
          show.legend = FALSE,
          linewidth = linewidth,
          color = color["ecdf"]
        ) +
        geom_segment(
          data = ref_df,
          mapping = aes(
            x = .data$x,
            y = .data$y,
            xend = .data$xend,
            yend = .data$yend
          ),
          linetype = "dashed",
          color = "darkgrey",
          linewidth = 0.3
        )

      if (nrow(red_df) > 0) {
        p <- p + geom_step(
          data = red_df,
          mapping = aes(x = .data$x, y = .data$ecdf_value,
            group = interaction(.data$group, .data$segment)),
          color = color["highlight"],
          linewidth = linewidth + 0.8
        )
      }

      if (nrow(red_points_df) > 0) {
        p <- p + geom_point(
          data = red_points_df,
          mapping = aes(x = .data$x, y = .data$ecdf_value),
          color = color["highlight"],
          size = linewidth + 1
        )
      }

      if (isTRUE(help_text) && nrow(ann_df) > 0) {
        label_size <- help_text_shrinkage * bayesplot_theme_get()$text@size / ggplot2::.pt
        p <- p + geom_text(
          data = ann_df,
          mapping = aes(x = .data$x, y = .data$y, label = .data$label),
          hjust = -0.05,
          vjust = 1.5,
          color = "black",
          parse = TRUE,
          size = label_size
        )
      }

      return(
        p +
          labs(y = ifelse(plot_diff, "ECDF difference", "ECDF"), x = "PIT") +
          yaxis_ticks(FALSE) +
          bayesplot_theme_get() +
          facet_wrap("group") +
          scale_color_ppc() +
          force_axes_in_facets()
      )
    }

    # independent method
    gammas <- lapply(group_levels, function(g) {
      N_g <- sum(data$group == g)
      adjust_gamma(
        N = N_g,
        K = K %||% N_g,
        prob = prob,
        interpolate_adj = interpolate_adj
      )
    })
    names(gammas) <- group_levels

    data <- data %>%
      dplyr::group_by(.data$group) %>%
      dplyr::group_map(
        ~ data.frame(
          ecdf_value = ecdf(.x$pit)(seq(0, 1, length.out = K %||% nrow(.x))),
          group = .y[1],
          lims_upper = ecdf_intervals(
            gamma = gammas[[unlist(.y[1])]],
            N = nrow(.x),
            K = K %||% nrow(.x)
          )$upper[-1] / nrow(.x),
          lims_lower = ecdf_intervals(
            gamma = gammas[[unlist(.y[1])]],
            N = nrow(.x),
            K = K %||% nrow(.x)
          )$lower[-1] / nrow(.x),
          x = seq(0, 1, length.out = K %||% nrow(.x))
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
