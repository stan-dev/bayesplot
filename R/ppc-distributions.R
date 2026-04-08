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
#'    See Säilynoja et al. (2021) for more details.
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
#' @param interpolate_adj For `ppc_pit_ecdf()` and `ppc_pit_ecdf_grouped()`
#'   when `method = "independent"`,
#'   a boolean defining if the simultaneous confidence bands should be 
#'   interpolated based on precomputed values rather than computed exactly. 
#'   Computing the bands may be computationally intensive and the approximation 
#'   gives a fast method for assessing the ECDF trajectory. The default is to use
#'   interpolation if `K` is greater than 200.
#' @param method For `ppc_pit_ecdf()` and `ppc_pit_ecdf_grouped()`, the method
#'   used to calculate the
#'   uniformity test:
#'   * `"independent"`: (default) Assumes independence (Säilynoja et al., 2022).
#'   * `"correlated"`: Accounts for correlation (Tesso & Vehtari, 2026).
#' @param test For `ppc_pit_ecdf()` and `ppc_pit_ecdf_grouped()` when
#'   `method = "correlated"`, which
#'   dependence-aware test to use: `"POT"`, `"PRIT"`, or `"PIET"`.
#'   Defaults to `"POT"`.
#' @param gamma For `ppc_pit_ecdf()` and `ppc_pit_ecdf_grouped()` when
#'   `method = "correlated"`, tolerance
#'   threshold controlling how strongly suspicious points are flagged. Larger
#'   values (gamma > 0) emphasizes points with larger deviations. If `NULL`, automatically
#'   determined based on p-value.
#' @param linewidth For `ppc_pit_ecdf()` and `ppc_pit_ecdf_grouped()` when
#'   `method = "correlated"`, the line width of the ECDF and highlighting
#'   points. Defaults to 0.3.
#' @param color For `ppc_pit_ecdf()` and `ppc_pit_ecdf_grouped()` when
#'   `method = "correlated"`, a vector
#'   with base color and highlight color for the ECDF plot. Defaults to
#'   `c(ecdf = "grey60", highlight = "red")`. The first element is used for
#'   the main ECDF line, the second for highlighted suspicious regions.
#' @param help_text For `ppc_pit_ecdf()` and `ppc_pit_ecdf_grouped()` when
#'   `method = "correlated"`, a boolean defining whether to add informative
#'   text to the plot. Defaults to `TRUE`.
#' @param pareto_pit For `ppc_pit_ecdf()` and `ppc_pit_ecdf_grouped()`, a
#'   boolean defining whether to compute the PIT values using Pareto-smoothed 
#'   importance sampling (if `TRUE` and no pit values are provided). 
#'   Defaults to `TRUE` when `method = "correlated"` and `test` is `"POT"` or `"PIET"`.
#'   Otherwise defaults to `FALSE`. If `TRUE` requires the specification of `lw` or `psis_object`.
#'   The defaults should not be changed by the user, but the option is provided for developers.
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
                         method = NULL,
                         test = NULL,
                         gamma = NULL,
                         linewidth = NULL,
                         color = NULL,
                         help_text = NULL,
                         pareto_pit = NULL
                        ) {
  check_ignored_arguments(...,
    ok_args = c(
      "K", "pareto_pit", "pit", "prob", "plot_diff", 
      "interpolate_adj", "method", "test", "gamma", "linewidth", 
      "color", "help_text"
    )
  )

  .warn_ignored <- function(method_name, args) {
    inform(paste0(
      "As method = ", method_name, " specified; ignoring: ",
      paste(args, collapse = ", "), "."
    ))
  }

  if (is.null(method)) {
    inform(c(
      "i" = paste(
        "In the next major release, the default `method`",
        "will change to 'correlated'."
      ),
      "*" = paste(
        "To silence this message, explicitly set",
        "`method = 'independent'` or `method = 'correlated'`."
      )
    ))
    method <- "independent"
  } else {
    method <- match.arg(method, choices = c("independent", "correlated"))
    if (method == "independent") {
      inform("The 'independent' method is superseded by the 'correlated' method.")
    }
  }

  switch(method,
    "correlated" = {
      if (!is.null(interpolate_adj)) .warn_ignored("'correlated'", "interpolate_adj")
      
      test <- match.arg(test %||% "POT", choices = c("POT", "PRIT", "PIET"))
      alpha <- 1 - prob
      gamma <- gamma     %||% 0
      linewidth <- linewidth %||% 0.3
      color <- color     %||% c(ecdf = "grey60", highlight = "red")
      help_text <- help_text %||%  TRUE
      pareto_pit <- pareto_pit %||% is.null(pit) && test %in% c("POT", "PIET")
    },
    "independent" = {
      ignored <- c(
        if (!is.null(test)) "test",
        if (!is.null(gamma)) "gamma",
        if (!is.null(help_text)) "help_text"
      )
      if (length(ignored) > 0) .warn_ignored("'independent'", ignored)
      pareto_pit <- pareto_pit %||% FALSE
    }
  )

  if (isTRUE(pareto_pit) && is.null(pit)) {
    # --- Pareto-smoothed PIT ---
    suggested_package("rstantools")
    y    <- validate_y(y)
    yrep <- validate_predictions(yrep, length(y))
    pit <- posterior::pareto_pit(x = yrep, y = y, weights = NULL, log = TRUE)
    K   <- K %||% length(pit)

  } else if (!is.null(pit)) {
    # --- Pre-supplied PIT values ---
    pit <- validate_pit(pit)
    K   <- K %||% length(pit)
    ignored <- c(
      if (!missing(y) && !is.null(y)) "y",
      if (!missing(yrep) && !is.null(yrep)) "yrep"
    )
    if (length(ignored) > 0) {
      inform(paste0(
        "As 'pit' specified; ignoring: ",
        paste(ignored, collapse = ", "), "."
      ))
    }
  } else {
    # --- Empirical PIT ---'
    y    <- validate_y(y)
    yrep <- validate_predictions(yrep, length(y))
    pit <- ppc_data(y, yrep) |> 
      group_by(.data$y_id) |> 
      dplyr::group_map(
        ~ mean(.x$value[.x$is_y] > .x$value[!.x$is_y]) +
        runif(1, max = mean(.x$value[.x$is_y] == .x$value[!.x$is_y]))
      ) |> 
      unlist()
    K <- K %||% min(nrow(yrep) + 1, 1000)
  }

  n_obs <- length(pit)
  unit_interval <- seq(0, 1, length.out = K)
  ecdf_pit_fn <- ecdf(pit)
  y_label <- if (plot_diff) "ECDF difference" else "ECDF"

  if (method == "correlated") {
    test_res <- posterior::uniformity_test(pit = pit, test = test)
    p_value_CCT <- test_res$pvalue
    pointwise_contrib <- test_res$pointwise
    max_contrib <- max(pointwise_contrib)
    if (gamma < 0 || gamma > max_contrib) {
      stop(sprintf(
        "gamma must be in [0, %.2f], but gamma = %s was provided.",
        max_contrib, gamma
      ))
    }
    x_combined <- sort(unique(c(unit_interval, pit)))
    df_main <- tibble::tibble(
      x = x_combined,
      ecdf_val = ecdf_pit_fn(x_combined) - plot_diff * x_combined
    )
    pit_sorted <- sort(pit)
    df_pit <- tibble::tibble(
      pit = pit_sorted,
      ecdf_val = ecdf_pit_fn(pit_sorted) - plot_diff * pit_sorted
    )

    p <- ggplot() +
      geom_step(
        data = df_main, 
        mapping = aes(x = .data$x, y = .data$ecdf_val),
        show.legend = FALSE, 
        linewidth = linewidth, 
        color = color["ecdf"]
      ) +
      geom_segment(
        mapping = aes(x = 0, y = 0, xend = 1, yend = if (plot_diff) 0 else 1),
        linetype = "dashed",
        color = "darkgrey",
        linewidth = 0.3
      ) +
      labs(x = "PIT", y = y_label)

    if (p_value_CCT < alpha) {
      red_idx <- which(pointwise_contrib > gamma)
      
      if (length(red_idx) > 0) {
        df_red <- df_pit[red_idx, ]
        df_red$segment <- cumsum(c(1, diff(red_idx) != 1))
        seg_sizes <- stats::ave(df_red$pit, df_red$segment, FUN = length)
        df_isolated <- df_red[seg_sizes == 1, ]
        df_grouped <- df_red[seg_sizes > 1, ]

        if (nrow(df_grouped) > 0) {
          df_segments <- do.call(rbind, lapply(
            split(df_grouped, df_grouped$segment),
            function(grp) {
              pit_idx <- match(grp$pit, x_combined)
              idx_range <- seq(min(pit_idx), max(pit_idx))
              tibble::tibble(
                x = df_main$x[idx_range],
                ecdf_val = df_main$ecdf_val[idx_range],
                segment = grp$segment[1L]
              )
            }
          ))

          p <- p + geom_step(
            data = df_segments,
            mapping = aes(x = .data$x, y = .data$ecdf_val, group = .data$segment),
            color = color["highlight"],
            linewidth = linewidth + 0.8
          )
        }

        if (nrow(df_isolated) > 0) {
          p <- p + geom_point(
            data = df_isolated,
            aes(x = .data$pit, y = .data$ecdf_val),
            color = color["highlight"],
            size = linewidth + 1
          )
        }
      }
    }

    if (isTRUE(help_text)) {
      label_size <- 0.7 * bayesplot_theme_get()$text@size / ggplot2::.pt
      p <- p + annotate(
        "text",
        x = -Inf, y = Inf,
        label = sprintf("p[unif]^{%s} == '%s' ~ (alpha == '%.2f')", 
        test, fmt_p(p_value_CCT), alpha
      ),
        hjust = -0.05, 
        vjust = 1.5, 
        color = "black", 
        parse = TRUE, 
        size = label_size
      )
    }

    if (plot_diff) {
      epsilon = max(
        sqrt(log(2 / (1 - prob)) / (2 * n_obs)),
        max(abs(df_main$ecdf_val))
      )
      p <- p + scale_y_continuous(limits = c(-epsilon, epsilon))
    }

    p <- p +
      yaxis_ticks(FALSE) +
      scale_color_ppc() +
      bayesplot_theme_get()

    return(p)
  }

  gamma_indep <- adjust_gamma(
    N = n_obs, K = K, prob = prob, interpolate_adj = interpolate_adj
  )
  lims <- ecdf_intervals(gamma = gamma_indep, N = n_obs, K = K)
  lims_upper <- lims$upper[-1] / n_obs - plot_diff * unit_interval
  lims_lower <- lims$lower[-1] / n_obs - plot_diff * unit_interval
  ecdf_eval  <- ecdf_pit_fn(unit_interval) - plot_diff * unit_interval

  p <- ggplot() +
    geom_step(
      mapping = aes(x = unit_interval, y = lims_upper, color = "yrep"),
      linetype = "dashed",
      linewidth = 0.3,
      show.legend = FALSE
    ) +
    geom_step(
      mapping = aes(x = unit_interval, y = lims_lower, color = "yrep"),
      linetype = "dashed",
      linewidth = 0.3,
      show.legend = FALSE
    ) +
    geom_step(
      mapping = aes(x = unit_interval, y = ecdf_eval, color = "y"),
      linewidth = 0.5,
      show.legend = FALSE
    ) +
    labs(x = "PIT", y = y_label) +
    yaxis_ticks(FALSE) +
    scale_color_ppc() +
    bayesplot_theme_get()

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
           interpolate_adj = NULL,
           method = NULL,
           test = NULL,
           gamma = NULL,
           linewidth = NULL,
           color = NULL,
           help_text = NULL,
           pareto_pit = NULL) {
    check_ignored_arguments(...,
      ok_args = c("K", "pareto_pit", "pit", "prob", "plot_diff",
                  "interpolate_adj", "method", "test", "gamma",
                  "linewidth", "color", "help_text")
    )

    .warn_ignored <- function(method_name, args) {
      inform(paste0(
        "As method = ", method_name, " specified; ignoring: ",
        paste(args, collapse = ", "), "."
      ))
    }

    # Resolve and validate `method`
    if (is.null(method)) {
      inform(c(
        "i" = paste(
          "In the next major release, the default `method`",
          "will change to 'correlated'."
        ),
        "*" = paste(
          "To silence this message, explicitly set",
          "`method = 'independent'` or `method = 'correlated'`."
        )
      ))
      method <- "independent"
    } else {
      method <- match.arg(method, choices = c("independent", "correlated"))
      if (method == "independent") {
        inform("The 'independent' method is superseded by the 'correlated' method.")
      }
    }

    switch(method,
      "correlated" = {
        if (!is.null(interpolate_adj)) .warn_ignored("'correlated'", "interpolate_adj")
        test <- match.arg(test %||% "POT", choices = c("POT", "PRIT", "PIET"))
        alpha <- 1 - prob
        gamma <- gamma %||% 0
        linewidth <- linewidth %||% 0.3
        color <- color %||% c(ecdf = "grey60", highlight = "red")
        help_text <- help_text %||% TRUE
        pareto_pit <- pareto_pit %||% is.null(pit) && test %in% c("POT", "PIET")
      },
      "independent" = {
        ignored <- c(
          if (!is.null(test)) "test",
          if (!is.null(gamma)) "gamma",
          if (!is.null(help_text)) "help_text"
        )
        if (length(ignored) > 0) .warn_ignored("'independent'", ignored)
        pareto_pit <- pareto_pit %||% FALSE
      }
    )

    if (isTRUE(pareto_pit) && is.null(pit)) {
      suggested_package("rstantools")
      y <- validate_y(y)
      yrep <- validate_predictions(yrep, length(y))
      group <- validate_group(group, length(y))
      pit <- posterior::pareto_pit(x = yrep, y = y, weights = NULL, log = TRUE)
      K <- K %||% length(pit)
    } else if (!is.null(pit)) {
      pit <- validate_pit(pit)
      group <- validate_group(group, length(pit))
      K <- K %||% length(pit)
      ignored <- c(
        if (!missing(y) && !is.null(y)) "y",
        if (!missing(yrep) && !is.null(yrep)) "yrep"
      )
      if (length(ignored) > 0) {
        inform(paste0(
          "As 'pit' specified; ignoring: ",
          paste(ignored, collapse = ", "), "."
        ))
      }
    } else {
      y <- validate_y(y)
      yrep <- validate_predictions(yrep, length(y))
      group <- validate_group(group, length(y))
      pit <- ppc_data(y, yrep, group) %>%
        group_by(.data$y_id) %>%
        dplyr::group_map(
          ~ mean(.x$value[.x$is_y] > .x$value[!.x$is_y]) +
          runif(1, max = mean(.x$value[.x$is_y] == .x$value[!.x$is_y]))
        ) %>%
        unlist()
      K <- K %||% min(nrow(yrep) + 1, 1000)
    }

    data <- data.frame(pit = pit, group = group, stringsAsFactors = FALSE)
    group_levels <- unique(data$group)

    if (method == "correlated") {
      data_cor <- dplyr::group_by(data, .data$group) %>%
        dplyr::group_map(function(.x, .y) {
          n_obs <- nrow(.x)
          K_g <- K %||% n_obs
          unit_interval <- seq(0, 1, length.out = K_g)
          ecdf_pit_fn <- ecdf(.x$pit)
          x_combined <- sort(unique(c(unit_interval, .x$pit)))
          df_main <- data.frame(
            x = x_combined,
            ecdf_value = ecdf_pit_fn(x_combined) - plot_diff * x_combined,
            group = .y[[1]],
            stringsAsFactors = FALSE
          )

          test_res <- posterior::uniformity_test(pit = .x$pit, test = test)
          p_value_CCT <- test_res$pvalue
          pointwise_contrib <- test_res$pointwise
          max_contrib <- max(pointwise_contrib)
          if (gamma < 0 || gamma > max_contrib) {
            stop(sprintf(
              "gamma must be in [0, %.2f], but gamma = %s was provided.",
              max_contrib, gamma
            ))
          }

          red <- NULL
          red_points <- NULL
          if (p_value_CCT < alpha) {
            red_idx <- which(pointwise_contrib > gamma)
            if (length(red_idx) > 0) {
              pit_sorted <- sort(.x$pit)
              df_pit <- data.frame(
                pit = pit_sorted,
                ecdf_value = ecdf_pit_fn(pit_sorted),
                stringsAsFactors = FALSE
              )
              df_red <- df_pit[red_idx, , drop = FALSE]
              df_red$segment <- cumsum(c(1, diff(red_idx) != 1))
              seg_sizes <- stats::ave(df_red$pit, df_red$segment, FUN = length)
              df_isolated <- df_red[seg_sizes == 1, , drop = FALSE]
              df_grouped <- df_red[seg_sizes > 1, , drop = FALSE]

              if (nrow(df_grouped) > 0) {
                red <- do.call(rbind, lapply(
                  split(df_grouped, df_grouped$segment),
                  function(grp) {
                    pit_idx <- match(grp$pit, x_combined)
                    idx_range <- seq(min(pit_idx), max(pit_idx))
                    data.frame(
                      x = x_combined[idx_range],
                      ecdf_value = ecdf_pit_fn(x_combined[idx_range]) -
                        plot_diff * x_combined[idx_range],
                      segment = grp$segment[1],
                      group = .y[[1]],
                      stringsAsFactors = FALSE
                    )
                  }
                ))
              }

              if (nrow(df_isolated) > 0) {
                red_points <- data.frame(
                  x = df_isolated$pit,
                  ecdf_value = df_isolated$ecdf_value - plot_diff * df_isolated$pit,
                  group = .y[[1]],
                  stringsAsFactors = FALSE
                )
              }
            }
          }

          ann <- NULL
          if (isTRUE(help_text)) {
            ann <- data.frame(
              group = .y[[1]],
              x = -Inf,
              y = Inf,
              label = sprintf(
                "p[unif]^{%s} == '%s' ~ (alpha == '%.2f')",
                test, fmt_p(p_value_CCT), alpha
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
          mapping = aes(x = .data$x, y = .data$ecdf_value, group = interaction(.data$group, .data$segment)),
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
        label_size <- 0.7 * bayesplot_theme_get()$text@size / ggplot2::.pt
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
          labs(y = if (plot_diff) "ECDF difference" else "ECDF", x = "PIT") +
          yaxis_ticks(FALSE) +
          bayesplot_theme_get() +
          facet_wrap("group") +
          scale_color_ppc() +
          force_axes_in_facets()
      )
    }

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
