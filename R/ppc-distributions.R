#' PPC distributions
#'
#' Compare the empirical distribution of the data `y` to the distributions
#' of simulated/replicated data `yrep` from the posterior predictive
#' distribution. See the **Plot Descriptions** section, below,
#' for details.
#'
#' @name PPC-distributions
#' @family PPCs
#'
#' @template args-y-yrep
#' @template args-hist
#' @template args-hist-freq
#' @template args-dens
#' @param size,alpha Passed to the appropriate geom to control the appearance of
#'   the `yrep` distributions.
#' @param ... Currently unused.
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
#'   \item{`ppc_freqpoly_grouped()`}{
#'    A separate frequency polygon is plotted for each level of a grouping
#'    variable for `y` and each dataset (row) in `yrep`. For this plot
#'    `yrep` should therefore contain only a small number of rows. See the
#'    **Examples** section.
#'   }
#'   \item{`ppc_ecdf_overlay(), ppc_dens_overlay(),
#'          ppc_ecdf_overlay_grouped(), ppc_dens_overlay_grouped()`}{
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
#' }
#'
#' @template reference-vis-paper
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#'
#' @examples
#' color_scheme_set("brightblue")
#' y <- example_y_data()
#' yrep <- example_yrep_draws()
#' dim(yrep)
#' ppc_dens_overlay(y, yrep[1:25, ])
#'
#' \donttest{
#' # ppc_ecdf_overlay with continuous data (set discrete=TRUE if discrete data)
#' ppc_ecdf_overlay(y, yrep[sample(nrow(yrep), 25), ])
#' }
#'
#' # for ppc_hist,dens,freqpoly,boxplot definitely use a subset yrep rows so
#' # only a few (instead of nrow(yrep)) histograms are plotted
#' ppc_hist(y, yrep[1:8, ])
#'
#' \donttest{
#' color_scheme_set("red")
#' ppc_boxplot(y, yrep[1:8, ])
#'
#' # wizard hat plot
#' color_scheme_set("blue")
#' ppc_dens(y, yrep[200:202, ])
#' }
#'
#' ppc_freqpoly(y, yrep[1:3,], alpha = 0.1, size = 1, binwidth = 5)
#'
#' # if groups are different sizes then the 'freq' argument can be useful
#' group <- example_group_data()
#' ppc_freqpoly_grouped(y, yrep[1:3,], group) + yaxis_text()
#' \donttest{
#' ppc_freqpoly_grouped(y, yrep[1:3,], group, freq = FALSE) + yaxis_text()
#' }
#'
#' # density and distribution overlays by group
#' ppc_dens_overlay_grouped(y, yrep[1:25, ], group = group)
#'
#' ppc_ecdf_overlay_grouped(y, yrep[1:25, ], group = group)
#'
#' # don't need to only use small number of rows for ppc_violin_grouped
#' # (as it pools yrep draws within groups)
#' color_scheme_set("gray")
#' ppc_violin_grouped(y, yrep, group, size = 1.5)
#' \donttest{
#' ppc_violin_grouped(y, yrep, group, alpha = 0)
#'
#' # change how y is drawn
#' ppc_violin_grouped(y, yrep, group, alpha = 0, y_draw = "points", y_size = 1.5)
#' ppc_violin_grouped(y, yrep, group, alpha = 0, y_draw = "both",
#'                    y_size = 1.5, y_alpha = 0.5, y_jitter = 0.33)
#' }
NULL



#' @rdname PPC-distributions
#' @export
ppc_data <- function(y, yrep, group = NULL) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  data <- melt_and_stack(y, yrep)

  if (!is.null(group)) {
    group <- validate_group(group, y)
    group_indices <- tibble::tibble(group, y_id = seq_along(group))
    data <- data %>%
      left_join(group_indices, by = "y_id") %>%
      select(.data$group, dplyr::everything())
  }

  data
}


#' @rdname PPC-distributions
#' @export
comparison_data <- function(y, yrep, pit, K) {
  if (missing(K)){
    if (!missing(y)) {
      K <- length(y)
    }
    else if (!missing(pit)) {
      K = ncol(pit)
    }
    else {
      K = ncol(yrep)
    }
  }
  z <- seq(0,1, length.out = K + 1)
  if (!missing(pit)) {
    pit <- validate_pit(pit)
    ecdfs <- t(apply(pit, 1, function(row) ecdf(row)(z)))
    ecdfs <- melt_and_stack(ecdfs[1,], ecdfs)
    ecdfs <- dplyr::filter(ecdfs, !ecdfs$is_y)
  } else if (missing(y)) {
    pit <- u_scale(validate_yrep(yrep, yrep[1, ], match_ncols = FALSE))
    if (nrow(yrep) < 2) {
      abort(paste("When both 'pit' and 'y' are missing, 'yrep' must include ",
                  "multiple rows to allow for sample comparison.", sep=""))
    }
    ecdfs <- t(apply(pit, 1, function(row) ecdf(row)(z)))
    ecdfs <- melt_and_stack(ecdfs[1,], ecdfs)
    ecdfs <- dplyr::filter(ecdfs, !ecdfs$is_y)
  } else {
    y <- validate_y(y)
    yrep <- validate_yrep(yrep, y, match_ncols = FALSE)
    pit <- u_scale(rbind(y,yrep))
    ecdfs <- t(apply(ecdfs, 1, function(row) ecdf(row)(z)))
    ecdfs <- melt_and_stack(ecdfs[1, ], ecdfs[2, nrow(yrep) + 1])
  }
  ecdfs
}


#' @rdname PPC-distributions
#' @export
ppc_hist <- function(y, yrep, ..., binwidth = NULL, breaks = NULL,
                     freq = TRUE) {
  check_ignored_arguments(...)
  data <- ppc_data(y, yrep)
  aes_list <- set_hist_aes(freq, fill = ~ is_y_label, color = ~ is_y_label)

  ggplot(data) +
    aes_list +
    geom_histogram(size = 0.25, binwidth = binwidth, breaks = breaks) +
    scale_fill_ppc_dist() +
    scale_color_ppc_dist() +
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
#' @param notch A logical scalar passed to [ggplot2::geom_boxplot()].
#'   Unlike for `geom_boxplot()`, the default is `notch=TRUE`.
ppc_boxplot <- function(y, yrep, ..., notch = TRUE, size = 0.5, alpha = 1) {
  check_ignored_arguments(...)
  data <- ppc_data(y, yrep)

  ggplot(data) +
    aes_(x = ~ rep_label, y = ~ value,
         fill = ~ is_y_label, color = ~ is_y_label) +
    geom_boxplot(
      notch = notch,
      size = size,
      alpha = alpha,
      outlier.alpha = 2 / 3) +
    scale_fill_ppc_dist() +
    scale_color_ppc_dist() +
    bayesplot_theme_get() +
    yaxis_title(FALSE) +
    xaxis_ticks(FALSE) +
    xaxis_text(FALSE) +
    xaxis_title(FALSE)
}



#' @rdname PPC-distributions
#' @export
ppc_freqpoly <- function(y, yrep, ...,
                         binwidth = NULL,
                         freq = TRUE,
                         size = 0.25,
                         alpha = 1) {
  check_ignored_arguments(...)
  data <- ppc_data(y, yrep)
  aes_list <- set_hist_aes(freq, fill = ~ is_y_label, color = ~ is_y_label)

  ggplot(data) +
    aes_list +
    aes_(x = ~ value, fill = ~ is_y_label, color = ~ is_y_label) +
    geom_area(stat = "bin", binwidth = binwidth, size = size, alpha = alpha) +
    scale_fill_ppc_dist() +
    scale_color_ppc_dist() +
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

#' @rdname PPC-distributions
#' @export
#' @template args-group

ppc_freqpoly_grouped <- function(y, yrep, group, ..., binwidth = NULL,
                                 freq = TRUE, size = 0.25, alpha = 1) {
    check_ignored_arguments(...)
    data <- ppc_data(y, yrep, group)
    aes_list <- set_hist_aes(freq)

    ggplot(data) +
      aes_list +
      geom_area(aes_(color = ~ is_y_label, fill = ~ is_y_label),
                stat = "bin", size = size, alpha = alpha,
                binwidth = binwidth, na.rm = TRUE) +
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


#' @rdname PPC-distributions
#' @export
ppc_dens <- function(y, yrep, ..., trim = FALSE, size = 0.5, alpha = 1) {
  check_ignored_arguments(...)
  data <- ppc_data(y, yrep)

  ggplot(data) +
    aes_(x = ~ value, fill = ~ is_y_label, color = ~ is_y_label) +
    geom_density(size = size, alpha = alpha, trim = trim) +
    scale_fill_ppc_dist() +
    scale_color_ppc_dist() +
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
#' @template args-density-controls
ppc_dens_overlay <- function(
  y,
  yrep,
  ...,
  size = 0.25,
  alpha = 0.7,
  trim = FALSE,
  bw = "nrd0",
  adjust = 1,
  kernel = "gaussian",
  n_dens = 1024
) {
  check_ignored_arguments(...)
  data <- ppc_data(y, yrep)

  ggplot(data) +
    aes_(x = ~ value) +
    stat_density(
      aes_(group = ~ rep_id, color = "yrep"),
      data = function(x) dplyr::filter(x, !.data$is_y),
      geom = "line",
      position = "identity",
      size = size,
      alpha = alpha,
      trim = trim,
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n_dens
    ) +
    stat_density(
      aes_(color = "y"),
      data = function(x) dplyr::filter(x, .data$is_y),
      geom = "line",
      position = "identity",
      lineend = "round",
      size = 1,
      trim = trim,
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n_dens
    ) +
    scale_color_ppc_dist() +
    bayesplot_theme_get() +
    xlab(y_label()) +
    dont_expand_axes() +
    yaxis_title(FALSE) +
    xaxis_title(FALSE) +
    yaxis_text(FALSE) +
    yaxis_ticks(FALSE)
}

#' @rdname PPC-distributions
#' @export
#' @template args-density-controls
ppc_dens_overlay_grouped <- function(
  y,
  yrep,
  group,
  ...,
  size = 0.25,
  alpha = 0.7,
  trim = FALSE,
  bw = "nrd0",
  adjust = 1,
  kernel = "gaussian",
  n_dens = 1024
) {
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
ppc_ecdf_overlay <- function(
  y,
  yrep,
  ...,
  discrete = FALSE,
  pad = TRUE,
  size = 0.25,
  alpha = 0.7
) {
  check_ignored_arguments(...)
  data <- ppc_data(y, yrep)

  ggplot(data) +
    aes_(x = ~ value) +
    hline_at(
      0.5,
      size = 0.1,
      linetype = 2,
      color = get_color("dh")
    ) +
    hline_at(
      c(0, 1),
      size = 0.2,
      linetype = 2,
      color = get_color("dh")
    ) +
    stat_ecdf(
      data = function(x) dplyr::filter(x, !.data$is_y),
      mapping = aes_(group = ~ rep_id, color = "yrep"),
      geom = if (discrete) "step" else "line",
      size = size,
      alpha = alpha,
      pad = pad
    ) +
    stat_ecdf(
      data = function(x) dplyr::filter(x, .data$is_y),
      mapping = aes_(color = "y"),
      geom = if (discrete) "step" else "line",
      size = 1,
      pad = pad
    ) +
    scale_color_ppc_dist() +
    xlab(y_label()) +
    scale_y_continuous(breaks = c(0, 0.5, 1)) +
    yaxis_title(FALSE) +
    xaxis_title(FALSE) +
    yaxis_ticks(FALSE) +
    bayesplot_theme_get()
}

#' @export
#' @rdname PPC-distributions
ppc_ecdf_overlay_grouped <- function(
  y,
  yrep,
  group,
  ...,
  discrete = FALSE,
  pad = TRUE,
  size = 0.25,
  alpha = 0.7
) {
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

#' @export
#' @rdname PPC-distributions
#' @param pit For 'ppc_ecdf_intervals' and 'ppc_ecdf_diff_intervals', the PIT
#'   values of one or more samples can be provided directly making 'y' and
#'   yrep' optional.
#' @param conf_level For 'ppc_ecdf_intervals' and 'ppc_ecdf_diff_intervals',
#'   the desired simultaneous confidence level of the bands to be drawn.
#' @param K, For 'ppc_ecdf_intervals' and 'ppc_ecdf_diff_intervals', number of
#'   equally spaced evaluation points for the ECDF plot. Affects the granularity
#' of the plot and can significantly speed up the computation of the confidence
#' bands. The default is 'K = ncol(yrep)', or 'K = ncol(pit)' if PIT values are
#' provided instead.
ppc_ecdf_intervals <- function(
  y,
  yrep,
  pit,
  gamma,
  K,
  ...,
  conf_level = 0.95,
  size = 0.25,
  alpha = 0.7
) {
  check_ignored_arguments(...)
  data <- comparison_data(y, yrep, pit, K)
  if (missing(K)) {
    K <- max(data$y_id)
  }
  if (missing(gamma)) {
    gamma <- adjust_gamma(
      N = max(data$y_id),
      L = max(data$rep_id) + any(data$is_y),
      K = K,
      conf_level = conf_level
    )
  }
  limits <- ecdf_intervals(gamma, K, L)
  z <- 0:K / K
  fig <- ggplot(data, mapping = aes_(x = rep(L, z))) +
    geom_step(
      data = function(x) dplyr::filter(x, !.data$is_y),
      aes_(group = ~ variable, y = ~ value, color = ~ variable)
    ) +
    geom_line(
      data = data.frame(x = c(0, 1), y = c(0, 1)),
      mapping = aes_(x = ~ x, y = ~ y),
      alpha = 0.5 * alpha,
      color = 'gray'
    ) +
    geom_step(aes_(y = limits$upper)) +
    geom_step(aes_(y = limits$lower))
  if any(data$is_y) {
    fig <- fig + geom_step(
      data = function(x) dplyr::filter(x, .data$is_y),
      aes_(y = ~ value)
    )
  }
  fig + scale_y_continuous(breaks = c(0, 0.5, 1)) +
    yaxis_title(FALSE) +
    xaxis_title(FALSE) +
    yaxis_ticks(FALSE) +
    bayesplot_theme_get()






}


#' @export
#' @rdname PPC-distributions
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
ppc_violin_grouped <- function(y, yrep, group, ..., probs = c(0.1, 0.5, 0.9),
                               size = 1, alpha = 1,
                               y_draw = c("violin", "points", "both"),
                               y_size = 1, y_alpha = 1, y_jitter = 0.1) {
  check_ignored_arguments(...)
  data <- ppc_data(y, yrep, group)

  y_draw <- match.arg(y_draw)
  y_violin <- y_draw %in% c("violin", "both")
  y_points <- y_draw %in% c("points", "both")

  args_violin_yrep <- list(
    data = function(x) dplyr::filter(x, !.data$is_y),
    aes_(fill = "yrep", color = "yrep"),
    draw_quantiles = probs,
    alpha = alpha,
    size = size
  )

  args_violin_y <- list(
    data = function(x) dplyr::filter(x, .data$is_y),
    aes_(fill = "y", color = "y"),
    show.legend = FALSE,
    alpha = 0
  )

  args_jitter_y <- list(
    data = function(x) dplyr::filter(x, .data$is_y),
    aes_(fill = "y", color = "y"),
    shape = 21,
    alpha = y_alpha,
    size = y_size,
    width = y_jitter,
    height = 0,
    show.legend = FALSE
  )

  violin_y_func <- if (y_violin) geom_violin else geom_ignore
  jitter_y_func <- if (y_points) geom_jitter else geom_ignore

  layer_violin_yrep <- do.call(geom_violin, args_violin_yrep)
  layer_violin_y <- do.call(violin_y_func, args_violin_y)
  layer_jitter_y <- do.call(jitter_y_func, args_jitter_y)

  ggplot(data) +
    aes_(x = ~ group, y = ~ value) +
    layer_violin_yrep +
    layer_violin_y +
    layer_jitter_y +
    scale_fill_ppc_dist(values = c(NA, get_color("l"))) +
    scale_color_ppc_dist() +
    labs(x = "Group", y = yrep_label()) +
    yaxis_title(FALSE) +
    xaxis_title(FALSE) +
    bayesplot_theme_get()
}


# internal ----------------------------------------------------------------
scale_color_ppc_dist <- function(name = NULL, values = NULL, labels = NULL) {
  scale_color_manual(
    name = name %||% "",
    values = values %||% get_color(c("dh", "lh")),
    labels = labels %||% c(y_label(), yrep_label())
  )
}
scale_fill_ppc_dist <- function(name = NULL, values = NULL, labels = NULL) {
  scale_fill_manual(
    name = name %||% "",
    values = values %||% get_color(c("d", "l")),
    labels = labels %||% c(y_label(), yrep_label())
  )
}
