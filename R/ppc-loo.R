#' LOO predictive checks
#'
#' Leave-One-Out (LOO) predictive checks. See the **Plot Descriptions** section,
#' below, and [Gabry et al. (2019)](https://github.com/jgabry/bayes-vis-paper#readme)
#' for details.
#'
#' @name PPC-loo
#' @family PPCs
#' @template args-y-yrep
#' @param ... Currently unused.
#' @param lw A matrix of (smoothed) log weights with the same dimensions as
#'   `yrep`. See [loo::psis()] and the associated `weights()` method as well as
#'   the **Examples** section, below. If `lw` is not specified then
#'   `psis_object` can be provided and log weights will be extracted.
#' @param psis_object If using **loo** version `2.0.0` or greater, an
#'   object returned by the `psis()` function (or by the `loo()` function
#'   with argument `save_psis` set to `TRUE`).
#' @param alpha,size,fatten,linewidth Arguments passed to code geoms to control
#'   plot aesthetics. For `ppc_loo_pit_qq()` and `ppc_loo_pit_overlay()`,`size`
#'   and `alpha` are passed to [ggplot2::geom_point()] and
#'   [ggplot2::geom_density()], respectively. For `ppc_loo_intervals()`, `size`
#'   `linewidth` and `fatten` are passed to [ggplot2::geom_pointrange()]. For
#'   `ppc_loo_ribbon()`, `alpha` and `size`  are passed to
#'   [ggplot2::geom_ribbon()].
#'
#' @template return-ggplot
#'
#' @section Plot Descriptions:
#' \describe{
#' \item{`ppc_loo_pit_overlay()`, `ppc_loo_pit_qq()`, `ppc_loo_pit_ecdf()`}{
#'  The calibration of marginal predictions can be assessed using probability
#'  integral transformation (PIT) checks. LOO improves the check by avoiding the
#'  double use of data. See the section on marginal predictive checks in Gelman
#'  et al. (2013, p. 152--153) and section 5 of Gabry et al. (2019) for an
#'  example of using **bayesplot** for these checks.
#'
#'  The LOO PIT values are asymptotically uniform (for continuous data) if the
#'  model is calibrated. The `ppc_loo_pit_overlay()` function creates a plot
#'  comparing the density of the LOO PITs (thick line) to the density estimates
#'  of many simulated data sets from the standard uniform distribution (thin
#'  lines). See Gabry et al. (2019) for an example of interpreting the shape of
#'  the miscalibration that can be observed in these plots.
#'
#'  The `ppc_loo_pit_qq()` function provides an alternative visualization of
#'  the miscalibration with a quantile-quantile (Q-Q) plot comparing the LOO
#'  PITs to the standard uniform distribution. Comparing to the uniform is not
#'  good for extreme probabilities close to 0 and 1, so it can sometimes be
#'  useful to set the `compare` argument to `"normal"`, which will
#'  produce a Q-Q plot comparing standard normal quantiles calculated from the
#'  PIT values to the theoretical standard normal quantiles. This can help see
#'  the (mis)calibration better for the extreme values. However, in most cases
#'  we have found that the overlaid density plot (`ppc_loo_pit_overlay()`)
#'  function will provide a clearer picture of calibration problems than the
#'  Q-Q plot.
#'
#'  The `ppc_loo_pit_ecdf()` function visualizes the empirical cumulative
#'  distribution function (ECDF) of the LOO PITs overlaid with simultaneous
#'  confidence intervals for a standard uniform sample. For large samples,
#'  these confidence intervals are visually very narrow. Setting the
#'  `plot_diff` argument to `TRUE` transforms the plot to display the
#'  difference of the ECDF and the theoretical expectation, which can aid in
#'  the visual assessment of calibration.
#' }
#' \item{`ppc_loo_intervals()`, `ppc_loo_ribbon()`}{
#'  Similar to [ppc_intervals()] and [ppc_ribbon()] but the intervals are for
#'  the LOO predictive distribution.
#' }
#' }
#'
#' @templateVar bdaRef (p. 152--153)
#' @template reference-bda
#' @template reference-vis-paper
#' @template reference-loo
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' library(loo)
#'
#' head(radon)
#' fit <- stan_lmer(
#'   log_radon ~ floor + log_uranium + floor:log_uranium
#'     + (1 + floor | county),
#'   data = radon,
#'   iter = 100,
#'   chains = 2,
#'   cores = 2
#' )
#' y <- radon$log_radon
#' yrep <- posterior_predict(fit)
#'
#' loo1 <- loo(fit, save_psis = TRUE, cores = 4)
#' psis1 <- loo1$psis_object
#' lw <- weights(psis1) # normalized log weights
#'
#' # marginal predictive check using LOO probability integral transform
#' color_scheme_set("orange")
#' ppc_loo_pit_overlay(y, yrep, lw = lw)
#'
#' ppc_loo_pit_qq(y, yrep, lw = lw)
#' ppc_loo_pit_qq(y, yrep, lw = lw, compare = "normal")
#'
#' # predictive calibration check using LOO probability integral transform
#' ppc_loo_pit_ecdf(y, yrep, lw)
#'
#' # With `plot_diff = TRUE` it is easier to assess the calibration.
#' ppc_loo_pit_ecdf(y, yrep, lw, plot_diff = TRUE)
#'
#' # can use the psis object instead of lw
#' ppc_loo_pit_qq(y, yrep, psis_object = psis1)
#'
#' # loo predictive intervals vs observations
#' keep_obs <- 1:50
#' ppc_loo_intervals(y, yrep, psis_object = psis1, subset = keep_obs)
#'
#' color_scheme_set("gray")
#' ppc_loo_intervals(y, yrep,
#'   psis_object = psis1, subset = keep_obs,
#'   order = "median"
#' )
#' }
#'
NULL

#' @rdname PPC-loo
#' @export
#' @param pit For `ppc_loo_pit_overlay()`, `ppc_loo_pit_qq()`, and
#'   `ppc_loo_pit_ecdf()` optionally a vector of precomputed PIT values that
#'   can be specified instead of `y`, `yrep`, and `lw` (these are all ignored
#'   if `pit` is specified). If not specified the PIT values are computed
#'   internally before plotting.
#' @param samples For `ppc_loo_pit_overlay()`, the number of data sets (each
#'   the same size as `y`) to simulate from the standard uniform
#'   distribution. The default is 100. The density estimate of each dataset is
#'   plotted as a thin line in the plot, with the density estimate of the LOO
#'   PITs overlaid as a thicker dark line.
#' @param compare For `ppc_loo_pit_qq()`, a string that can be either
#'   `"uniform"` or `"normal"`. If `"uniform"` (the default) the Q-Q plot
#'   compares computed PIT values to the standard uniform distribution. If
#'   `compare="normal"`, the Q-Q plot compares standard normal quantiles
#'   calculated from the PIT values to the theoretical standard normal
#'   quantiles.
#' @param trim Passed to [ggplot2::stat_density()].
#' @template args-density-controls
#' @param boundary_correction For `ppc_loo_pit_overlay()`, when set to `TRUE`
#'   (the default) the function will compute boundary corrected density values
#'   via convolution and a Gaussian filter, also known as the reflection method
#'   (Boneva et al., 1971). As a result, parameters controlling the standard
#'   kernel density estimation such as `adjust`, `kernel` and `n_dens` are
#'   ignored. NOTE: The current implementation only works well for continuous
#'   observations.
#' @param grid_len For `ppc_loo_pit_overlay()`, when `boundary_correction` is
#'   set to `TRUE` this parameter specifies the number of points used to
#'   generate the estimations. This is set to 512 by default.
#'
#' @references Boneva, L. I., Kendall, D., & Stefanov, I. (1971). Spline
#'   transformations: Three new diagnostic aids for the statistical
#'   data-analyst. *J. R. Stat. Soc. B* (Methodological), 33(1), 1-71.
#'   https://www.jstor.org/stable/2986005.
#'
ppc_loo_pit_overlay <- function(y,
                                yrep,
                                lw = NULL,
                                ...,
                                psis_object = NULL,
                                pit = NULL,
                                samples = 100,
                                size = 0.25,
                                alpha = 0.7,
                                boundary_correction = TRUE,
                                grid_len = 512,
                                bw = "nrd0",
                                trim = FALSE,
                                adjust = 1,
                                kernel = "gaussian",
                                n_dens = 1024) {
  check_ignored_arguments(..., ok_args = list("moment_match"))

  data <-
    ppc_loo_pit_data(
      y = y,
      yrep = yrep,
      lw = lw,
      psis_object = psis_object,
      pit = pit,
      samples = samples,
      bw = bw,
      boundary_correction = boundary_correction,
      grid_len = grid_len
    )

  if (!missing(y) && all(y %in% 0:1)) {
    warning(
      "This plot is not recommended for binary data. ",
      "For plots that are more suitable see ",
      "\nhttps://avehtari.github.io/modelselection/diabetes.html#44_calibration_of_predictions",
      call. = FALSE
    )
  }

  message(paste(
    "NOTE: The kernel density estimate assumes continuous observations",
    "and is not optimal for discrete observations."
  ))

  if (boundary_correction) {
    p <- ggplot(data) +
      aes(x = .data$x, y = .data$value) +
      geom_line(
        aes(group = .data$rep_id, color = "yrep"),
        data = function(x) dplyr::filter(x, !.data$is_y),
        alpha = alpha,
        linewidth = size,
        na.rm = TRUE
      ) +
      geom_line(
        aes(color = "y"),
        data = function(x) dplyr::filter(x, .data$is_y),
        linewidth = 1,
        lineend = "round",
        na.rm = TRUE
      ) +
      scale_x_continuous(
        limits = c(0, 1),
        expand = expansion(0, 0.01),
        breaks = seq(0, 1, by = 0.25),
        labels = c("0", "0.25", "0.5", "0.75", "1")
      )
  } else {
    p <- ggplot(data) +
      aes(x = .data$value) +
      stat_density(
        aes(group = .data$rep_id, color = "yrep"),
        data = function(x) dplyr::filter(x, !.data$is_y),
        geom = "line",
        position = "identity",
        linewidth = size,
        alpha = alpha,
        trim = trim,
        bw = bw,
        adjust = adjust,
        kernel = kernel,
        n = n_dens,
        na.rm = TRUE
      ) +
      stat_density(
        aes(color = "y"),
        data = function(x) dplyr::filter(x, .data$is_y),
        geom = "line",
        position = "identity",
        lineend = "round",
        linewidth = 1,
        trim = trim,
        bw = bw,
        adjust = adjust,
        kernel = kernel,
        n = n_dens,
        na.rm = TRUE
      ) +
      scale_x_continuous(
        limits = c(0.05, 0.95),
        expand = expansion(0, 0),
        breaks = seq(from = .1, to = .9, by = .2)
      )
  }

  p +
    scale_color_ppc(labels = c("PIT", "Unif")) +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, .25))
    ) +
    bayesplot_theme_get() +
    yaxis_title(FALSE) +
    xaxis_title(FALSE) +
    yaxis_text(FALSE) +
    yaxis_ticks(FALSE)
}

#' @rdname PPC-loo
#' @export
ppc_loo_pit_data <-
  function(y,
           yrep,
           lw = NULL,
           ...,
           psis_object = NULL,
           pit = NULL,
           samples = 100,
           bw = "nrd0",
           boundary_correction = TRUE,
           grid_len = 512) {
    if (!is.null(pit)) {
      stopifnot(is.numeric(pit), is_vector_or_1Darray(pit))
      inform("'pit' specified so ignoring 'y','yrep','lw' if specified.")
    } else {
      suggested_package("rstantools")
      y <- validate_y(y)
      yrep <- validate_predictions(yrep, length(y))
      lw <- .get_lw(lw, psis_object)
      stopifnot(identical(dim(yrep), dim(lw)))
      pit <- rstantools::loo_pit(object = yrep, y = y, lw = lw)
    }

    if (!boundary_correction) {
      unifs <- matrix(runif(length(pit) * samples), nrow = samples)
      data <- ppc_data(pit, unifs)
    } else {
      unifs <- matrix(runif(grid_len * samples), nrow = samples)
      ref_list <- .ref_kde_correction(unifs, bw = bw, grid_len = grid_len)
      pit_list <- .kde_correction(pit, bw = bw, grid_len = grid_len)

      pit <- pit_list$bc_pvals
      unifs <- ref_list$unifs
      xs <- c(pit_list$xs, ref_list$xs)

      data <-
        ppc_data(pit, unifs) %>%
        dplyr::arrange(.data$rep_id) %>%
        mutate(x = xs)
    }
    data
  }

#' @rdname PPC-loo
#' @export
ppc_loo_pit_qq <- function(y,
                           yrep,
                           lw = NULL,
                           ...,
                           psis_object = NULL,
                           pit = NULL,
                           compare = c("uniform", "normal"),
                           size = 2,
                           alpha = 1) {
  check_ignored_arguments(..., ok_args = list("moment_match"))

  compare <- match.arg(compare)
  if (!is.null(pit)) {
    stopifnot(is.numeric(pit), is_vector_or_1Darray(pit))
    inform("'pit' specified so ignoring 'y','yrep','lw' if specified.")
  } else {
    suggested_package("rstantools")
    y <- validate_y(y)
    yrep <- validate_predictions(yrep, length(y))
    lw <- .get_lw(lw, psis_object)
    stopifnot(identical(dim(yrep), dim(lw)))
    pit <- rstantools::loo_pit(object = yrep, y = y, lw = lw)
  }

  if (compare == "uniform") {
    theoretical <- stats::qunif
    x_lab <- "Uniform"
    y_lab <- "LOO-PIT"
  } else {
    pit <- as.vector(stats::qnorm(pit))
    theoretical <- stats::qnorm
    x_lab <- "Normal"
    y_lab <- "LOO-PIT (standard normal quantiles)"
  }

  qq <- ggplot(data.frame(p = pit)) +
    geom_qq(
      aes(sample = .data$p),
      distribution = theoretical,
      color = get_color("m"),
      size = size,
      alpha = alpha
    ) +
    geom_abline(
      linetype = 2,
      color = "black"
    ) +
    bayesplot_theme_get() +
    labs(x = x_lab, y = y_lab)
  if (compare == "uniform") {
    qq + lims(x = c(0, 1), y = c(0, 1))
  } else {
    qq
  }
}

#' @rdname PPC-loo
#' @export
#' @param K For `ppc_loo_pit_ecdf()` an optional integer defining the number
#'  of equally spaced evaluation points for the PIT-ECDF. Reducing K when
#'  using `interpolate_adj = FALSE` makes computing the confidence bands
#'  faster. If `pit` is supplied, defaults to `length(pit)`, otherwise
#'  `yrep` determines the maximum accuracy of the estimated PIT values and
#'  `K` is set to `min(nrow(yrep) + 1, 1000)`.
#' @param plot_diff For `ppc_loo_pit_ecdf()`, a boolean defining whether to
#'   plot the difference between the observed PIT-ECDF and the theoretical
#'   expectation for uniform PIT values rather than plotting the regular ECDF.
#'   The default is `FALSE`, but for large samples we recommend setting
#'   `plot_diff = TRUE` to better use the plot area.
#' @param interpolate_adj For `ppc_loo_pit_ecdf()`, a boolean defining if the
#'   simultaneous confidence bands should be interpolated based on precomputed
#'   values rather than computed exactly. Computing the bands may be
#'   computationally intensive and the approximation gives a fast method for
#'   assessing the ECDF trajectory. The default is to use interpolation if `K`
#'   is greater than 200.
ppc_loo_pit_ecdf <- function(y,
                             yrep,
                             lw = NULL,
                             ...,
                             psis_object = NULL,
                             pit = NULL,
                             K = NULL,
                             prob = .99,
                             plot_diff = FALSE,
                             interpolate_adj = NULL) {
  check_ignored_arguments(..., ok_args = list("moment_match"))

  if (!is.null(pit)) {
    inform("'pit' specified so ignoring 'y','yrep','lw' if specified.")
    pit <- validate_pit(pit)
    if (is.null(K)) {
      K <- length(pit)
    }
  } else {
    suggested_package("rstantools")
    y <- validate_y(y)
    yrep <- validate_predictions(yrep, length(y))
    lw <- .get_lw(lw, psis_object)
    stopifnot(identical(dim(yrep), dim(lw)))
    pit <- pmin(1, rstantools::loo_pit(object = yrep, y = y, lw = lw))
    if (is.null(K)) {
      K <- min(nrow(yrep) + 1, 1000)
    }
  }

  n_obs <- length(pit)
  gamma <- adjust_gamma(
    N = n_obs,
    K = K,
    prob = prob,
    interpolate_adj = interpolate_adj
  )
  lims <- ecdf_intervals(gamma = gamma, N = n_obs, K = K)
  ggplot() +
    aes(
      x = seq(0, 1, length.out = K),
      y = ecdf(pit)(seq(0, 1, length.out = K)) -
        (plot_diff == TRUE) * seq(0, 1, length.out = K),
      color = "y"
    ) +
    geom_step(show.legend = FALSE) +
    geom_step(
      aes(
        y = lims$upper[-1] / n_obs -
          (plot_diff == TRUE) * seq(0, 1, length.out = K),
        color = "yrep"
      ),
      linetype = 2, show.legend = FALSE
    ) +
    geom_step(
      aes(
        y = lims$lower[-1] / n_obs -
          (plot_diff == TRUE) * seq(0, 1, length.out = K),
        color = "yrep"
      ),
      linetype = 2, show.legend = FALSE
    ) +
    labs(y = ifelse(plot_diff, "ECDF difference", "ECDF"), x = "LOO PIT") +
    yaxis_ticks(FALSE) +
    scale_color_ppc() +
    bayesplot_theme_get()
}


#' @rdname PPC-loo
#' @export
ppc_loo_pit <-
  function(y,
           yrep,
           lw,
           pit = NULL,
           compare = c("uniform", "normal"),
           ...,
           size = 2,
           alpha = 1) {
    .Deprecated("ppc_loo_pit_qq or ppc_loo_pit_overlay")
    ppc_loo_pit_qq(
      y = y,
      yrep = yrep,
      lw = lw,
      pit = pit,
      compare = compare,
      size = size,
      alpha = alpha,
      ...
    )
  }


#' @rdname PPC-loo
#' @export
#' @param intervals For `ppc_loo_intervals()` and `ppc_loo_ribbon()`, optionally
#'   a matrix of pre-computed LOO predictive intervals that can be specified
#'   instead of `yrep` (ignored if `intervals` is specified). If not specified
#'   the intervals are computed internally before plotting. If specified,
#'   `intervals` must be a matrix with number of rows equal to the number of
#'   data points and five columns in the following order: lower outer interval,
#'   lower inner interval, median (50%), upper inner interval and upper outer
#'   interval (column names are ignored).
#' @param order For `ppc_loo_intervals()`, a string indicating how to arrange
#'   the plotted intervals. The default (`"index"`) is to plot them in the
#'   order of the observations. The alternative (`"median"`) arranges them
#'   by median value from smallest (left) to largest (right).
#' @param prob,prob_outer Values between `0` and `1` indicating the desired
#'   probability mass to include in the inner and outer intervals. The defaults
#'   are `prob=0.5` and `prob_outer=0.9` for `ppc_loo_intervals()` and
#'   `prob = 0.99` for `ppc_loo_pit_ecdf()`.
#' @param subset For `ppc_loo_intervals()` and `ppc_loo_ribbon()`, an optional
#'   integer vector indicating which observations in `y` (and `yrep`) to
#'   include. Dropping observations from `y` and `yrep` manually before passing
#'   them to the plotting function will not work because the dimensions will not
#'   match up with the dimensions of `psis_object`, but if all of `y` and `yrep`
#'   are passed along with `subset` then **bayesplot** can do the subsetting
#'   internally for `y`, `yrep` *and* `psis_object`. See the **Examples**
#'   section for a demonstration.
#'
ppc_loo_intervals <-
  function(y,
           yrep,
           psis_object,
           ...,
           subset = NULL,
           intervals = NULL,
           prob = 0.5,
           prob_outer = 0.9,
           alpha = 0.33,
           size = 1,
           fatten = 2.5,
           linewidth = 1,
           order = c("index", "median")) {
    check_ignored_arguments(..., ok_args = list("moment_match"))
    y <- validate_y(y)
    order_by_median <- match.arg(order) == "median"
    if (!is.null(intervals)) {
      stopifnot(is.matrix(intervals), ncol(intervals) %in% c(3, 5))
      inform(paste(
        "'intervals' specified so ignoring",
        "'yrep', 'psis_object', 'subset', if specified."
      ))
      if (ncol(intervals) == 3) {
        intervals <- cbind(intervals[, 1], intervals, intervals[, 3])
      }
    } else {
      suggested_package("loo", min_version = "2.0.0")
      yrep <- validate_predictions(yrep, length(y))
      if (!is.null(subset)) {
        stopifnot(length(y) >= length(subset))
        y <- y[subset]
        yrep <- yrep[, subset, drop = FALSE]
        psis_object <- .psis_subset(psis_object, subset)
      }
      probs <- sort(c(prob, prob_outer))
      a <- (1 - probs) / 2
      stopifnot(identical(dim(psis_object), dim(yrep)))
      intervals <- suppressWarnings(t(loo::E_loo(
        x = yrep,
        psis_object = psis_object,
        type = "quantile",
        probs = sort(c(a, 0.5, 1 - a))
      )$value))
    }

    x <- seq_along(y)
    if (order_by_median) {
      x <- reorder(x, intervals[, 2])
    }
    xy_labs <- if (order_by_median) {
      labs(x = "Ordered by median", y = NULL)
    } else {
      intervals_axis_labels(has_x = FALSE)
    }

    data <- .loo_intervals_data(y, x, intervals)
    ggplot(data) +
      intervals_inner_aes(
        needs_y = TRUE,
        color = "yrep",
        fill = "yrep"
      ) +
      geom_linerange(
        mapping = intervals_outer_aes(color = "yrep"),
        alpha = alpha,
        size = size
      ) +
      geom_pointrange(
        shape = 21,
        stroke = 0.5,
        linewidth = linewidth,
        size = size,
        fatten = fatten
      ) +
      geom_point(
        mapping = aes(
          y = .data$y_obs,
          color = "y",
          fill = "y"
        ),
        shape = 21,
        stroke = 0.5,
        size = 1
      ) +
      scale_color_ppc() +
      scale_fill_ppc() +
      bayesplot_theme_get() +
      xy_labs +
      xaxis_text(!order_by_median) +
      xaxis_ticks(!order_by_median)
  }

#' @rdname PPC-loo
#' @export
ppc_loo_ribbon <-
  function(y,
           yrep,
           psis_object,
           ...,
           subset = NULL,
           intervals = NULL,
           prob = 0.5,
           prob_outer = 0.9,
           alpha = 0.33,
           size = 0.25) {
    check_ignored_arguments(..., ok_args = list("moment_match"))
    y <- validate_y(y)
    if (!is.null(intervals)) {
      stopifnot(is.matrix(intervals), ncol(intervals) %in% c(3, 5))
      inform(paste(
        "'intervals' specified so ignoring",
        "'yrep', 'psis_object', 'subset', if specified."
      ))
      if (ncol(intervals) == 3) {
        intervals <- cbind(intervals[, 1], intervals, intervals[, 3])
      }
    } else {
      suggested_package("loo", min_version = "2.0.0")
      yrep <- validate_predictions(yrep, length(y))
      if (!is.null(subset)) {
        stopifnot(length(y) >= length(subset))
        y <- y[subset]
        yrep <- yrep[, subset, drop = FALSE]
        psis_object <- .psis_subset(psis_object, subset)
      }
      probs <- sort(c(prob, prob_outer))
      a <- (1 - probs) / 2
      stopifnot(identical(dim(psis_object), dim(yrep)))
      intervals <- suppressWarnings(t(loo::E_loo(
        x = yrep,
        psis_object = psis_object,
        type = "quantile",
        probs = sort(c(a, 0.5, 1 - a))
      )$value))
    }

    data <- .loo_intervals_data(y, x = seq_along(y), intervals)
    ggplot(data) +
      intervals_inner_aes(fill = "yrep", color = "yrep") +
      geom_ribbon(
        mapping = intervals_outer_aes(fill = "yrep", color = "yrep"),
        alpha = alpha,
        linewidth = 0.05
      ) +
      geom_ribbon(
        mapping = intervals_outer_aes(),
        alpha = 1,
        linewidth = 0.05,
        fill = NA,
        color = get_color("m")
      ) +
      geom_ribbon(linewidth = 0.05) +
      geom_line(
        mapping = aes(y = .data$m),
        color = get_color("m"),
        linewidth = size
      ) +
      geom_blank(aes(fill = "y")) +
      geom_line(
        aes(y = .data$y_obs, color = "y"),
        linewidth = 0.5,
        alpha = 2 / 3
      ) +
      scale_color_ppc() +
      scale_fill_ppc(values = c(NA, get_color("l"))) +
      intervals_axis_labels(has_x = FALSE) +
      bayesplot_theme_get()
  }



# internal ----------------------------------------------------------------
.loo_intervals_data <- function(y, x, intervals) {
  stopifnot(length(y) == nrow(intervals), length(x) == length(y))

  tibble::tibble(
    y_id = seq_along(y),
    y_obs = y,
    x = x,
    ll = intervals[, 1],
    l = intervals[, 2],
    m = intervals[, 3],
    h = intervals[, 4],
    hh = intervals[, 5]
  )
}

# subset a psis_object without breaking it
# (FIXME: use function from loo package when subset.psis() method becomes available)
.psis_subset <- function(psis_object, subset) {
  stopifnot(all(subset == as.integer(subset)))
  if (length(subset) > dim(psis_object)[2]) {
    abort("'subset' has too many elements.")
  }
  psis_object$log_weights <- psis_object$log_weights[, subset, drop = FALSE]
  psis_object$diagnostics$pareto_k <- psis_object$diagnostics$pareto_k[subset]
  psis_object$diagnostics$n_eff <- psis_object$diagnostics$n_eff[subset]

  attr(psis_object, "dims") <- c(dim(psis_object)[1], length(subset))
  attr(psis_object, "norm_const_log") <- attr(psis_object, "norm_const_log")[subset]
  attr(psis_object, "tail_len") <- attr(psis_object, "tail_len")[subset]
  attr(psis_object, "r_eff") <- attr(psis_object, "r_eff")[subset]
  psis_object
}

## Boundary correction based on code by ArViz development team
# The main method is a 1-D density estimation for linear data with
# convolution with a Gaussian filter.

# Based on scipy.signal.gaussian formula
.gaussian <- function(N, bw) {
  n <- seq(0, N - 1) - (N - 1) / 2
  sigma <- 2 * bw * bw
  w <- exp(-n^2 / sigma)
  return(w)
}

.linear_convolution <- function(x,
                                bw,
                                grid_counts,
                                grid_breaks,
                                grid_len) {
  # 1-D Gaussian estimation via
  # convolution of a Gaussian filter and the binned relative freqs
  bin_width <- grid_breaks[2] - grid_breaks[1]
  f <- grid_counts / bin_width / length(x)
  bw <- bw / bin_width

  # number of data points to generate for gaussian filter
  gauss_n <- as.integer(bw * 2 * pi)
  if (gauss_n == 0) {
    gauss_n <- 1
  }

  # Generate Gaussian filter vector
  kernel <- .gaussian(gauss_n, bw)
  npad <- as.integer(grid_len / 5)

  # Reflection method (i.e. get first N and last N points to pad vector)
  f <- c(
    rev(f[1:(npad)]),
    f,
    rev(f)[(grid_len - npad):(grid_len - 1)]
  )

  # Convolution: Gaussian filter + reflection method (pading) works as an
  # averaging moving window based on a Gaussian density which takes care
  # of the density boundary values near 0 and 1.
  bc_pvals <- stats::filter(f,
    kernel,
    method = "convolution",
    sides = 2
  )[(npad + 1):(npad + grid_len)]

  bc_pvals / (bw * (2 * pi)^0.5)
}

.kde_correction <- function(x,
                            bw,
                            grid_len) {
  # Generate boundary corrected values via a linear convolution using a
  # 1-D Gaussian window filter. This method uses the "reflection method"
  # to estimate these pvalues and helps speed up the code
  if (any(is.infinite(x))) {
    warning(paste(
      "Ignored", sum(is.infinite(x)),
      "Non-finite PIT values are invalid for KDE boundary correction method"
    ))
    x <- x[is.finite(x)]
  }

  if (grid_len < 100) {
    grid_len <- 100
  }

  # Get relative frequency boundaries and counts for input vector
  bins <- seq(from = min(x), to = max(x), length.out = grid_len + 1)
  hist_obj <- graphics::hist(x, breaks = bins, plot = FALSE)
  grid_breaks <- hist_obj$breaks
  grid_counts <- hist_obj$counts

  # Compute bandwidth based on use specification
  bw <- stats::density(x, bw = bw)$bw

  # 1-D Convolution
  bc_pvals <- .linear_convolution(x, bw, grid_counts, grid_breaks, grid_len)

  # Generate vector of x-axis values for plotting based on binned relative freqs
  n_breaks <- length(grid_breaks)

  xs <- (grid_breaks[2:n_breaks] + grid_breaks[1:(n_breaks - 1)]) / 2

  first_nonNA <- utils::head(which(!is.na(bc_pvals)), 1)
  last_nonNA <- utils::tail(which(!is.na(bc_pvals)), 1)
  bc_pvals[1:first_nonNA] <- bc_pvals[first_nonNA]
  bc_pvals[last_nonNA:length(bc_pvals)] <- bc_pvals[last_nonNA]

  list(xs = xs, bc_pvals = bc_pvals)
}

# Wrapper function to generate runif reference lines based on
# .kde_correction()
.ref_kde_correction <- function(unifs, bw, grid_len) {
  # Allocate memory
  idx <- seq(
    from = 1,
    to = ncol(unifs) * nrow(unifs) + ncol(unifs),
    by = ncol(unifs)
  )
  idx <- c(idx, ncol(unifs) * nrow(unifs))
  xs <- rep(0, ncol(unifs) * nrow(unifs))
  bc_mat <- matrix(0, nrow(unifs), ncol(unifs))

  # Generate boundary corrected reference values
  for (i in 1:nrow(unifs)) {
    bc_list <- .kde_correction(unifs[i, ],
      bw = bw,
      grid_len = grid_len
    )
    bc_mat[i, ] <- bc_list$bc_pvals
    xs[idx[i]:(idx[i + 1] - 1)] <- bc_list$xs
  }

  list(xs = xs, unifs = bc_mat)
}

# Extract log weights from psis_object if provided
.get_lw <- function(lw = NULL, psis_object = NULL) {
  if (is.null(lw) && is.null(psis_object)) {
    abort("One of 'lw' and 'psis_object' must be specified.")
  } else if (is.null(lw)) {
    suggested_package("loo", min_version = "2.0.0")
    if (!loo::is.psis(psis_object)) {
      abort("If specified, 'psis_object' must be a PSIS object from the loo package.")
    }
    lw <- loo::weights.importance_sampling(psis_object)
  }
  lw
}
