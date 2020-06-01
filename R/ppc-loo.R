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
#'   the **Examples** section, below.
#' @param alpha,size,fatten Arguments passed to code geoms to control plot
#'   aesthetics. For `ppc_loo_pit_qq()` and `ppc_loo_pit_overlay()`, `size` and
#'   `alpha` are passed to [ggplot2::geom_point()] and
#'   [ggplot2::geom_density()], respectively. For `ppc_loo_intervals()`, `size`
#'   and `fatten` are passed to [ggplot2::geom_pointrange()]. For
#'   `ppc_loo_ribbon()`, `alpha` and `size` are passed to
#'   [ggplot2::geom_ribbon()].
#'
#' @template return-ggplot
#'
#' @section Plot Descriptions:
#' \describe{
#' \item{`ppc_loo_pit_overlay()`, `ppc_loo_pit_qq()`}{
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
#'  produce a Q-Q plot comparing standardized PIT values to the standard normal
#'  distribution that can help see the (mis)calibration better for the extreme
#'  values. However, in most cases we have found that the overlaid density plot
#'  (`ppc_loo_pit_overlay()`) function will provided a clearer picture of
#'  calibration problems that the Q-Q plot.
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
#'
#' \dontrun{
#' suppressPackageStartupMessages(library(rstanarm))
#' suppressPackageStartupMessages(library(loo))
#'
#' head(radon)
#' fit <- stan_lmer(
#'   log_radon ~ floor + log_uranium + floor:log_uranium
#'                + (1 + floor | county),
#'   data = radon,
#'   iter = 1000,
#'   chains = 2,  # cores = 2
#'   refresh = 500
#'  )
#' y <- radon$log_radon
#' yrep <- posterior_predict(fit)
#'
#' loo1 <- loo(fit, save_psis = TRUE, cores = 2)
#' psis1 <- loo1$psis_object
#' lw <- weights(psis1)
#'
#' # marginal predictive check using LOO probability integral transform
#' color_scheme_set("orange")
#' ppc_loo_pit_overlay(y, yrep, lw = lw)
#'
#' ppc_loo_pit_qq(y, yrep, lw = lw)
#' ppc_loo_pit_qq(y, yrep, lw = lw, compare = "normal")
#'
#'
#' # loo predictive intervals vs observations
#' keep_obs <- 1:50
#' ppc_loo_intervals(y, yrep, psis_object = psis1, subset = keep_obs)
#'
#' color_scheme_set("gray")
#' ppc_loo_intervals(y, yrep, psis_object = psis1, subset = keep_obs,
#'                   order = "median")
#' }
#'
NULL

#' @rdname PPC-loo
#' @export
#' @param pit For `ppc_loo_pit_overlay()` and `ppc_loo_pit_qq()`, optionally a
#'   vector of precomputed PIT values that can be specified instead of `y`,
#'   `yrep`, and `lw` (these are all ignored if `pit` is specified). If not
#'   specified the PIT values are computed internally before plotting.
#' @param samples For `ppc_loo_pit_overlay()`, the number of data sets (each
#'   the same size as `y`) to simulate from the standard uniform
#'   distribution. The default is 100. The density estimate of each dataset is
#'   plotted as a thin line in the plot, with the density estimate of the LOO
#'   PITs overlaid as a thicker dark line.
#' @param compare For `ppc_loo_pit_qq()`, a string that can be either
#'   `"uniform"` or `"normal"`. If `"uniform"` (the default) the Q-Q plot
#'   compares computed PIT values to the standard uniform distribution. If
#'   `compare="normal"`, the Q-Q plot compares standardized PIT values to the
#'   standard normal distribution.
#' @param trim Passed to [ggplot2::stat_density()].
#' @template args-density-controls
ppc_loo_pit_overlay <- function(y,
                                yrep,
                                lw,
                                pit,
                                samples = 100,
                                ...,
                                size = 0.25,
                                alpha = 0.7,
                                trim = FALSE,
                                bw = "nrd0",
                                adjust = 1,
                                kernel = "gaussian",
                                n_dens = 1024) {
  check_ignored_arguments(...)

  if (!missing(pit)) {
    stopifnot(is.numeric(pit), is_vector_or_1Darray(pit))
    inform("'pit' specified so ignoring 'y','yrep','lw' if specified.")
  } else {
    suggested_package("rstantools")
    y <- validate_y(y)
    yrep <- validate_yrep(yrep, y)
    stopifnot(identical(dim(yrep), dim(lw)))
    pit <- rstantools::loo_pit(object = yrep, y = y, lw = lw)
  }

  unifs <- matrix(runif(length(pit) * samples), nrow = samples)

  data <- ppc_data(pit, unifs)

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
      n = n_dens,
      na.rm = TRUE) +
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
      n = n_dens,
      na.rm = TRUE) +
    scale_color_ppc_dist(labels = c("PIT", "Unif")) +
    scale_x_continuous(
      limits = c(.1, .9),
      expand = expansion(0, 0),
      breaks = seq(from = .1, to = .9, by = .2)) +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, .25))) +
    bayesplot_theme_get() +
    yaxis_title(FALSE) +
    xaxis_title(FALSE) +
    yaxis_text(FALSE) +
    yaxis_ticks(FALSE)
}


#' @rdname PPC-loo
#' @export
ppc_loo_pit_qq <- function(y,
                           yrep,
                           lw,
                           pit,
                           compare = c("uniform", "normal"),
                           ...,
                           size = 2,
                           alpha = 1) {
  check_ignored_arguments(...)

  compare <- match.arg(compare)
  if (!missing(pit)) {
    stopifnot(is.numeric(pit), is_vector_or_1Darray(pit))
    inform("'pit' specified so ignoring 'y','yrep','lw' if specified.")
  } else {
    suggested_package("rstantools")
    y <- validate_y(y)
    yrep <- validate_yrep(yrep, y)
    stopifnot(identical(dim(yrep), dim(lw)))
    pit <- rstantools::loo_pit(object = yrep, y = y, lw = lw)
  }

  if (compare == "uniform") {
    theoretical <- stats::qunif
    x_lab <- "Uniform"
    y_lab <- "LOO-PIT"
  } else {
    pit <- as.vector(scale(pit))
    theoretical <- stats::qnorm
    x_lab <- "Normal"
    y_lab <- "LOO-PIT (standardized)"
  }

  ggplot(data.frame(p = pit)) +
    geom_qq(
      aes_(sample = ~ p),
      distribution = theoretical,
      color = get_color("m"),
      size = size,
      alpha = alpha) +
    geom_qq_line(
      aes_(sample = ~ p),
      linetype = 2,
      distribution = theoretical,
      color = "black",
      fullrange = FALSE) +
    bayesplot_theme_get() +
    labs(x = x_lab, y = y_lab)
}


#' @rdname PPC-loo
#' @export
ppc_loo_pit <-
  function(y,
           yrep,
           lw,
           pit,
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
#' @template args-prob-prob_outer
#' @param psis_object If using **loo** version `2.0.0` or greater, an
#'   object returned by the `psis()` function (or by the `loo()` function
#'   with argument `save_psis` set to `TRUE`).
#' @param intervals For `ppc_loo_intervals()` and `ppc_loo_ribbon()`,
#'   optionally a matrix of precomputed LOO predictive intervals
#'   that can be specified instead of `yrep` and `lw` (these are both
#'   ignored if `intervals` is specified). If not specified the intervals
#'   are computed internally before plotting. If specified, `intervals`
#'   must be a matrix with number of rows equal to the number of data points and
#'   five columns in the following order: lower outer interval, lower inner
#'   interval, median (50%), upper inner interval and upper outer interval
#'   (column names are ignored).
#' @param order For `ppc_loo_intervals()`, a string indicating how to arrange
#'   the plotted intervals. The default (`"index"`) is to plot them in the
#'   order of the observations. The alternative (`"median"`) arranges them
#'   by median value from smallest (left) to largest (right).
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
           subset = NULL,
           intervals = NULL,
           ...,
           prob = 0.5,
           prob_outer = 0.9,
           size = 1,
           fatten = 3,
           order = c("index", "median")) {

    check_ignored_arguments(...)
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
      yrep <- validate_yrep(yrep, y)
      if (!is.null(subset)) {
        stopifnot(length(y) >= length(subset))
        y <- y[subset]
        yrep <- yrep[, subset, drop=FALSE]
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

    graph <- .ppc_intervals(
      data = .loo_intervals_data(y, x, intervals),
      grouped = FALSE,
      style = "intervals",
      size = size,
      fatten = fatten,
      x_lab = "Data point (index)"
    )

    if (!order_by_median) {
      return(graph)
    }

    graph +
      xlab("Ordered by median") +
      xaxis_text(FALSE) +
      xaxis_ticks(FALSE)
  }

#' @rdname PPC-loo
#' @export
ppc_loo_ribbon <-
  function(y,
           yrep,
           lw,
           psis_object,
           subset = NULL,
           intervals = NULL,
           ...,
           prob = 0.5,
           prob_outer = 0.9,
           alpha = 0.33,
           size = 0.25) {
    check_ignored_arguments(...)
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
      yrep <- validate_yrep(yrep, y)
      if (!is.null(subset)) {
        stopifnot(length(y) >= length(subset))
        y <- y[subset]
        yrep <- yrep[, subset, drop=FALSE]
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
    .ppc_intervals(
      data = .loo_intervals_data(y, x = seq_along(y), intervals),
      grouped = FALSE,
      style = "ribbon",
      size = size,
      alpha = alpha,
      x_lab = "Data point (index)"
    )
  }



# internal ----------------------------------------------------------------
.loo_intervals_data <- function(y, x, intervals) {
  stopifnot(length(y) == nrow(intervals), length(x) == length(y))

  data.frame(
    y_id = seq_along(y),
    y_obs = y,
    x = x,
    ll = intervals[, 1],
    l  = intervals[, 2],
    m  = intervals[, 3],
    h  = intervals[, 4],
    hh = intervals[, 5])
}

# subset a psis_object without breaking it
.psis_subset <- function(psis_object, subset) {
  stopifnot(all(subset == as.integer(subset)))
  if (length(subset) > dim(psis_object)[2]) {
    abort("'subset' has too many elements.")
  }
  psis_object$log_weights <- psis_object$log_weights[, subset, drop=FALSE]
  psis_object$diagnostics$pareto_k <- psis_object$diagnostics$pareto_k[subset]
  psis_object$diagnostics$n_eff <- psis_object$diagnostics$n_eff[subset]

  attr(psis_object, "dims") <- c(dim(psis_object)[1], length(subset))
  attr(psis_object, "norm_const_log") <- attr(psis_object, "norm_const_log")[subset]
  attr(psis_object, "tail_len") <- attr(psis_object, "tail_len")[subset]
  attr(psis_object, "r_eff") <- attr(psis_object, "r_eff")[subset]
  return(psis_object)
}

