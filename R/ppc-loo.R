#' LOO predictive checks
#'
#' Leave-One-Out (LOO) predictive checks. See the \strong{Plot Descriptions}
#' section below for details.
#'
#' @name PPC-loo
#' @family PPCs
#' @template args-y-yrep
#' @param ... Currently unused.
#' @param lw A matrix of (smoothed) log weights with the same dimensions as
#'   \code{yrep}. See the \code{\link[loo]{psislw}} function in the \pkg{loo}
#'   package, which returns smoothed weights that can be used to specify
#'   \code{lw}.
#' @param alpha,size,fatten Arguments passed to code geoms to control plot
#'   aesthetics. For \code{ppc_loo_pit}, \code{size} and \code{alpha} are passed
#'   to \code{\link[ggplot2]{geom_point}}. For \code{ppc_loo_intervals},
#'   \code{size} and \code{fatten} are passed to
#'   \code{\link[ggplot2]{geom_pointrange}}. For \code{ppc_loo_ribbon},
#'   \code{alpha} and \code{size} are passed to
#'   \code{\link[ggplot2]{geom_ribbon}}.
#'
#' @template return-ggplot
#'
#' @section Plot Descriptions:
#' \describe{
#' \item{\code{ppc_loo_pit}}{
#'  The calibration of marginal predictions can be assessed using probability
#'  integral transformation (PIT) checks. LOO improves the check by avoiding the
#'  double use of data. See the section on marginal predictive checks in Gelman
#'  et al. (2013, p. 152--153). The default LOO PIT predictive check is a
#'  quantile-quantile (Q-Q) plot comparing the LOO PITs to the standard uniform
#'  distribution. Alternatively, setting the \code{compare} argument to
#'  \code{"normal"} will produce a Q-Q plot comparing standardized PIT values
#'  to the standard normal distribution.
#' }
#' \item{\code{ppc_loo_intervals, ppc_loo_ribbon}}{
#'  Same \code{\link{ppc_intervals}} and \code{\link{ppc_ribbon}} but the
#'  intervals are for the LOO predictive distribution.
#' }
#' }
#'
#' @templateVar bdaRef (p. 152--153)
#' @template reference-bda
#' @template reference-loo
#'
#' @examples
#' color_scheme_set("orange")
#'
#' \dontrun{
#' library(rstanarm)
#' library(loo)
#'
#' head(radon)
#' fit <- stan_lmer(log_radon ~ floor + log_uranium + floor:log_uranium
#'                    + (1 + floor | county), data = radon)
#' y <- radon$log_radon
#' yrep <- posterior_predict(fit)
#' psis <- psislw(-log_lik(fit), cores = 2)
#' ppc_loo_pit(y, yrep, lw = psis$lw_smooth)
#' ppc_loo_pit(y, yrep, lw = psis$lw_smooth, compare = "normal")
#' }
#'
NULL

#' @rdname PPC-loo
#' @export
#' @param pit For \code{ppc_loo_pit}, optionally a vector of precomputed PIT
#'   values that can be specified instead of \code{y}, \code{yrep}, and
#'   \code{lw} (these are all ignored if \code{pit} is specified). If not
#'   specified the PIT values are computed internally before plotting.
#' @param compare For \code{ppc_loo_pit}, a string that can be either
#'   \code{"uniform"} or \code{"normal"}. If \code{"uniform"} (the default) the
#'   Q-Q plot compares computed PIT values to the standard uniform distribution.
#'   If \code{compare="normal"}, the Q-Q plot compares standardized PIT values
#'   to the standard normal distribution.
#'
ppc_loo_pit <-
  function(y,
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
      .ignore_y_yrep_lw(y, yrep, lw)
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

    graph <- ggplot(data.frame(p = pit)) +
      geom_point(
        aes_(sample = ~ p),
        stat = "qq",
        distribution = theoretical,
        color = get_color("m"),
        size = size,
        alpha = alpha
      ) +
      geom_abline(
        slope = 1,
        intercept = 0,
        linetype = 2,
        color = "black"
      )

    if (compare == "uniform") {
      xylim <- c(0, 1)
    } else {
      g <- ggplot_build(graph)
      xylim <- g$layout$panel_ranges[[1]]
      xylim <- range(xylim$y.range, xylim$x.range)
    }

    graph +
      coord_fixed(xlim = xylim, ylim = xylim) +
      labs(y = y_lab, x = x_lab) +
      theme_default()
  }




#' @rdname PPC-loo
#' @export
#' @param prob A value between 0 and 1 indicating the desired probability mass
#'   to include in the intervals. The default is 0.9.
#' @param intervals For \code{ppc_loo_intervals} and \code{ppc_loo_ribbon},
#'   optionally a matrix of precomputed LOO predictive intervals intervals with
#'   that can be specified instead of \code{yrep} and \code{lw} (these are both
#'   ignored if \code{intervals} is specified). If not specified the intervals
#'   are computed internally before plotting. If specified, \code{intervals}
#'   must be a matrix with number of rows equal to the number of data points and
#'   three columns in the following order: the first for the lower bound of the
#'   interval, the second for median (50\%), and the third for the interval
#'   upper bound (column names are ignored).
#'
ppc_loo_intervals <-
  function(y,
           yrep,
           lw,
           intervals,
           ...,
           prob = 0.9,
           size = 1,
           fatten = 3) {
    check_ignored_arguments(...)
    y <- validate_y(y)
    if (!missing(intervals)) {
      stopifnot(is.matrix(intervals), ncol(intervals) == 3)
      .ignore_y_yrep_lw(yrep = yrep, lw = lw) # dont ignore y
    } else {
      suggested_package("loo")
      yrep <- validate_yrep(yrep, y)
      stopifnot(identical(dim(yrep), dim(lw)))
      a <- (1 - prob) / 2
      intervals <- t(loo::E_loo(
        x = yrep,
        lw = lw,
        type = "quantile",
        probs = sort(c(a, 0.5, 1 - a))
      ))
    }
    .ppc_intervals(
      data = .loo_intervals_data(y, intervals),
      grouped = FALSE,
      style = "intervals",
      size = size,
      fatten = fatten,
      x_lab = "Data point (index)"
    )
  }

#' @rdname PPC-loo
#' @export
ppc_loo_ribbon <-
  function(y,
           yrep,
           lw,
           intervals,
           ...,
           prob = 0.9,
           alpha = 0.33,
           size = 0.25) {
    check_ignored_arguments(...)
    y <- validate_y(y)
    if (!missing(intervals)) {
      stopifnot(is.matrix(intervals), ncol(intervals) == 3)
      .ignore_y_yrep_lw(yrep = yrep, lw = lw) # dont ignore y
    } else {
      suggested_package("loo")
      yrep <- validate_yrep(yrep, y)
      stopifnot(identical(dim(yrep), dim(lw)))
      a <- (1 - prob) / 2
      intervals <- t(loo::E_loo(
        x = yrep,
        lw = lw,
        type = "quantile",
        probs = sort(c(a, 0.5, 1 - a))
      ))
    }
    .ppc_intervals(
      data = .loo_intervals_data(y, intervals),
      grouped = FALSE,
      style = "ribbon",
      size = size,
      alpha = alpha,
      x_lab = "Data point (index)"
    )
  }




# internal ----------------------------------------------------------------
.loo_intervals_data <- function(y, intervals) {
  colnames(intervals) <- c("lo", "mid", "hi")
  stopifnot(length(y) == nrow(intervals))
  x <- seq_along(y)
  dplyr::bind_rows(
    data.frame(x, is_y = TRUE, lo = y, mid = y, hi = y),
    data.frame(x, is_y = FALSE, intervals)
  )
}
.ignore_y_yrep_lw <- function(y, yrep, lw) {
  specified <- .which_specified(y, yrep, lw)
  if (length(specified))
    warning(
      "Ignoring ", paste(sQuote(specified), collapse = ","), " because ",
      sQuote("pit"), " was specified.",
      call. = FALSE
    )
}
.which_specified <- function(.y, .yrep, .lw) {
  w <- c(y = !missing(.y), yrep = !missing(.yrep), lw = !missing(.lw))
  names(w)[w]
}





# #' @rdname PPC-loo
# #' @export
# #' @param k A vector of estimates of the shape parameter \eqn{k} of the
# #'   generalized Pareto distribution.
# #' @param label_points Should observation numbers corresponding to any values of
# #'   \code{k} greater than 0.5 be displayed in the plot? Defaults to
# #'   \code{FALSE}.
# # loo_pareto_k <-
# #   function(y, k,
# #            ...,
# #            label_points = FALSE,
# #            size = 2.5,
#            alpha = 0.8) {
#
#     k_inf <- !is.finite(k)
#     if (any(k_inf)) {
#       warning(signif(100 * mean(k_inf), 2),
#               "% of Pareto k estimates are Inf/NA/NaN and not plotted.")
#     }
#
#     klabs <- c("good", "ok")
#
#     plot_data <- data.frame(
#       k = as.vector(k),
#       # idx = seq_along(k),
#       y = as.vector(y),
#       kcolor = cut(
#         k,
#         breaks = c(-Inf, 0.5, 0.7, 1, Inf),
#         labels = c("good", "ok", "bad", "very bad")
#       )
#     )
#     ggplot(plot_data, aes_(
#       x = ~ y,
#       y = ~ k,
#       color = ~ k,
#       fill = ~ k
#     )) +
#       .shaded_rect(ymin = -Inf,
#                    ymax = 0.5,
#                    fill = "gray30") +
#       .shaded_rect(ymin = 0.5,
#                    ymax = 0.7,
#                    fill = "gray50") +
#       .shaded_rect(ymin = 0.7,
#                    ymax = 1,
#                    fill = "gray70") +
#       .shaded_rect(ymin = 1,
#                    ymax = Inf,
#                    fill = "gray90") +
#       hline_at(c(0.5, 0.7, 1),
#                color = "darkgray",
#                size = 0.25) +
#       geom_point(shape = 21,
#                  size = size,
#                  alpha = alpha) +
#       scale_y_continuous(breaks = c(0, 0.5, 0.7, 1)) +
#       scale_color_gradient2(low = get_color("lh"), high = get_color("dh")) +
#       scale_fill_gradient2(low = get_color("l"), high = get_color("d")) +
#       labs(x = "Data point",
#            y = expression(paste("Shape parameter ", italic(k)))) +
#       theme_default()
#   }
#
# .shaded_rect <- function(ymin, ymax, fill = "black", ...) {
#   annotate(
#     "rect",
#     xmin = -Inf,
#     xmax = Inf,
#     ymin = ymin,
#     ymax = ymax,
#     fill = fill,
#     ...
#   )
# }
#
