#' LOO predictive checks
#'
#' @name PPC-loo
#' @family PPCs
#' @template args-y-yrep
#' @param ... Currently unused.
#' @param lw A matrix of (smoothed) log weights with the same dimensions as
#'   \code{yrep}.
#' @param size,alpha Passed to \code{\link[ggplot2]{geom_point}} to control the
#'   appearance of the points.
#'
#' @section Plot Descriptions:
#' \describe{
#' \item{\code{ppc_loo_pit}}{
#'  The calibration of marginal predictions can be checked with probability
#'  integral transformation (PIT) checks. LOO improves the check by avoiding the
#'  double use of data. See the section on marginal predictive checks in Gelman
#'  et al. (2013, p. 152--153). The default LOO probability integral
#'  transformation (PIT) predictive check is a quantile-quantile (Q-Q) plot
#'  comparing the LOO PITs to the standard uniform distribution. Alternatively,
#'  setting the \code{compare} argument to \code{"normal"} will produce a Q-Q
#'  plot comparing standardized PIT values may to the standard normal
#'  distribution.
#' }
#' }
#'
#' @templateVar bdaRef (p. 152--153)
#' @template reference-bda
#' @template reference-loo
#'
#' @examples
#' color_scheme_set("red")
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
#' psis <- psislw(-log_lik(fit))
#' ppc_loo_pit(y, yrep, lw = psis$lw_smooth)
#' ppc_loo_pit(y, yrep, lw = psis$lw_smooth, compare = "normal")
#' }
#'
NULL

#' @rdname PPC-loo
#' @export
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
           compare = c("uniform", "normal"),
           ...,
           size = 2,
           alpha = 0.5) {
    y <- validate_y(y)
    yrep <- validate_yrep(yrep, y)
    stopifnot(identical(dim(yrep), dim(lw)))
    compare <- match.arg(compare)
    pits <- pit(y, yrep, lw)

    if (compare == "uniform") {
      theoretical <- stats::qunif
      x_lab <- "Uniform"
      y_lab <- "LOO-PIT"
    } else {
      pits <- as.vector(scale(pits))
      theoretical <- stats::qnorm
      x_lab <- "Normal"
      y_lab <- "LOO-PIT (standardized)"
    }
    pits <- data.frame(pit = pits)

    graph <- ggplot(pits) +
      geom_point(
        aes_(sample = ~ pit),
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


# #' @rdname PPC-loo
# #' @export
# ppc_loo_intervals <- function(y, yrep, lw, ...) {
#   y <- validate_y(y)
#   yrep <- validate_yrep(yrep, y)
#   stopifnot(identical(dim(yrep), dim(lw)))
#
# }

# #' @rdname PPC-loo
# #' @export
# ppc_loo_ribbon <- function(y, yrep, lw, ...) {
#   y <- validate_y(y)
#   yrep <- validate_yrep(yrep, y)
#   stopifnot(identical(dim(yrep), dim(lw)))
#
# }

# internal ----------------------------------------------------------------
pit <- function(y, yrep, lw) {
  vapply(seq_len(ncol(yrep)), function(j) {
    sel <- yrep[, j] <= y[j]
    exp_log_sum_exp(lw[sel, j])
  }, FUN.VALUE = 1)
}
exp_log_sum_exp <- function(x) {
  m <- max(x, na.rm = TRUE)
  exp(m + log(sum(exp(x - m))))
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
