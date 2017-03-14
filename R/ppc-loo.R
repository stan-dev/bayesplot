#' LOO predictive checks
#'
#' @name PPC-loo
#' @family PPCs
#' @template args-y-yrep
#' @param ... Currently unused.
#' @param lw A matrix of log weights with the same dimensions as \code{yrep}.
#' @param size,alpha Passed to \code{\link[ggplot2]{geom_point}} to control the
#'   appearance of the points.
#'
#' @section Plot Descriptions:
#' \describe{
#' \item{\code{ppc_loo_pit}}{
#'  The calibration of marginal predictions can be checked with probability
#'  integral transformation (PIT) checks. LOO improves the check by avoiding the
#'  double use of data. See the section on marginal predictive checks in BDA3 p.
#'  152-153. The LOO probability integral transformation (PIT) predictive check
#'  is a quantile-quantile plot comparing the LOO PITs to the standard uniform
#'  distribution.
#' }
#' }
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
#' psis <- loo::psislw(-log_lik(fit))
#' ppc_loo_pit(y, yrep, lw = psis$lw_smooth)
#' }
#'
NULL

#' @rdname PPC-loo
#' @export
ppc_loo_pit <- function(y, yrep, lw, ..., size = 2, alpha = 0.5) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  stopifnot(identical(dim(yrep), dim(lw)))
  pit <- vapply(seq_len(ncol(yrep)), function(j) {
    sel <- yrep[, j] <= y[j]
    exp(log_sum_exp(lw[sel, j]))
  }, FUN.VALUE = 1)

  ggplot() +
    geom_point(
      aes_(sample = pit),
      stat = "qq",
      distribution = stats::qunif,
      color = get_color("m"),
      size = size,
      alpha = alpha
    ) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = 2,
      color = "black"
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(y = "LOO-PIT", x = "Uniform") +
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
log_sum_exp <- function(x) {
  m <- suppressWarnings(max(x))
  m + log(sum(exp(x - m)))
}


# #' @export
# loo_pareto_k <- function(k, ..., label_points = FALSE, size = 2.5, alpha = 0.8) {
#   k_inf <- !is.finite(k)
#   if (any(k_inf)) {
#     warning(signif(100 * mean(k_inf), 2),
#             "% of Pareto k estimates are Inf/NA/NaN and not plotted.")
#   }
#
#
#   klabs <- c("good", "ok")
#
#
#   plot_data <- data.frame(
#     k = as.vector(k),
#     idx = seq_along(k),
#     kcolor = cut(k, breaks = c(-Inf, 0.5, 0.7, 1, Inf),
#                  labels = c("good", "ok", "bad", "very bad"))
#   )
#   ggplot(plot_data, aes_(x = ~ idx, y = ~ k, color = ~ k, fill = ~ k)) +
#     .shaded_rect(ymin = -Inf, ymax = 0.5, fill = "gray30") +
#     .shaded_rect(ymin = 0.5, ymax = 0.7, fill = "gray50") +
#     .shaded_rect(ymin = 0.7, ymax = 1, fill = "gray70") +
#     .shaded_rect(ymin = 1, ymax = Inf, fill = "gray90") +
#     hline_at(c(0.5, 0.7, 1), color = "darkgray", size = 0.25) +
#     geom_point(
#       shape = 21,
#       size = size,
#       alpha = alpha
#     ) +
#     scale_y_continuous(breaks = c(0, 0.5, 0.7, 1)) +
#     scale_color_gradient2(low = get_color("lh"), high = get_color("dh")) +
#     scale_fill_gradient2(low = get_color("l"), high = get_color("d")) +
#     labs(
#       x = "Data point",
#       y = expression(paste("Shape parameter ", italic(k)))
#     ) +
#     theme_default()
# }

# .shaded_rect <- function(ymin, ymax, fill = "black", ...) {
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax,
#            fill = fill, ...)
# }

