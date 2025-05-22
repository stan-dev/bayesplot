# x' PPC calibration
#'
#' Assess the calibration of the predictive distributions `yrep` in relation to
#' the data `y'.
#' See the **Plot Descriptions** section, below, for details.
#'
#' @name PPC-calibration
#' @family PPCs
#'
#' @template args-y-rep
#' @template args-group
#'
#' @template return-ggplot-or-data
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{`ppc_calibration()`,`ppc_calibration_grouped()`}{
#'
#'   },
#'   \item{`ppc_calibration_overlay()`,`ppc_calibration_overlay_grouped()`}{
#'
#'   },
#'   \item{`ppc_loo_calibration()`,`ppc_loo_calibration_grouped()`}{
#'
#'   }
#' }
#'
#' @examples
#' color_scheme_set("brightblue")
#'
#' # Make an example dataset of binary observations
#' ymin <- range(example_y_data(), example_yrep_draws())[1]
#' ymax <- range(example_y_data(), example_yrep_draws())[2]
#' y <- rbinom(length(example_y_data()), 1, (example_y_data() - ymin) / (ymax - ymin))
#' prep <- (example_yrep_draws() - ymin) / (ymax - ymin)
#'
#' ppc_calibration_overlay(y, prep[1:50, ])
NULL


#' @rdname PPC-calibration
#' @export
ppc_calibration_overlay <- function(
    y, prep, ..., linewidth = 0.25, alpha = 0.5) {
  check_ignored_arguments(...)
  data <- .ppc_calibration_data(y, prep)
  ggplot(data) +
    geom_abline(color = "black", linetype = 2) +
    geom_line(
      aes(value, cep, group = rep_id, color = "yrep"),
      linewidth = linewidth, alpha = alpha
    ) +
    scale_color_ppc() +
    bayesplot_theme_get() +
    legend_none() +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    xlab("Predicted probability") +
    ylab("Conditional event probability") +
    NULL
}

#' @rdname PPC-calibration
#' @export
ppc_calibration_overlay_grouped <- function(
    y, prep, group, ..., linewidth = 0.25, alpha = 0.7) {
  check_ignored_arguments(...)
  data <- .ppc_calibration_data(y, prep, group)
  ggplot(data) +
    geom_abline(color = "black", linetype = 2) +
    geom_line(aes(value, cep, group = rep_id, color = "yrep"),
      linewidth = linewidth, alpha = alpha
    ) +
    facet_wrap(vars(group)) +
    scale_color_ppc() +
    bayesplot_theme_get() +
    legend_none() +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    xlab("Predicted probability") +
    ylab("Conditional event probability") +
    NULL
}

#' @rdname PPC-calibration
#' @export
ppc_calibration <- function(
    y, prep, prob = .95, show_mean = TRUE, ..., linewidth = 0.5, alpha = 0.7) {
  check_ignored_arguments(...)
  data <- .ppc_calibration_data(y, prep) %>%
    group_by(y_id) %>%
    summarise(
      value = median(value),
      lb = quantile(cep, .5 - .5 * prob),
      ub = quantile(cep, .5 + .5 * prob),
      cep = median(cep)
    )

  ggplot(data) +
    aes(value, cep) +
    geom_abline(color = "black", linetype = 2) +
    geom_ribbon(aes(ymin = lb, ymax = ub, fill = "yrep"), alpha = alpha) +
    geom_line(
      aes(color = "y"),
      linewidth = linewidth
    ) +
    scale_color_ppc() +
    scale_fill_ppc() +
    bayesplot_theme_get() +
    legend_none() +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    xlab("Predicted probability") +
    ylab("Conditional event probability") +
    NULL
}

#' @rdname PPC-calibration
#' @export
ppc_calibration_grouped <- function(
    y, prep, group, prob = .95, show_mean = TRUE, ..., linewidth = 0.5, alpha = 0.7) {
  check_ignored_arguments(...)
  data <- .ppc_calibration_data(y, prep, group) %>%
    group_by(group, y_id) %>%
    summarise(
      value = median(value),
      lb = quantile(cep, .5 - .5 * prob),
      ub = quantile(cep, .5 + .5 * prob),
      cep = median(cep)
    )

  ggplot(data) +
    aes(value, cep) +
    geom_abline(color = "black", linetype = 2) +
    geom_ribbon(aes(ymin = lb, ymax = ub, fill = "yrep"), alpha = alpha) +
    geom_line(
      aes(color = "y"),
      linewidth = linewidth
    ) +
    facet_wrap(vars(group)) +
    scale_color_ppc() +
    scale_fill_ppc() +
    bayesplot_theme_get() +
    legend_none() +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    xlab("Predicted probability") +
    ylab("Conditional event probability") +
    NULL
}

#' @rdname PPC-calibration
#' @export
ppc_loo_calibration <- function(
    y, prep, lw, ..., linewidth = 0.25, alpha = 0.7) {
  check_ignored_arguments(...)
  data <- .ppc_calibration_data(y, prep)
  ggplot(data) +
    geom_abline(color = "black", linetype = 2) +
    geom_line(
      aes(value, cep, group = rep_id, color = "yrep"),
      linewidth = linewidth, alpha = alpha
    ) +
    scale_color_ppc() +
    bayesplot_theme_get() +
    legend_none() +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    xlab("Predicted probability") +
    ylab("Conditional event probability") +
    NULL
}

#' @rdname PPC-calibration
#' @export
ppc_loo_calibration_grouped <- function(
    y, prep, group, lw, ..., linewidth = 0.25, alpha = 0.7) {
  check_ignored_arguments(...)
  data <- .ppc_calibration_data(y, prep, group)
  ggplot(data) +
    geom_abline(color = "black", linetype = 2) +
    geom_line(aes(value, cep, group = rep_id, color = "yrep"),
      linewidth = linewidth, alpha = alpha
    ) +
    facet_wrap(vars(group)) +
    scale_color_ppc() +
    bayesplot_theme_get() +
    legend_none() +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    xlab("Predicted probability") +
    ylab("Conditional event probability") +
    NULL
}

.ppc_calibration_data <- function(y, prep, group = NULL) {
  y <- validate_y(y)
  n_obs <- length(y)
  prep <- validate_predictions(prep, n_obs)
  if (any(prep > 1 | prep < 0)) {
    stop("Values of ´prep´ should be predictive probabilities between 0 and 1.")
  }
  if (!is.null(group)) {
    group <- validate_group(group, n_obs)
  } else {
    group <- rep(1, n_obs * nrow(prep))
  }

  if (requireNamespace("monotone", quietly = TRUE)) {
    monotone <- monotone::monotone
  } else {
    monotone <- function(y) {
      stats::isoreg(y)$yf
    }
  }
  .ppd_data(prep, group = group) %>%
    group_by(group, rep_id) %>%
    mutate(
      ord = order(value),
      value = value[ord],
      cep = monotone(y[ord])
    ) |>
    ungroup()
}

.loo_resample_data <- function(prep, lw, psis_object) {
  lw <- .get_lw(lw, psis_object)
  stopifnot(identical(dim(prep), dim(lw)))
  # Work in progress here...
}
