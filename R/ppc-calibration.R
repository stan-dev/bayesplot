#' PPC calibration
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
#'   \item{`ppc_calibration_overlay()`,`ppc_calibration_overlay_grouped()`}{
#'
#'   }
#' }
#'
NULL

#' @rdname PPC-calibration
#' @export
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

#' @rdname PPC-calibration
#' @export
ppc_calibration_overlay <- function(
    y, prep, ..., linewidth = 0.25, alpha = 0.7) {
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
