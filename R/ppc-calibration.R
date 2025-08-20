#' PPC calibration
#'
#' Assess the calibration of the predictions, or predictive probabilites in relation to
#' binary observations.
#' See the **Plot Descriptions** section, below, for details.
#'
#' @name PPC-calibration
#' @family PPCs
#'
#' @template args-y-yrep
#' @template args-group
#' @param interval_type For `ppc_calibration()`, `ppc_calibration_grouped()`,
#'   'ppc_loo_calibration()', and ´ppc_loo_calibration_grouped()´, the type of
#'    interval to compute. Options are '"consistency"' (default) for credible
#'    intervals for the PAV-adjusted calibration curve of posterior predictive
#'    sample, or `"confidence"` for the credible intervals of the calibration
#'    curve of the observed binary events.
#'
#' @template return-ggplot-or-data
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{`ppc_calibration()`,`ppc_calibration_grouped()`}{
#'   PAV-adjusted calibration plots showing the relationship between the
#'   predicted event probabilities and the conditional event probabilities.
#'   The `interval_type` parameter controls whether confidence intervals, or
#'   consistency intervals are computed.},
#'   \item{`ppc_calibration_overlay()`,`ppc_calibration_overlay_grouped()`}{
#'   Overlay plots showing posterior samples of PAV-adjusted calibration
#'   curves.
#'   },
#'   \item{`ppc_loo_calibration()`,`ppc_loo_calibration_grouped()`}{
#'   PAV-adjusted calibration plots to assess the calibration of the
#'   leave-one-out (LOO) predictive probabilities.
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
#'
#' # Compare confidence vs consistency intervals
#' ppc_calibration(y, prep, interval_type = "confidence")
#' ppc_calibration(y, prep, interval_type = "consistency")
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
  data <- .ppc_calibration_data(y, prep = prep, group = group)
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
    y, prep = NULL, yrep = NULL, prob = .95, interval_type = c("confidence", "consistency"), ...,
    linewidth = 0.5, alpha = 0.7) {
  check_ignored_arguments(...)
  interval_type <- match.arg(interval_type)
  data <- .ppc_calibration_data(y, prep, yrep, NULL, interval_type) %>%
    group_by(idx) %>%
    summarise(
      value = mean(value),
      lb = quantile(cep_intervals, .5 - .5 * prob),
      ub = quantile(cep_intervals, .5 + .5 * prob),
      cep = mean(cep)
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
    y,
    prep = NULL,
    yrep = NULL,
    group,
    prob = .95,
    interval_type = c("confidence", "consistency"),
    ...,
    linewidth = 0.5,
    alpha = 0.7) {
  check_ignored_arguments(...)
  interval_type <- match.arg(interval_type)
  data <- .ppc_calibration_data(y, prep, yrep, group, interval_type) %>%
    group_by(group, idx) %>%
    summarise(
      value = mean(value),
      lb = quantile(cep_intervals, .5 - .5 * prob),
      ub = quantile(cep_intervals, .5 + .5 * prob),
      cep = mean(cep),
    ) %>%
    ungroup()

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
    y, yrep, lw, prob = .95, interval_type = c("confidence", "consistency"), ..., linewidth = 0.5, alpha = 0.7) {
  check_ignored_arguments(...)
  # Create LOO-predictive samples using resampling
  yrep_resampled <- .loo_resample_data(yrep, lw, psis_object = NULL)
  ppc_calibration(y, yrep = yrep_resampled, prob = prob, interval_type = interval_type, ..., linewidth = linewidth, alpha = alpha)
}

#' @rdname PPC-calibration
#' @export
ppc_loo_calibration_grouped <- function(
    y,
    yrep,
    group,
    lw,
    prob = .95,
    interval_type = c("confidence", "consistency"),
    ...,
    linewidth = 0.5,
    alpha = 0.7) {
  check_ignored_arguments(...)
  # Create LOO-predictive samples using resampling
  yrep_resampled <- .loo_resample_data(yrep, lw, psis_object = NULL)
  ppc_calibration_grouped(
    y,
    yrep = yrep_resampled,
    group = group,
    prob = prob,
    interval_type = interval_type,
    ...,
    linewidth = linewidth,
    alpha = alpha
  )
}

.ppc_calibration_data <- function(
    y,
    prep = NULL,
    yrep = NULL,
    group = NULL,
    interval_type = c("confidence", "consistency")) {
  y <- validate_y(y)
  n_obs <- length(y)
  interval_type <- match.arg(interval_type)

  # Determine if we're using prep (probabilities) or yrep (predictive samples)
  if (!is.null(prep)) {
    predictions <- validate_predictions(prep, n_obs)
    if (any(prep > 1 | prep < 0)) {
      stop(
        "Values of ´prep´ should be predictive probabilities between 0 and 1."
      )
    }
    is_probability <- TRUE
  } else if (!is.null(yrep)) {
    predictions <- validate_predictions(yrep, n_obs)
    is_probability <- FALSE
  } else {
    stop("Either 'prep' or 'yrep' must be provided.")
  }

  if (!is.null(group)) {
    group <- validate_group(group, n_obs)
  } else {
    group <- rep(1, n_obs)
  }

  data <- .ppd_data(predictions, group = group)

  if (interval_type == "confidence") {
    if (is_probability) {
      # confidence interval from predicted probabilities:
      # cep = cep_intervals = monotone(y[order(ppred)])
      # i.e. posterior of the calibration curve trajectory
      # data %>%
      #   group_by(group, rep_id) %>%
      #   mutate(
      #     ord = order(value),
      #     value = value[ord],
      #     cep_intervals = .monotone(y[ord]),
      #     cep = cep_intervals,
      #     idx = seq_len(n())
      #   ) %>%
       data %>%
        group_by(group) %>%
        group_modify(.f = pava_transform(.x, .y, y, NULL)) %>%
        ungroup() %>%
        select(value, cep_intervals, cep, idx, group)
    } else {
      ppred <- colMeans(predictions)
      data %>%
        group_by(group, rep_id) %>%
        mutate(
          idx_boot = sample(y_id, n(), replace = TRUE),
          ord = order(ppred[idx_boot]),
          value = ppred[idx_boot][ord],
          cep_intervals = .monotone(y[idx_boot][ord]),
          cep = cep_intervals,
          idx = seq_len(n()),
        ) %>%
        ungroup() %>%
        select(value, cep_intervals, cep, idx, group)
    }
  } else {
    # Consistency intervals
    if (is_probability) {
      # For prep (probabilities), generate predictive samples from binomial
      data %>%
        group_by(group, rep_id) %>%
        mutate(
          # Generate predictive samples from binomial distribution
          ord = order(value),
          value = value[ord],
          cep_intervals = .monotone(rbinom(n(), 1, value)),
          cep = .monotone(y[ord]),
          idx = seq_len(n())
        ) %>%
        ungroup() %>%
        select(value, cep, cep_intervals, idx, group)
    } else {
      # For yrep (predictive samples), use column averages for ordering
      ppred <- colMeans(predictions)
      data %>%
        group_by(group, rep_id) %>%
        mutate(
          # Use column averages for ordering when yrep is provided
          ord = order(ppred[y_id]),
          cep_intervals = .monotone(value[ord]),
          value = ppred[y_id][ord],
          cep = .monotone(y[y_id][ord]),
          idx = seq_len(n()),
        ) %>%
        ungroup() %>%
        select(value, cep_intervals, cep, idx, group)
    }
  }
}

.loo_resample_data <- function(yrep, lw, psis_object) {
  lw <- .get_lw(lw, psis_object)
  stopifnot(identical(dim(yrep), dim(lw)))

  # Resample each column (observation) with its corresponding weights
  n_obs <- ncol(yrep)
  n_draws <- nrow(yrep)

  # Initialize resampled matrix
  yrep_resampled <- matrix(NA, nrow = n_draws, ncol = n_obs)

  for (i in 1:n_obs) {
    # Create draws object for this observation
    draws_i <- posterior::as_draws_matrix(yrep[, i, drop = FALSE])

    # Resample using the weights for this observation
    weights_i <- lw[, i]
    resampled_i <- posterior::resample_draws(
      draws_i,
      weights = weights_i, ndraws = n_draws
    )

    # Extract the resampled values
    yrep_resampled[, i] <- as.numeric(resampled_i)
  }

  # Add observation names if available
  if (!is.null(colnames(yrep))) {
    colnames(yrep_resampled) <- colnames(yrep)
  }

  yrep_resampled
}

.monotone <- function(y) {
  if (requireNamespace("monotone", quietly = TRUE)) {
    monotone <- monotone::monotone
  } else {
    monotone <- function(y) {
      stats::isoreg(y)$yf
    }
  }
  monotone(y)
}

pava_transform <- function(.x, .y, y, yrep, interval_type) {
  if (no_prob) {
    probs <- .x %>%
      group_by(y_id) %>%
      summarise(p = mean(yrep)) %>%
      arrange(y_id) %>%
      pull(p)
    ord <- order(probs)
    data <- .x |>
      group_by(rep_id) |>
      mutate(ord = ord) |>
      ungroup()
  } else {
    data <- data |>
      group_by(rep_id) |>
      mutate(ord = order(value)) |>
      ungroup()
  }
  if (interval_type == "confidence") {
    data %>%
      group_by(rep_id) %>%
      mutate(
        cep_intervals = .monotone(y[y_id][ord_v]),
        value = ifelse(is.null(yrep), value[ord_v], probs[ord],
        cep = cep_interval,
        idx = seq_len(n()),
      ) %>%
      ungroup() %>%
      select(value, cep_intervals, cep, idx, rep_id, y_id)
  } else {
    data 
  }
}
