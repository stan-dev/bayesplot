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
#'    `ppc_loo_calibration()`, and `ppc_loo_calibration_grouped()`, the type
#'    of interval to compute. Options are `"consistency"` (default) for credible
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
#'   leave-one-out (LOO) predictive probabilities, computed by resampling each
#'   observation's posterior predictive draws using the corresponding LOO
#'   importance weights.
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
    y, prep, ..., prob = NULL, linewidth = 0.25, alpha = 0.2,
    x_range = c("full", "data")) {
  check_ignored_arguments(...)
  x_range <- match.arg(x_range)
  data <- ppc_calibration_data(y, prep)
  params <- .calibration_plot_params(
    data = data,
    x_range = x_range,
    linewidth = linewidth,
    show_qdots = FALSE,
    prob = 0.95,
    interval = "consistency"
  )
  ggplot(data) +
    geom_abline(color = "darkgrey", linetype = 2, linewidth = 0.5) +
    geom_line(
      aes(.data$value, .data$cep, group = .data$rep_id, color = "yrep"),
      linewidth = linewidth, alpha = alpha
    ) +
    scale_x_continuous(breaks = params$x_breaks, labels = params$x_labels) +
    scale_y_continuous(breaks = params$y_breaks) +
    scale_color_ppc() +
    bayesplot_theme_get() +
    legend_none() +
    coord_equal(xlim = params$xlim, ylim = c(0, 1), expand = FALSE) +
    xlab("Predicted probability") +
    ylab("Conditional event probability") +
    NULL
}

#' @rdname PPC-calibration
#' @export
ppc_calibration_overlay_grouped <- function(
    y, prep, group, ..., prob = NULL, linewidth = 0.25, alpha = 0.2,
    x_range = c("full", "data")) {
  check_ignored_arguments(...)
  x_range <- match.arg(x_range)
  data <- ppc_calibration_data(y, prep, group)
  params <- .calibration_plot_params(
    data = data,
    x_range = x_range,
    linewidth = linewidth,
    show_qdots = FALSE,
    prob = 0.95,
    interval = "consistency"
  )
  ggplot(data) +
    geom_abline(color = "darkgrey", linetype = 2, linewidth = 0.5) +
    geom_line(aes(.data$value, .data$cep, group = .data$rep_id, color = "yrep"),
      linewidth = linewidth, alpha = alpha
    ) +
    facet_wrap(vars(group)) +
    scale_x_continuous(breaks = params$x_breaks, labels = params$x_labels) +
    scale_y_continuous(breaks = params$y_breaks) +
    scale_color_ppc() +
    bayesplot_theme_get() +
    legend_none() +
    coord_equal(xlim = params$xlim, ylim = c(0, 1), expand = FALSE) +
    xlab("Predicted probability") +
    ylab("Conditional event probability") +
    NULL
}

#' @rdname PPC-calibration
#' @param prep For `ppc_calibration()`, `ppc_calibration_grouped()`,
#'   `ppc_calibration_overlay()`, and `ppc_calibration_overlay_grouped()`,
#'   an `S` by `N` matrix of predicted probabilities in `[0, 1]`, where `S` is
#'   the number of draws and `N = length(y)`.
#' @param lw For `ppc_loo_calibration()` and `ppc_loo_calibration_grouped()`,
#'   a matrix of log weights with the same dimensions as `yrep`. Either
#'   `psis_object` or `lw` has to be specified.
#' @param psis_object For `ppc_loo_calibration()` and `ppc_loo_calibration_grouped()`,
#'   an object of class `"psis"` that is created when the `loo()` function calls
#'   `psis()` internally to do the PSIS procedure. Either `psis_object` or `lw`
#'   has to be specified.
#' @param prob Probability mass to include in the plotted interval.
#' @param interval For `ppc_calibration()`, `ppc_calibration_grouped()`,
#'   `ppc_loo_calibration()`, and `ppc_loo_calibration_grouped()`, choose
#'   `"confidence"` to use uncertainty in the estimated calibration curve, or
#'   `"consistency"` to compare to replicated outcomes.
#' @param interval_type Deprecated alias for `interval`.
#' @param x_range For `ppc_calibration()`, `ppc_calibration_grouped()`,
#'   `ppc_calibration_overlay()`, and `ppc_calibration_overlay_grouped()`,
#'   choose `"full"` to always show the x-axis on `[0, 1]` (default), or
#'   `"data"` to zoom to the range of predicted probabilities in the plotted
#'   data.
#' @param B For calibration plots that use `yrep` with `interval = "confidence"`,
#'   the number of bootstrap samples.
#' @param show_mean For calibration interval plots, if `TRUE` (default), draw
#'   the estimated calibration curve.
#' @param help_text For `ppc_calibration()` and `ppc_calibration_grouped()`,
#'   if `TRUE` (default) display a math-style label in the plot indicating the
#'   interval as `CI` with the selected `prob` and `interval` values.
#' @param show_qdots For `ppc_calibration()`, `ppc_calibration_grouped()`,
#'   `ppc_loo_calibration()`, and `ppc_loo_calibration_grouped()`, if `TRUE`
#'   add a quantile dot plot at the bottom of the panel to show the marginal
#'   distribution of predicted probabilities.
#' @param qdots_quantiles For `ppc_calibration()`, `ppc_calibration_grouped()`,
#'   `ppc_loo_calibration()`, and `ppc_loo_calibration_grouped()`, positive
#'   integer passed to
#'   the `quantiles` argument of `ggdist::stat_dots()` when
#'   `show_qdots = TRUE`.
#' @param ... Currently unused.
#' @param linewidth,alpha Arguments passed to geoms controlling line width and
#'   opacity.
#' @export
ppc_calibration <- function(
    y,
    prep = NULL,
    yrep = NULL,
    prob = .95,
    interval = c("confidence", "consistency"),
    interval_type = NULL,
    x_range = c("full", "data"),
    help_text = TRUE,
    B = 200,
    show_mean = TRUE,
    show_qdots = TRUE,
    qdots_quantiles = 100,
    ...,
    linewidth = 1,
    alpha = 0.1) {
  check_ignored_arguments(...)
  if (!is.null(interval_type)) {
    interval <- interval_type
  }
  interval <- match.arg(interval)
  x_range <- match.arg(x_range)
  .validate_calibration_qdots_args(show_qdots, qdots_quantiles)

  data <- ppc_calibration_interval_data(
    y = y,
    prep = prep,
    yrep = yrep,
    prob = prob,
    interval = interval,
    B = B
  )
  params <- .calibration_plot_params(
    data = data,
    x_range = x_range,
    linewidth = linewidth,
    show_qdots = show_qdots,
    prob = prob,
    interval = interval
  )

  p <- ggplot(data) +
    aes(.data$value, .data$cep) +
    geom_abline(color = "darkgrey", linetype = 2, linewidth = 0.5) +
    geom_ribbon(
      aes(ymin = .data$lb, ymax = .data$ub, fill = "yrep"),
      alpha = alpha
    ) +
    scale_x_continuous(breaks = params$x_breaks, labels = params$x_labels) +
    scale_y_continuous(breaks = params$y_breaks) +
    scale_color_ppc() +
    scale_fill_ppc() +
    bayesplot_theme_get() +
    legend_none() +
    coord_equal(xlim = params$xlim, ylim = params$ylim, expand = FALSE) +
    xlab("predicted probability") +
    ylab("conditional event probability") +
    NULL
  p <- .maybe_add_calibration_qdots(
    p = p,
    data = data,
    show_qdots = show_qdots,
    qdots_quantiles = qdots_quantiles
  )
  if (isTRUE(show_mean)) {
    p <- p + geom_step(aes(color = "y"), linewidth = linewidth)
  }
  if (isTRUE(help_text)) {
    p <- p + annotate(
    "text",
    x = params$xlim[1] + 0.05,
    y = 0.95,
    label = params$ci_label,
    hjust = 0, size = 0.8 * .theme_text_size() / ggplot2::.pt
  )
  }
  p
}

#' @rdname PPC-calibration
#' @export
ppc_calibration_grouped <- function(
    y,
    yrep = NULL,
    prep = NULL,
    group,
    prob = .95,
    interval = c("confidence", "consistency"),
    interval_type = NULL,
    x_range = c("full", "data"),
    help_text = TRUE,
    B = 200,
    show_mean = TRUE,
    show_qdots = TRUE,
    qdots_quantiles = 100,
    ...,
    linewidth = 1,
    alpha = 0.1) {
  check_ignored_arguments(...)
  if (!is.null(interval_type)) {
    interval <- interval_type
  }
  interval <- match.arg(interval)
  x_range <- match.arg(x_range)
  .validate_calibration_qdots_args(show_qdots, qdots_quantiles)
  data <- ppc_calibration_interval_data_grouped(
    y = y,
    prep = prep,
    yrep = yrep,
    group = group,
    prob = prob,
    interval = interval,
    B = B
  )
  params <- .calibration_plot_params(
    data = data,
    x_range = x_range,
    linewidth = linewidth,
    show_qdots = show_qdots,
    prob = prob,
    interval = interval
  )

  p <- ggplot(data) +
    aes(.data$value, .data$cep) +
    geom_abline(color = "darkgrey", linetype = 2, linewidth = 0.5) +
    geom_ribbon(
      aes(ymin = .data$lb, ymax = .data$ub, fill = "yrep"),
      alpha = alpha
    ) +
    scale_x_continuous(breaks = params$x_breaks, labels = params$x_labels) +
    scale_y_continuous(breaks = params$y_breaks) +
    facet_wrap(vars(group)) +
    scale_color_ppc() +
    scale_fill_ppc() +
    bayesplot_theme_get() +
    legend_none() +
    coord_equal(xlim = params$xlim, ylim = params$ylim, expand = FALSE) +
    xlab("predicted probability") +
    ylab("conditional event probability") +
    NULL
  p <- .maybe_add_calibration_qdots(
    p = p,
    data = data,
    show_qdots = show_qdots,
    qdots_quantiles = qdots_quantiles
  )
  if (isTRUE(show_mean)) {
    p <- p + geom_step(aes(color = "y"), linewidth = linewidth)
  }
  if (isTRUE(help_text)) {
    p <- p + annotate(
      "text",
      x = params$xlim[1] + 0.05,
      y = 0.95,
      label = params$ci_label,
      hjust = 0, size = 0.8 * .theme_text_size() / ggplot2::.pt
    )
  }
  p
}

#' @rdname PPC-calibration
#' @export
ppc_loo_calibration <- function(
    y,
    yrep,
    lw = NULL,
    psis_object = NULL,
    prob = .95,
    interval = c("confidence", "consistency"),
    interval_type = NULL,
    x_range = c("full", "data"),
    help_text = TRUE,
    B = 200,
    show_mean = TRUE,
    show_qdots = TRUE,
    qdots_quantiles = 100,
    ...,
    linewidth = 1,
    alpha = 0.1) {
  check_ignored_arguments(...)
  yrep_resampled <- .loo_resample_data(yrep, lw, psis_object)
  ppc_calibration(
    y = y,
    yrep = yrep_resampled,
    prep = NULL,
    prob = prob,
    interval = interval,
    interval_type = interval_type,
    x_range = x_range,
    help_text = help_text,
    B = B,
    show_mean = show_mean,
    show_qdots = show_qdots,
    qdots_quantiles = qdots_quantiles,
    ...,
    linewidth = linewidth,
    alpha = alpha
  )
}

#' @rdname PPC-calibration
#' @export
ppc_loo_calibration_grouped <- function(
    y,
    yrep,
    lw = NULL,
    psis_object = NULL,
    group,
    prob = .95,
    interval = c("confidence", "consistency"),
    interval_type = NULL,
    x_range = c("full", "data"),
    help_text = TRUE,
    B = 200,
    show_mean = TRUE,
    show_qdots = TRUE,
    qdots_quantiles = 100,
    ...,
    linewidth = 1,
    alpha = 0.1) {
  check_ignored_arguments(...)
  yrep_resampled <- .loo_resample_data(yrep, lw, psis_object)
  ppc_calibration_grouped(
    y = y,
    yrep = yrep_resampled,
    prep = NULL,
    group = group,
    prob = prob,
    interval = interval,
    interval_type = interval_type,
    x_range = x_range,
    help_text = help_text,
    B = B,
    show_mean = show_mean,
    show_qdots = show_qdots,
    qdots_quantiles = qdots_quantiles,
    ...,
    linewidth = linewidth,
    alpha = alpha
  )
}

#' @rdname PPC-calibration
#' @export
ppc_calibration_data <- function(y, prep, group = NULL) {
  y <- validate_y(y)
  n_obs <- length(y)
  prep <- validate_predictions(prep, n_obs)
  if (any(prep > 1 | prep < 0)) {
    stop("Values of 'prep' should be predictive probabilities between 0 and 1.")
  }

  if (!is.null(group)) {
    group <- validate_group(group, n_obs)
  } else {
    group <- rep(1, n_obs)
  }
  monotone <- .calibration_monotone_fn()
  d <- .ppd_data(prep, group = group) |>
    group_by(.data$group, .data$rep_id) |>
    mutate(
      ord = order(.data$value),
      value = .data$value[.data$ord],
      cep = monotone(y[.data$ord])
    ) |>
    ungroup()
}

.validate_calibration_qdots_args <- function(show_qdots, qdots_quantiles) {
  if (!is.logical(show_qdots) || length(show_qdots) != 1 || is.na(show_qdots)) {
    abort("'show_qdots' must be a single TRUE or FALSE.")
  }
  if (!is.numeric(qdots_quantiles) || length(qdots_quantiles) != 1 ||
      is.na(qdots_quantiles) || qdots_quantiles < 1 ||
      qdots_quantiles != as.integer(qdots_quantiles)) {
    abort("'qdots_quantiles' must be a positive integer.")
  }
}

.calibration_plot_params <- function(data, x_range, linewidth, show_qdots, prob, interval) {
  xlim <- if (identical(x_range, "data")) {
    xlim_data <- range(data$value, na.rm = TRUE)
    if (!all(is.finite(xlim_data)) || diff(xlim_data) <= 0) c(0, 1) else xlim_data
  } else {
    c(0, 1)
  }
  x_breaks <- if (identical(x_range, "data")) {
    seq(from = xlim[1], to = xlim[2], length.out = 5)
  } else {
    ggplot2::waiver()
  }
  x_labels <- if (identical(x_range, "data")) {
    scales::label_number(accuracy = 0.01)
  } else {
    ggplot2::waiver()
  }
  prob_pct <- sub("\\.?0+$", "", sprintf("%.2f", 100 * prob))
  list(
    xlim = xlim,
    x_breaks = x_breaks,
    x_labels = x_labels,
    ylim = c(0 - linewidth / 200, 1 + linewidth / 200),
    y_breaks = pretty(c(0, 1), n = 5),
    ci_label = sprintf("%s%%-%sInterval", prob_pct, capitalize_first(interval))
  )
}

.maybe_add_calibration_qdots <- function(p, data, show_qdots, qdots_quantiles) {
  if (!isTRUE(show_qdots)) {
    return(p)
  }
  suggested_package("ggdist")
  p + ggdist::stat_dots(
    aes(x = .data$value),
    data = data,
    quantiles = qdots_quantiles,
    height = .25,
    scale = 1,
    shape = 19,
    colour = color_scheme_get()$mid,
    inherit.aes = FALSE,
    alpha = 0.7
  )
}

#' @rdname PPC-calibration
#' @export
ppc_calibration_interval_data <- function(
    y,
    prep = NULL,
    yrep = NULL,
    prob = .95,
    interval = c("confidence", "consistency"),
    B = 200) {
  interval <- match.arg(interval)
  stopifnot(prob > 0, prob < 1)
  if (is.null(prep) == is.null(yrep)) {
    abort("Specify exactly one of 'prep' or 'yrep'.")
  }
  if (B < 1 || B != as.integer(B)) {
    abort("'B' must be a positive integer.")
  }

  y <- validate_y(y)
  if (any(y < 0 | y > 1)) {
    abort("'y' must contain values in [0, 1] for calibration.")
  }

  monotone <- .calibration_monotone_fn()
  alpha <- 1 - prob
  probs <- c(alpha / 2, 1 - alpha / 2)

  if (!is.null(prep)) {
    prep <- validate_predictions(prep, length(y))
    if (any(prep < 0 | prep > 1)) {
      abort("Values of 'prep' should be predictive probabilities in [0, 1].")
    }
  } else {
    yrep <- validate_predictions(yrep, length(y))
    if (any(yrep < 0 | yrep > 1)) {
      abort("Values of 'yrep' should be binary outcomes in [0, 1].")
    }
  }
  out <- .calibration_interval_data_core(
    y = y,
    prep = prep,
    yrep = yrep,
    interval = interval,
    probs = probs,
    B = B,
    monotone = monotone
  )

  tibble::as_tibble(out)
}

#' @rdname PPC-calibration
#' @export
ppc_calibration_interval_data_grouped <- function(
    y,
    prep = NULL,
    yrep = NULL,
    group,
    prob = .95,
    interval = c("confidence", "consistency"),
    B = 200) {
  y <- validate_y(y)
  n_obs <- length(y)
  if (is.null(prep) == is.null(yrep)) {
    abort("Specify exactly one of 'prep' or 'yrep'.")
  }
  interval <- match.arg(interval)
  stopifnot(prob > 0, prob < 1)
  if (B < 1 || B != as.integer(B)) {
    abort("'B' must be a positive integer.")
  }
  if (any(y < 0 | y > 1)) {
    abort("'y' must contain values in [0, 1] for calibration.")
  }
  monotone <- .calibration_monotone_fn()
  alpha <- 1 - prob
  probs <- c(alpha / 2, 1 - alpha / 2)
  if (!is.null(prep)) {
    prep <- validate_predictions(prep, n_obs)
    if (any(prep < 0 | prep > 1)) {
      abort("Values of 'prep' should be predictive probabilities in [0, 1].")
    }
  } else {
    yrep <- validate_predictions(yrep, n_obs)
    if (any(yrep < 0 | yrep > 1)) {
      abort("Values of 'yrep' should be binary outcomes in [0, 1].")
    }
  }
  group <- validate_group(group, n_obs)
  split_idx <- split(seq_len(n_obs), factor(group, levels = unique(group)))

  out <- lapply(split_idx, function(idx) {
    out_g <- .calibration_interval_data_core(
      y = y[idx],
      prep = if (is.null(prep)) NULL else prep[, idx, drop = FALSE],
      yrep = if (is.null(yrep)) NULL else yrep[, idx, drop = FALSE],
      interval = interval,
      probs = probs,
      B = B,
      monotone = monotone
    )
    out_g$group <- group[idx][1]
    out_g
  })

  tibble::as_tibble(dplyr::bind_rows(out))
}

.calibration_interval_data_core <- function(
    y, prep, yrep, interval, probs, B, monotone) {
  if (!is.null(prep)) {
    .calibration_interval_data_from_prep(
      y = y,
      prep = prep,
      interval = interval,
      probs = probs,
      monotone = monotone
    )
  } else {
    .calibration_interval_data_from_yrep(
      y = y,
      yrep = yrep,
      interval = interval,
      probs = probs,
      B = B,
      monotone = monotone
    )
  }
}

.calibration_col_quantiles <- function(x, probs) {
  t(vapply(
    seq_len(ncol(x)),
    function(j) stats::quantile(x[, j], probs = probs, names = FALSE),
    numeric(length(probs))
  ))
}

.calibration_interval_data_from_prep <- function(y, prep, interval, probs, monotone) {
  S <- nrow(prep)
  n <- ncol(prep)
  m_obs <- matrix(NA_real_, nrow = S, ncol = n)
  p_ord <- matrix(NA_real_, nrow = S, ncol = n)
  m_rep <- if (identical(interval, "consistency")) {
    matrix(NA_real_, nrow = S, ncol = n)
  } else {
    NULL
  }

  for (s in seq_len(S)) {
    ord <- order(prep[s, ])
    p_s <- prep[s, ord]
    y_s <- y[ord]
    m_obs[s, ] <- monotone(y_s)
    p_ord[s, ] <- p_s
    if (identical(interval, "consistency")) {
      y_tilde <- stats::rbinom(n, size = 1, prob = p_s)
      m_rep[s, ] <- monotone(y_tilde)
    }
  }

  band_source <- if (identical(interval, "confidence")) m_obs else m_rep
  ci <- .calibration_col_quantiles(band_source, probs)
  data.frame(
    y_id = seq_len(n),
    value = colMeans(p_ord),
    cep = colMeans(m_obs),
    lb = ci[, 1],
    ub = ci[, 2]
  )
}

.calibration_interval_data_from_yrep <- function(
    y, yrep, interval, probs, B, monotone) {
  n <- length(y)
  p <- colMeans(yrep)
  ord <- order(p)
  y_sorted <- y[ord]
  p_sorted <- p[ord]
  m_obs <- monotone(y_sorted)

  if (identical(interval, "confidence")) {
    m_boot <- matrix(NA_real_, nrow = B, ncol = n)
    for (b in seq_len(B)) {
      idx <- sample.int(n, size = n, replace = TRUE)
      idx_ord <- idx[order(p[idx])]
      m_boot[b, ] <- monotone(y[idx_ord])
    }
    ci <- .calibration_col_quantiles(m_boot, probs)
  } else {
    S <- nrow(yrep)
    m_rep <- matrix(NA_real_, nrow = S, ncol = n)
    for (s in seq_len(S)) {
      m_rep[s, ] <- monotone(yrep[s, ord])
    }
    ci <- .calibration_col_quantiles(m_rep, probs)
  }

  data.frame(
    y_id = seq_len(n),
    value = p_sorted,
    cep = m_obs,
    lb = ci[, 1],
    ub = ci[, 2]
  )
}

.calibration_monotone_fn <- function() {
  if (requireNamespace("monotone", quietly = TRUE)) {
    monotone::monotone
  } else {
    function(y) stats::isoreg(y)$yf
  }
}

.loo_resample_data <- function(yrep, lw, psis_object) {
  lw <- .get_lw(lw, psis_object)
  stopifnot(identical(dim(yrep), dim(lw)))

  yrep <- as.matrix(yrep)
  lw <- as.matrix(lw)

  # Resample each column (observation) with its corresponding weights.
  # Sampling indices directly is much faster than constructing draws objects.
  n_obs <- ncol(yrep)
  n_draws <- nrow(yrep)
  yrep_resampled <- matrix(NA_real_, nrow = n_draws, ncol = n_obs)

  for (i in seq_len(n_obs)) {
    probs_i <- .loo_resampling_probs(lw[, i])
    idx_i <- sample.int(n_draws, size = n_draws, replace = TRUE, prob = probs_i)
    yrep_resampled[, i] <- yrep[idx_i, i]
  }

  # Add observation names if available
  if (!is.null(colnames(yrep))) {
    colnames(yrep_resampled) <- colnames(yrep)
  }

  yrep_resampled
}

.loo_resampling_probs <- function(w) {
  if (!all(is.finite(w))) {
    abort("All values in 'lw' must be finite.")
  }
  p <- if (any(w < 0)) {
    # Treat negative entries as log-weights and stabilize before exponentiating.
    exp(w - max(w))
  } else {
    w
  }
  total <- sum(p)
  if (!is.finite(total) || total <= 0) {
    rep(1 / length(w), length(w))
  } else {
    p / total
  }
}
