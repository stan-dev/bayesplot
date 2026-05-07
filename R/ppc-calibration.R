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
#' @param interval For `ppc_calibration()`, `ppc_calibration_grouped()`,
#'    `ppc_loo_calibration()`, and `ppc_loo_calibration_grouped()`, the type
#'    of interval to compute. Options are `"consistency"` (default) for credible
#'    intervals for the PAV-adjusted calibration curve of posterior predictive
#'    sample, or `"confidence"` for the credible intervals of the calibration
#'    curve of the observed binary events.
#'
#' @template return-ggplot-or-data
#'
#' @details
#' The ppc_calibration functions are designed to assess the calibration of a 
#' model with binary outcomes. In this context, calibration refers to the 
#' agreement between predicted probabilities and conditional event probabilities
#' (CEPs) see Dimitriadis et al. (2021) and Säilynoja et al. (2025) for details.
#' 
#' The required inputs are `y`, representing binary observations
#' (0 or 1), and either `yrep` or `prep`. Specifically,
#' `ppc_calibration_overlay()` and `ppc_calibration_overlay_grouped()` require
#' `prep`, while `ppc_calibration()`, `ppc_calibration_grouped()`,
#' `ppc_loo_calibration()`, and `ppc_loo_calibration_grouped()` accept either
#' `prep` or `yrep`.  
#' 
#' A document with detailed explanations and examples is available in the 
#' [vignettes](https://mc-stan.org/bayesplot/vignettes/articles-online-only/).
#' 
#' @section Plot Descriptions:
#' \describe{
#'   \item{`ppc_calibration()`,`ppc_calibration_grouped()`}{
#'   PAV-adjusted calibration plots showing the relationship between the
#'   predicted event probabilities and the conditional event probabilities.
#'   The `interval` parameter controls whether confidence intervals, or
#'   consistency intervals are computed around the calibration curve.}
#'   \item{`ppc_calibration_overlay()`,`ppc_calibration_overlay_grouped()`}{
#'   Overlay plots showing posterior samples of PAV-adjusted calibration
#'   curves for each posterior draw, which can be used to visually assess the 
#'   uncertainty in the calibration curve.
#'   }
#'   \item{`ppc_loo_calibration()`,`ppc_loo_calibration_grouped()`}{
#'   PAV-adjusted calibration plots to assess the calibration of the
#'   leave-one-out (LOO) predictive probabilities, computed by resampling each
#'   observation's posterior predictive draws using LOO importance weights.
#'   }
#'   \item{`ppc_calibration_data()`}{
#'   Data frame containing the data underlying the calibration plots, which can
#'   be used to build custom calibration plots. The `type` argument controls
#'   whether the data frame for `ppc_calibration_overlay()` and its `_grouped``
#'   variant is computed (`type = "overlay"`), or the data frame for 
#'   `ppc_calibration()` and its `_grouped` or `_loo` variant is computed
#'   (`type = "interval"`).
#'   }
#' }
#' 
#' @references 
#' Dimitriadis, T., Gneiting, T., & Jordan, A. I. (2021). Stable 
#' reliability diagrams for probabilistic classifiers. Proceedings of the 
#' National Academy of Sciences, 118(8). 
#' https://doi.org/10.1073/pnas.2016191118
#' 
#' Säilynoja, T., Johnson, A. R., Martin, O. A., & Vehtari, A. (2025). 
#' Recommendations for visual predictive checks in Bayesian workflow. 
#' (Preprint). arXiv. https://doi.org/10.48550/arXiv.2503.01509
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
#' ppc_calibration(y, prep, interval = "confidence")
#' ppc_calibration(y, prep, interval = "consistency")
NULL


#' @rdname PPC-calibration
#' @export
ppc_calibration_overlay <- function(
    y, prep, ..., prob = NULL, linewidth = 0.25, alpha = 0.2) {
  check_ignored_arguments(...)
  data <- ppc_calibration_data(y = y, prep = prep, type = "overlay")
  params <- .calibration_plot_params(
    data = data,
    linewidth = linewidth,
    show_qdots = FALSE,
    prob = 0.95,
    interval = "consistency",
    # currently hardcoded; in future we want to support also "simultaneous"
    interval_type = "pointwise"
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
    xlab("predicted probability") +
    ylab("conditional event probability") +
    NULL
}

#' @rdname PPC-calibration
#' @export
ppc_calibration_overlay_grouped <- function(
    y, prep, group, ..., prob = NULL, linewidth = 0.25, alpha = 0.2) {
  check_ignored_arguments(...)
  data <- ppc_calibration_data(y = y, prep = prep, group = group, type = "overlay")
  params <- .calibration_plot_params(
    data = data,
    linewidth = linewidth,
    show_qdots = FALSE,
    prob = 0.95,
    interval = "consistency",
    # currently hardcoded; in future we want to support also "simultaneous"
    interval_type = "pointwise"
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
    xlab("predicted probability") +
    ylab("conditional event probability") +
    NULL
}

#' @rdname PPC-calibration
#' @param prep For `ppc_calibration()`, `ppc_calibration_grouped()`,
#'   `ppc_calibration_overlay()`, and `ppc_calibration_overlay_grouped()`,
#'   an `S` by `N` matrix of predicted probabilities in `[0, 1]`, where `S` is
#'   the number of draws and `N` the number of observations (`N = length(y)`).
#' @param type For `ppc_calibration_data()`, the data structure to compute:
#'   `"overlay"` for `ppc_calibration_overlay()` or `"interval"` for 
#'   `ppc_calibration()` and their corresponding _grouped and _loo variants.
#' @param lw For `ppc_loo_calibration()` and `ppc_loo_calibration_grouped()`,
#'   a matrix of log weights with the same dimensions as `yrep`. Either
#'   `psis_object` or `lw` has to be specified.
#' @param psis_object For `ppc_loo_calibration()` and `ppc_loo_calibration_grouped()`,
#'   an object of class `"psis"` that is created when the `loo()` function calls
#'   `psis()` internally to do the PSIS procedure. Either `psis_object` or `lw`
#'   has to be specified.
#' @param prob Probability used to compute the uncertainty intervals. Is `NULL`
#'   for `ppc_calibration_overlay()` and `ppc_calibration_overlay_grouped()`, 
#'   where no intervals are computed, and defaults to `0.95` for the other 
#'   calibration functions.
#' @param interval For `ppc_calibration()`, `ppc_calibration_grouped()`,
#'   `ppc_loo_calibration()`, and `ppc_loo_calibration_grouped()`, pointwise 
#'   uncertainty interval around the calibration curve. Choose `"confidence"` 
#'   (default) to answer the question: "Where does the calibration curve of the
#'   model lie?" or `"consistency"` to answer the question: "If the model is 
#'   correctly specified, where would we expect the calibration curve to fall?".
#' @param B For `ppc_calibration()`, `ppc_calibration_grouped()`,
#'   `ppc_loo_calibration()`, and `ppc_loo_calibration_grouped()` that use 
#'   `yrep` with `interval = "confidence"`, the number of bootstrap samples. 
#'   Default is `200`. Ignored if `prep` is used or `interval = "consistency"`.
#' @param show_mean For `ppc_calibration()`, `ppc_calibration_grouped()`,
#'   `ppc_loo_calibration()`, and `ppc_loo_calibration_grouped()`, if `TRUE` 
#'   (default), draw the estimated calibration curve.
#' @param help_text For `ppc_calibration()`, `ppc_calibration_grouped()`, 
#'   `ppc_loo_calibration()`, and `ppc_loo_calibration_grouped()`, if `TRUE` 
#'   (default) display a label in the plot indicating the interval type as 
#'   `CI` (confidence) or `CsI` (consistency) with the selected `prob`.
#' @param show_qdots For `ppc_calibration()`, `ppc_calibration_grouped()`,
#'   `ppc_loo_calibration()`, and `ppc_loo_calibration_grouped()`, if `TRUE` 
#'   (default) add a quantile dot plot at the bottom of the panel to show the 
#'   marginal distribution of predicted probabilities.
#' @param qdots_quantiles For `ppc_calibration()`, `ppc_calibration_grouped()`,
#'   `ppc_loo_calibration()`, and `ppc_loo_calibration_grouped()`, positive
#'   integer indicating the number of dots in the quantile dot plot. Default is
#'   `100`.
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
    help_text = TRUE,
    B = 200,
    show_mean = TRUE,
    show_qdots = TRUE,
    qdots_quantiles = 100,
    ...,
    linewidth = 1,
    alpha = 0.1) {
  check_ignored_arguments(...)
  interval <- rlang::arg_match(interval, values = c("confidence", "consistency"))
  .validate_calibration_qdots_args(show_qdots, qdots_quantiles)

  data <- ppc_calibration_data(
    y = y,
    prep = prep,
    yrep = yrep,
    group = NULL,
    type = "interval",
    prob = prob,
    interval = interval,
    B = B
  )
  params <- .calibration_plot_params(
    data = data,
    linewidth = linewidth,
    show_qdots = show_qdots,
    prob = prob,
    interval = interval,
    # currently hardcoded; in future we want to support also "simultaneous"
    interval_type = "pointwise"
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
    help_text = TRUE,
    B = 200,
    show_mean = TRUE,
    show_qdots = TRUE,
    qdots_quantiles = 100,
    ...,
    linewidth = 1,
    alpha = 0.1) {
  check_ignored_arguments(...)
  interval <- rlang::arg_match(interval, values = c("confidence", "consistency"))
  .validate_calibration_qdots_args(show_qdots, qdots_quantiles)
  data <- ppc_calibration_data(
    y = y,
    prep = prep,
    yrep = yrep,
    group = group,
    type = "interval",
    prob = prob,
    interval = interval,
    B = B
  )
  params <- .calibration_plot_params(
    data = data,
    linewidth = linewidth,
    show_qdots = show_qdots,
    prob = prob,
    interval = interval,
    # currently hardcoded; in future we want to support also "simultaneous"
    interval_type = "pointwise"
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
ppc_calibration_data <- function(
    y,
    prep = NULL,
    yrep = NULL,
    group = NULL,
    type = c("overlay", "interval"),
    prob = .95,
    interval = c("confidence", "consistency"),
    B = 200) {
  type <- match.arg(type)
  y <- validate_y(y)
  n_obs <- length(y)
  if (!is.null(group)) {
    group <- validate_group(group, n_obs)
  } else {
    group <- rep(1, n_obs)
  }
  if (any(y < 0 | y > 1)) {
    abort("'y' must contain values in [0, 1] for calibration.")
  }
  monotone <- .calibration_monotone_fn()
  if (identical(type, "overlay")) {
    if (is.null(prep) || !is.null(yrep)) {
      abort("For type = 'overlay', specify 'prep' and leave 'yrep' as NULL.")
    }
    prep <- validate_predictions(prep, n_obs)
    if (any(prep > 1 | prep < 0)) {
      stop("Values of 'prep' should be predictive probabilities between 0 and 1.")
    }
    d <- .ppd_data(prep, group = group) |>
      group_by(.data$group, .data$rep_id) |>
      mutate(
        ord = order(.data$value),
        y_id = .data$ord,
        value = .data$value[.data$ord],
        cep = monotone(y[.data$ord])
      ) |>
      ungroup() |>
      dplyr::select(dplyr::all_of(c("group", "y_id", "rep_id", "value", "cep")))
  } else {
    interval <- match.arg(interval)
    stopifnot(prob > 0, prob < 1)
    if (is.null(prep) == is.null(yrep)) {
      abort("Specify exactly one of 'prep' or 'yrep'.")
    }
    if (B < 1 || B != as.integer(B)) {
      abort("'B' must be a positive integer.")
    }
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
    d <- tibble::as_tibble(dplyr::bind_rows(out)) |>
      dplyr::select(dplyr::all_of(c("group", "y_id", "value", "cep", "lb", "ub")))
  }
  tibble::as_tibble(d)
}

# internal funtions ---------------------------

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

.calibration_plot_params <- function(data, linewidth, show_qdots, prob, interval,
interval_type) {
  xlim <- c(0, 1)
  x_breaks <- ggplot2::waiver()
  x_labels <- ggplot2::waiver()
  prob_pct <- sub("\\.?0+$", "", sprintf("%.2f", 100 * prob))
  list(
    xlim = xlim,
    x_breaks = x_breaks,
    x_labels = x_labels,
    ylim = c(0 - linewidth / 200, 1 + linewidth / 200),
    y_breaks = pretty(c(0, 1), n = 5),
    ci_label = sprintf(
      "%s%%-%sI (%s)",
      prob_pct, 
      switch(interval, consistency = "Cs", "C"),
      switch(interval_type, pointwise = "ptw.", "sim.")
    )
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
