#' PPC censoring
#'
#' @description Compare the empirical distribution of censored data `y` to the
#'   distributions of simulated/replicated data `yrep` from the posterior
#'   predictive distribution. See the **Plot Descriptions** section, below, for
#'   details.
#'
#'   Although some of the other \pkg{bayesplot} plots can be used with censored
#'   data, `ppc_km_overlay()` is currently the only plotting function designed
#'   *specifically* for censored data. We encourage you to suggest or contribute
#'   additional plots at
#'   [github.com/stan-dev/bayesplot](https://github.com/stan-dev/bayesplot).
#'
#' @name PPC-censoring
#' @family PPCs
#'
#' @template args-y-yrep
#' @param size,alpha Passed to the appropriate geom to control the appearance of
#'   the `yrep` distributions.
#' @param ... Currently only used internally.
#'
#' @template return-ggplot
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{`ppc_km_overlay()`}{
#'   Empirical CCDF estimates of each dataset (row) in `yrep` are overlaid, with
#'   the Kaplan-Meier estimate (Kaplan and Meier, 1958) for `y` itself on top
#'   (and in a darker shade). This is a PPC suitable for right-censored `y`.
#'   Note that the replicated data from `yrep` is assumed to be uncensored. Left
#'   truncation (delayed entry) times for `y` can be specified using
#'   `left_truncation_y`.
#'   }
#'   \item{`ppc_km_overlay_grouped()`}{
#'    The same as `ppc_km_overlay()`, but with separate facets by `group`.
#'   }
#' }
#'
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#' @template reference-km
#'
#' @examples
#' \donttest{
#' color_scheme_set("brightblue")
#'
#' # For illustrative purposes, (right-)censor values y > 110:
#' y <- example_y_data()
#' status_y <- as.numeric(y <= 110)
#' y <- pmin(y, 110)
#'
#' # In reality, the replicated data (yrep) would be obtained from a
#' # model which takes the censoring of y properly into account. Here,
#' # for illustrative purposes, we simply use example_yrep_draws():
#' yrep <- example_yrep_draws()
#' dim(yrep)
#'
#' # Overlay 25 curves
#' ppc_km_overlay(y, yrep[1:25, ], status_y = status_y)
#'
#' # With extrapolation_factor = 1 (no extrapolation)
#' ppc_km_overlay(y, yrep[1:25, ], status_y = status_y, extrapolation_factor = 1)
#'
#' # With extrapolation_factor = Inf (show all posterior predictive draws)
#' ppc_km_overlay(y, yrep[1:25, ], status_y = status_y, extrapolation_factor = Inf)
#'
#' # With separate facets by group:
#' group <- example_group_data()
#' ppc_km_overlay_grouped(y, yrep[1:25, ], group = group, status_y = status_y)
#'
#' # With left-truncation (delayed entry) times:
#' min_vals <- pmin(y, apply(yrep, 2, min))
#' left_truncation_y <- rep(0, length(y))
#' condition <- y > mean(y) / 2
#' left_truncation_y[condition] <- pmin(
#'   runif(sum(condition), min = 0.6, max = 0.99) * y[condition],
#'   min_vals[condition] - 0.001
#' )
#' ppc_km_overlay(y, yrep[1:25, ], status_y = status_y,
#'               left_truncation_y = left_truncation_y)
#' }
NULL

#' @export
#' @rdname PPC-censoring
#' @param status_y The status indicator for the observations from `y`. This must
#'   be a numeric vector of the same length as `y` with values in \{0, 1\} (0 =
#'   right censored, 1 = event).
#' @param left_truncation_y Optional parameter that specifies left-truncation
#'   (delayed entry) times for the observations from `y`. This must be a numeric
#'   vector of the same length as `y`. If `NULL` (default), no left-truncation
#'   is assumed.
#' @param extrapolation_factor A numeric value (>=1) that controls how far the
#'   plot is extended beyond the largest observed value in `y`. The default
#'   value is 1.2, which corresponds to 20 % extrapolation. Note that all
#'   posterior predictive draws may not be shown by default because of the
#'   controlled extrapolation. To display all posterior predictive draws, set
#'   `extrapolation_factor = Inf`.
#'
ppc_km_overlay <- function(
  y,
  yrep,
  ...,
  status_y,
  left_truncation_y = NULL,
  extrapolation_factor = 1.2,
  size = 0.25,
  alpha = 0.7
) {
  check_ignored_arguments(..., ok_args = "add_group")
  add_group <- list(...)$add_group

  suggested_package("survival")
  suggested_package("ggfortify")

  if (!is.numeric(status_y) || length(status_y) != length(y) || !all(status_y %in% c(0, 1))) {
    stop("`status_y` must be a numeric vector of 0s and 1s the same length as `y`.", call. = FALSE)
  }

  if (!is.null(left_truncation_y)) {
    if (!is.numeric(left_truncation_y) || length(left_truncation_y) != length(y)) {
      stop("`left_truncation_y` must be a numeric vector of the same length as `y`.", call. = FALSE)
    }
  }

  if (extrapolation_factor < 1) {
    stop("`extrapolation_factor` must be greater than or equal to 1.", call. = FALSE)
  }
  if (extrapolation_factor == 1.2) {
    message(
      "Note: `extrapolation_factor` now defaults to 1.2 (20%).\n",
      "To display all posterior predictive draws, set `extrapolation_factor = Inf`."
    )
  }

  data <- ppc_data(y, yrep, group = status_y)

  # Modify the status indicator:
  #   * For the observed data ("y"), convert the status indicator back to
  #     a numeric.
  #   * For the replicated data ("yrep"), set the status indicator
  #     to 1 ("event"). This way, the Kaplan-Meier estimator reduces
  #     to "1 - ECDF" with ECDF denoting the ordinary empirical cumulative
  #     distribution function.
  data <- data %>%
    dplyr::mutate(group = ifelse(.data$is_y,
                                 as.numeric(as.character(.data$group)),
                                 1))

  if (is.null(left_truncation_y)) {
    sf_form <- survival::Surv(time = data$value, event = data$group) ~ rep_label
  } else {
    sf_form <- survival::Surv(time = left_truncation_y[data$y_id], time2 = data$value, event = data$group) ~ rep_label
  }

  if (!is.null(add_group)) {
    data <- dplyr::inner_join(data,
                              tibble::tibble(y_id = seq_along(y),
                                             add_group = add_group),
                              by = "y_id")
    sf_form <- update(sf_form, . ~ . + add_group)
  }
  sf <- survival::survfit(
    sf_form,
    data = data
  )
  names(sf$strata) <- sub("add_group=", "add_group:", names(sf$strata)) # Needed to split the strata names in ggfortify:::fortify.survfit() properly.
  fsf <- fortify(sf)
  if(any(grepl("add_group", levels(fsf$strata)))){
    strata_split <- strsplit(as.character(fsf$strata), split = ", add_group:")
    fsf$strata <- as.factor(sapply(strata_split, "[[", 1))
    fsf$group <- as.factor(sapply(strata_split, "[[", 2))
  }

  fsf$is_y_color <- as.factor(sub("\\[rep\\] \\(.*$", "rep", sub("^italic\\(y\\)", "y", fsf$strata)))
  fsf$is_y_size <- ifelse(fsf$is_y_color == "yrep", size, 1)
  fsf$is_y_alpha <- ifelse(fsf$is_y_color == "yrep", alpha, 1)

  max_time_y <- max(y, na.rm = TRUE)
  fsf <- fsf %>%
    dplyr::filter(.data$is_y_color != "yrep" | .data$time <= max_time_y * extrapolation_factor)

  # Ensure that the observed data gets plotted last by reordering the
  # levels of the factor "strata"
  fsf$strata <- factor(fsf$strata, levels = rev(levels(fsf$strata)))

  ggplot(data = fsf,
         mapping = aes(x = .data$time,
                       y = .data$surv,
                       color = .data$is_y_color,
                       group = .data$strata,
                       size = .data$is_y_size,
                       alpha = .data$is_y_alpha)) +
    geom_step() +
    hline_at(
      0.5,
      linewidth = 0.1,
      linetype = 2,
      color = get_color("dh")
    ) +
    hline_at(
      c(0, 1),
      linewidth = 0.2,
      linetype = 2,
      color = get_color("dh")
    ) +
    scale_size_identity() +
    scale_alpha_identity() +
    scale_color_ppc() +
    scale_y_continuous(breaks = c(0, 0.5, 1)) +
    xlab(y_label()) +
    yaxis_title(FALSE) +
    xaxis_title(FALSE) +
    yaxis_ticks(FALSE) +
    bayesplot_theme_get()
}

#' @export
#' @rdname PPC-censoring
#' @template args-group
ppc_km_overlay_grouped <- function(
  y,
  yrep,
  group,
  ...,
  status_y,
  left_truncation_y = NULL,
  extrapolation_factor = 1.2,
  size = 0.25,
  alpha = 0.7
) {
  check_ignored_arguments(...)

  p_overlay <- ppc_km_overlay(
    y = y,
    yrep = yrep,
    add_group = group,
    ...,
    status_y = status_y,
    left_truncation_y = left_truncation_y,
    size = size,
    alpha = alpha,
    extrapolation_factor = extrapolation_factor
  )

  p_overlay +
    facet_wrap("group") +
    force_axes_in_facets()
}
