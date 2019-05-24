#' PPD distributions
#'
#' Plot posterior predictive distributions.
#' See the \strong{Plot Descriptions} section, below, for details.
#'
#' @name PPD-distributions
#' @family PPDs
#'
#' @inheritParams PPC-distributions
#' @param ypred An \eqn{S} by \eqn{N} matrix of draws from the posterior
#'   predictive distribution, where \eqn{S} is the size of the posterior sample
#'   (or subset of the posterior sample used to generate \code{yrep}) and
#'   \eqn{N} is the number of predicted observations.
#'
NULL

#' @rdname PPD-distributions
#' @export
ppd_data <- function(ypred, group = NULL) {
  ypred <- validate_predictions(ypred)
  if (!is.null(group)) {
    group <- validate_group(group, n_obs = ncol(ypred))
  }
  .ppd_data(predictions = ypred,
            observations = NULL,
            group = group)
}

#' @rdname PPD-distributions
#' @export
ppd_dens <- function(ypred, ..., trim = FALSE, size = 0.5, alpha = 1) {
  check_ignored_arguments(...)
  data <- ppd_data(ypred)
  ggplot(data) +
    aes_(x = ~ value, fill = "ypred", color = "ypred") +
    geom_density(
      fill = get_color("l"),
      color = get_color("lh"),
      size = size,
      alpha = alpha,
      trim = trim
    ) +
    bayesplot_theme_get() +
    facet_wrap_parsed("rep_label") +
    force_axes_in_facets() +
    dont_expand_y_axis() +
    legend_none() +
    yaxis_text(FALSE) +
    yaxis_title(FALSE) +
    yaxis_ticks(FALSE) +
    xaxis_title(FALSE) +
    facet_text(FALSE) +
    facet_bg(FALSE)
}

#' @rdname PPD-distributions
#' @export
ppd_hist <- function(ypred, ..., binwidth = NULL, breaks = NULL,
                     freq = TRUE) {
  check_ignored_arguments(...)
  data <- ppd_data(ypred)
  ggplot(data, set_hist_aes(freq)) +
    geom_histogram(
      fill = get_color("l"),
      color = get_color("lh"),
      size = 0.25,
      binwidth = binwidth,
      breaks = breaks
    ) +
    bayesplot_theme_get() +
    facet_wrap_parsed("rep_label") +
    force_axes_in_facets() +
    dont_expand_y_axis() +
    legend_none() +
    yaxis_text(FALSE) +
    yaxis_title(FALSE) +
    yaxis_ticks(FALSE) +
    xaxis_title(FALSE) +
    facet_text(FALSE) +
    facet_bg(FALSE)
}


#' @rdname PPD-distributions
#' @export
ppd_dens_overlay <- function(ypred,
                            ...,
                            size = 0.25,
                            alpha = 0.7,
                            trim = FALSE,
                            bw = "nrd0",
                            adjust = 1,
                            kernel = "gaussian",
                            n_dens = 1024) {

  check_ignored_arguments(...)
  data <- ppd_data(ypred)
  ggplot(data, aes_(x = ~ value)) +
    stat_density(
      aes_(group = ~ rep_id),
      geom = "line",
      position = "identity",
      color = get_color("lh"),
      size = size,
      alpha = alpha,
      trim = trim,
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n_dens
    ) +
    bayesplot_theme_get() +
    dont_expand_axes() +
    legend_none() +
    yaxis_title(FALSE) +
    xaxis_title(FALSE) +
    yaxis_text(FALSE) +
    yaxis_ticks(FALSE)
}

#' @rdname PPD-distributions
#' @export
ppd_boxplot <- function(ypred, ..., notch = TRUE, size = 0.5, alpha = 1) {
  check_ignored_arguments(...)
  data <- ppd_data(ypred)
  ggplot(data, aes_(x = ~ rep_label, y = ~ value)) +
    geom_boxplot(
      fill = get_color("l"),
      color = get_color("mh"),
      notch = notch,
      size = size,
      alpha = alpha,
      outlier.color = get_color("lh")
    ) +
    bayesplot_theme_get() +
    yaxis_title(FALSE) +
    xaxis_ticks(FALSE) +
    xaxis_text(FALSE) +
    xaxis_title(FALSE)
}


# internal ----------------------------------------------------------------

#' Back end for both `ppd_data()` and `ppc_data()`
#'
#' @noRd
#' @param predictions SxN matrix of predictions (`ypred`, or `yrep`).
#' @param y User's `y` argument, if applicable.
#' @param group User's `group` argument.
#'
.ppd_data <- function(predictions, y = NULL, group = NULL) {
  if (!is.null(y)) {
    data <- melt_and_stack(y, predictions)
  } else {
    data <- melt_predictions(predictions)
    levels(data$rep_label) <- gsub("rep", "pred", levels(data$rep_label))
  }
  if (!is.null(group)) {
    group_indices <- tibble::tibble(group, y_id = seq_along(group))
    data <- data %>%
      left_join(group_indices, by = "y_id") %>%
      select(.data$group, dplyr::everything())
  }
  data
}

