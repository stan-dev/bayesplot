#' PPD distributions
#'
#' Plot posterior predictive distributions.
#' See the \strong{Plot Descriptions} section, below, for details.
#'
#' @name PPD-distributions
#' @family PPDs

#' @rdname PPD-distributions
#' @export
ppd_dens <- function(ypred, ..., trim = FALSE, size = 0.5, alpha = 1) {
  check_ignored_arguments(...)
  y <- rep(1, ncol(ypred)) # fake y (temporary hack to be able to use ppc_data())
  data <- dplyr::filter(ppc_data(y, ypred), !.data$is_y)
  levels(data$rep_label) <- gsub("rep", "pred", levels(data$rep_label))

  ggplot(data) +
    aes_(x = ~ value, fill = "ypred", color = "ypred") +
    geom_density(size = size, alpha = alpha, trim = trim) +
    scale_fill_ppc_dist(
      values = get_color("l"),
      labels = expression(italic(y)[pred])
    ) +
    scale_color_ppc_dist(
      values = get_color("lh"),
      labels = expression(italic(y)[pred])
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
  y <- rep(1, ncol(ypred)) # fake y (temporary hack to be able to use ppc_data())
  data <- dplyr::filter(ppc_data(y, ypred), !.data$is_y)

  ggplot(data) +
    aes_(x = ~ value) +
    stat_density(
      aes_(group = ~ rep_id, color = "ypred"),
      geom = "line",
      position = "identity",
      size = size,
      alpha = alpha,
      trim = trim,
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n_dens
    ) +
    scale_color_ppc_dist(
      values = get_color("lh"),
      labels = expression(italic(y)[pred])
    ) +
    bayesplot_theme_get() +
    dont_expand_axes() +
    legend_none() +
    yaxis_title(FALSE) +
    xaxis_title(FALSE) +
    yaxis_text(FALSE) +
    yaxis_ticks(FALSE)
}

