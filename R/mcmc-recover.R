#' Compare MCMC estimates to "true" parameter values
#'
#' Plots comparing MCMC estimates to "true" parameter values. Before fitting a
#' model to real data it is useful to simulate data according to the model using
#' known (fixed) parameter values and to check that these "true" parameter
#' values are (approximately) recovered by fitting the model to the simulated
#' data. See the **Plot Descriptions** section, below, for details on the
#' available plots.
#'
#' @name MCMC-recover
#' @family MCMC
#'
#' @template args-mcmc-x
#' @template args-facet_args
#' @param true A numeric vector of "true" values of the parameters in `x`.
#'   There should be one value in `true` for each parameter included in
#'   `x` and the order of the parameters in `true` should be the same
#'   as the order of the parameters in `x`.
#' @param batch Optionally, a vector-like object (numeric, character, integer,
#'   factor) used to split the parameters into batches. If `batch` is
#'   specified, it must have the same length as `true` and be in the same
#'   order as `true`. Parameters in the same batch will be grouped together
#'   in the same facet in the plot (see the **Examples** section, below).
#'   The default is to group all parameters together into a single batch.
#'   Changing the default is most useful when parameters are on very different
#'   scales, in which case `batch` can be used to group them into batches
#'   within which it makes sense to use the same y-axis.
#' @param ... Currently unused.
#' @param prob The probability mass to include in the inner interval. The
#'   default is `0.5` (50% interval).
#' @param prob_outer The probability mass to include in the outer interval. The
#'   default is `0.9` (90% interval).
#' @param point_est The point estimate to show. Either `"median"` (the
#'   default), `"mean"`, or `"none"`.
#' @param size,alpha Passed to [ggplot2::geom_point()] to control the
#'   appearance of plotted points.
#'
#' @template return-ggplot
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{`mcmc_recover_intervals()`}{
#'    Central intervals and point estimates computed from MCMC draws, with
#'    "true" values plotted using a different shape.
#'   }
#'   \item{`mcmc_recover_scatter()`}{
#'    Scatterplot of posterior means (or medians) against "true" values.
#'   }
#'   \item{`mcmc_recover_hist()`}{
#'    Histograms of the draws for each parameter with the "true" value overlaid
#'    as a vertical line.
#'   }
#' }
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' alpha <- 1; beta <- rnorm(10, 0, 3); sigma <- 2
#' X <- matrix(rnorm(1000), 100, 10)
#' y <- rnorm(100, mean = c(alpha + X %*% beta), sd = sigma)
#' fit <- stan_glm(y ~ ., data = data.frame(y, X), refresh = 0)
#' draws <- as.matrix(fit)
#' print(colnames(draws))
#' true <- c(alpha, beta, sigma)
#'
#' mcmc_recover_intervals(draws, true)
#'
#' # put the coefficients on X into the same batch
#' mcmc_recover_intervals(draws, true, batch = c(1, rep(2, 10), 1))
#' # equivalent
#' mcmc_recover_intervals(draws, true, batch = grepl("X", colnames(draws)))
#' # same but facets stacked vertically
#' mcmc_recover_intervals(draws, true,
#'                        batch = grepl("X", colnames(draws)),
#'                        facet_args = list(ncol = 1),
#'                        size = 3)
#'
#' # each parameter in its own facet
#' mcmc_recover_intervals(draws, true, batch = 1:ncol(draws))
#' # same but in a different order
#' mcmc_recover_intervals(draws, true, batch = c(1, 3, 4, 2, 5:12))
#' # present as bias by centering with true values
#' mcmc_recover_intervals(sweep(draws, 2, true), rep(0, ncol(draws))) + hline_0()
#'
#'
#' # scatterplot of posterior means vs true values
#' mcmc_recover_scatter(draws, true, point_est = "mean")
#'
#'
#' # histograms of parameter draws with true value added as vertical line
#' color_scheme_set("brightblue")
#' mcmc_recover_hist(draws[, 1:4], true[1:4])
#' }
#'
NULL

#' @rdname MCMC-recover
#' @export
mcmc_recover_intervals <-
  function(x,
           true,
           batch = rep(1, length(true)),
           ...,
           facet_args = list(),
           prob = 0.5,
           prob_outer = 0.9,
           point_est = c("median", "mean", "none"),
           size = 4,
           alpha = 1) {

    check_ignored_arguments(...)
    x <- merge_chains(prepare_mcmc_array(x))

    stopifnot(
      is.numeric(true),
      ncol(x) == length(true),
      length(batch) == length(true),
      prob_outer >= prob,
      prob > 0,
      prob_outer <= 1
    )
    all_separate <- length(unique(batch)) == length(true)
    point_est <- match.arg(point_est)
    if (point_est == "none") {
      point_est <- NULL
    }

    alpha1 <- (1 - prob) / 2
    alpha2 <- (1 - prob_outer) / 2
    probs <- sort(c(alpha1, 1 - alpha1, alpha2, 1 - alpha2))
    intervals <- t(apply(x, 2, quantile, probs = probs))
    colnames(intervals) <- c("ll", "l", "u", "uu")

    plot_data <- data.frame(
      Parameter = rownames(intervals),
      True = true,
      Point = apply(x, 2, point_est %||% function(x) NA),
      intervals
    )
    if (!all_separate) {
      plot_data$Batch <- factor(batch, levels = unique(batch))
    } else {
      plot_data$Batch <-
        factor(rownames(intervals),
               levels = rownames(intervals)[as.integer(as.factor(batch))])
    }
    facet_args[["facets"]] <- "Batch"
    facet_args[["strip.position"]] <- facet_args[["strip.position"]] %||% "top"
    facet_args[["scales"]] <- facet_args[["scales"]] %||% "free"

    plot_caption <- paste0("Showing ", round(prob * 100, 1), "% and ",
                           round(prob_outer * 100, 1), "% intervals")
    graph <- ggplot(plot_data, aes(x = .data$Parameter, xend = .data$Parameter)) +
      geom_segment(
        aes(y = .data$ll, yend = .data$uu, color = "Estimated"),
        lineend = "round",
        show.legend = FALSE
      ) +
      geom_segment(
        aes(y = .data$l, yend = .data$u, color = "Estimated"),
        linewidth = 2,
        lineend = "round",
        show.legend = FALSE
      ) +
      bayesplot_theme_get()

    if (!is.null(point_est)) {
      graph <- graph +
      geom_point(
        aes(y = .data$Point, shape = "Estimated",
            color = "Estimated", fill = "Estimated"),
        size = size
      )
    }

    graph <- graph +
      geom_point(
        aes(y = .data$True, shape = "True",
            color = "True", fill = "True"),
        size = size,
        alpha = alpha
      ) +
      scale_color_manual(
        name = "",
        values = c(Estimated = get_color("d"), True = get_color("dh")),
        guide = if (is.null(point_est)) "none" else "legend"
      ) +
      scale_fill_manual(
        name = "",
        values = c(Estimated = get_color("d"), True = get_color("l"))
      ) +
      scale_shape_manual(
        name = "",
        values = c(Estimated = 21, True = 24)
      ) +
      do.call("facet_wrap", facet_args) +
      labs(y = "Value", x = "Parameter", subtitle = plot_caption) +
      theme(plot.caption = element_text(hjust = 0)) +
      xaxis_title(FALSE) +
      yaxis_title(FALSE)

    if (all_separate) {
      return(
        graph +
          theme(axis.line.x = element_blank()) +
          xaxis_ticks(FALSE) +
          xaxis_text(FALSE)
      )
    }

    graph +
      xaxis_text(face = "bold") +
      facet_text(FALSE)
  }


#' @rdname MCMC-recover
#' @export
mcmc_recover_scatter <-
  function(x,
           true,
           batch = rep(1, length(true)),
           ...,
           facet_args = list(),
           point_est = c("median", "mean"),
           size = 3,
           alpha = 1) {

  check_ignored_arguments(...)
  x <- merge_chains(prepare_mcmc_array(x))

  stopifnot(
    is.numeric(true),
    ncol(x) == length(true),
    length(batch) == length(true)
  )

  one_true_per_batch <- length(unique(batch)) == length(true)
  one_batch <- length(unique(batch)) == 1

  point_est <- match.arg(point_est)
  plot_data <- data.frame(
    Parameter = colnames(x),
    Point = apply(x, 2, point_est),
    True = true
  )

  if (!one_true_per_batch) {
    plot_data$Batch <- factor(batch, levels = unique(batch))
  } else {
    plot_data$Batch <-
      factor(colnames(x), levels = colnames(x)[as.integer(as.factor(batch))])
  }

  facet_args[["facets"]] <- "Batch"
  facet_args[["strip.position"]] <- facet_args[["strip.position"]] %||% "top"
  facet_args[["scales"]] <- facet_args[["scales"]] %||% "free"

  # To ensure that the x and y scales have the same range, find the min and max
  # value on each coordinate. plot them invisibly with geom_blank() later on.
  corners <- plot_data %>%
    group_by(.data$Batch) %>%
    summarise(
      min = min(pmin(.data$Point, .data$True)),
      max = max(pmax(.data$Point, .data$True))
    )

  graph <-
    ggplot(plot_data, aes(x = .data$True, y = .data$Point)) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = 2,
      color = "black"
    ) +
    geom_point(
      shape = 21,
      color = get_color("mh"),
      fill = get_color("m"),
      size = size,
      alpha = alpha
    ) +
    geom_blank(aes(x = min, y = min), data = corners) +
    geom_blank(aes(x = max, y = max), data = corners) +
    do.call("facet_wrap", facet_args) +
    labs(x = "True", y = "Estimated") +
    bayesplot_theme_get()

  if (one_batch) {
    graph <- graph + facet_text(FALSE)
  }

  graph
}


#' @rdname MCMC-recover
#' @export
#' @template args-hist
mcmc_recover_hist <-
  function(x,
           true,
           ...,
           facet_args = list(),
           binwidth = NULL,
           bins = NULL,
           breaks = NULL) {

    check_ignored_arguments(...)
    x <- merge_chains(prepare_mcmc_array(x))

    stopifnot(
      is.numeric(true),
      ncol(x) == length(true)
    )

    vline_data <- data.frame(Parameter = colnames(x), True = true)
    hist_data <- melt_mcmc(x)[, -1]
    vline_data$Parameter <- factor(vline_data$Parameter, levels = levels(hist_data$Parameter))

    facet_args[["facets"]] <- "Parameter"
    facet_args[["scales"]] <- facet_args[["scales"]] %||% "free"

    ggplot() +
      geom_histogram(
        aes(x = .data$Value, fill = "Estimated"),
        data = hist_data,
        color = get_color("lh"),
        linewidth = 0.25,
        binwidth = binwidth,
        bins = bins,
        breaks = breaks
      ) +
      geom_vline(
        aes(xintercept = .data$True, color = "True"),
        data = vline_data,
        linewidth = 1.5
      ) +
      do.call("facet_wrap", facet_args) +
      scale_fill_manual("", values = get_color("l")) +
      scale_color_manual("", values = get_color("dh")) +
      guides(color = guide_legend(), fill = guide_legend(order = 1)) +
      dont_expand_y_axis() +
      bayesplot_theme_get() +
      reduce_legend_spacing(0.25) +
      xaxis_title(FALSE) +
      yaxis_text(FALSE) +
      yaxis_ticks(FALSE) +
      yaxis_title(FALSE)
  }
