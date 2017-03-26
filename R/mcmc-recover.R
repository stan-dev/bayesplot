#' Compare MCMC estimates to "true" parameter values
#'
#' Plots comparing MCMC estimates to "true" parameter values. Before fitting a
#' model to real data it is useful to simulate data according to the model using
#' known (fixed) parameter values and to check that these "true" parameter
#' values are (approximately) recovered by fitting the model to the simulated
#' data. See the \strong{Plot Descriptions} section, below, for details on the
#' available plots.
#'
#' @name MCMC-recover
#' @family MCMC
#'
#' @template args-mcmc-x
#' @param true A numeric vector of "true" values of the parameters in \code{x}.
#'   There should be one value in \code{true} for each parameter included in
#'   \code{x} and the order of the parameters in \code{true} should be the same
#'   as the order of the parameters in \code{x}.
#' @param batch Optionally, a vector-like object (numeric, character, integer,
#'   factor) used to split the parameters into batches. If \code{batch} is
#'   specified, it must have the same length as \code{true} and be in the same
#'   order as \code{true}. Parameters in the same batch will be grouped together
#'   in the same facet in the plot (see the \strong{Examples} section, below).
#'   The default is to group all parameters together into a single batch.
#'   Changing the default is most useful when parameters are on very different
#'   scales, in which case \code{batch} can be used to group them into batches
#'   within which it makes sense to use the same \eqn{y}-axis.
#' @param facet_args Arguments (other than \code{facets}) passed to
#'   \code{\link[ggplot2]{facet_wrap}} to control faceting.
#' @param ... Currently unused.
#' @param prob The probability mass to include in the inner interval. The
#'   default is \code{0.5} (50\% interval).
#' @param prob_outer The probability mass to include in the outer interval. The
#'   default is \code{0.9} (90\% interval).
#' @param point_est The point estimate to show. Either \code{"median"} (the
#'   default), \code{"mean"}, or \code{"none"}.
#' @param size,alpha Passed to \code{\link[ggplot2]{geom_point}} to control the
#'   appearance of plotted points.
#'
#' @template return-ggplot
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{\code{mcmc_recover_intervals}}{
#'    Central intervals and point estimates computed from MCMC draws, with
#'    "true" values plotted using a different shape.
#'   }
#' }
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' alpha <- 1; beta <- rnorm(10, 0, 3); sigma <- 2
#' X <- matrix(rnorm(1000), 100, 10)
#' y <- rnorm(100, mean = c(alpha + X %*% beta), sd = sigma)
#' fit <- stan_glm(y ~ ., data = data.frame(y, X))
#' draws <- as.matrix(fit)
#' print(colnames(draws))
#' true <- c(alpha, beta, sigma)
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
#' # scatterplot of posterior means vs true values
#' mcmc_recover_scatter(draws, true, point_est = "mean")
#' }
#'
NULL

#' @rdname MCMC-recover
#' @export
mcmc_recover_intervals <-
  function(x,
           true,
           batch = rep(1, length(true)),
           facet_args = list(),
           ...,
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
    if (point_est == "none")
      point_est <- NULL

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
    facet_args[["facets"]] <- ~ Batch
    if (is.null(facet_args[["strip.position"]]))
      facet_args[["strip.position"]] <- "top"
    if (is.null(facet_args[["scales"]]))
      facet_args[["scales"]] <- "free"

    plot_caption <- paste0("Showing ", round(prob * 100, 1), "% and ",
                           round(prob_outer * 100, 1), "% intervals")
    graph <- ggplot(plot_data, aes_(x = ~ Parameter, xend = ~ Parameter)) +
      geom_segment(
        aes_(y = ~ ll, yend = ~ uu, color = "Estimated"),
        lineend = "round",
        show.legend = FALSE
      ) +
      geom_segment(
        aes_(y = ~ l, yend = ~ u, color = "Estimated"),
        size = 2,
        lineend = "round",
        show.legend = FALSE
      )

    if (!is.null(point_est))
      graph <- graph +
      geom_point(
        aes_(y = ~ Point, shape = "Estimated",
             color = "Estimated", fill = "Estimated"),
        size = size
      )

    graph <- graph +
      geom_point(
        aes_(y = ~ True, shape = "True",
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
      theme_default() +
      theme(plot.caption = element_text(hjust = 0)) +
      xaxis_title(FALSE) +
      yaxis_title(FALSE)

    if (!all_separate)
      return(graph + xaxis_text(face = "bold") + facet_text(FALSE))

    graph +
      theme(axis.line.x = element_blank()) +
      xaxis_ticks(FALSE) +
      xaxis_text(FALSE)
  }


#' @rdname MCMC-recover
#' @export
mcmc_recover_scatter <-
  function(x,
           true,
           batch = rep(1, length(true)),
           facet_args = list(),
           ...,
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
    all_separate <- length(unique(batch)) == length(true)
    point_est <- match.arg(point_est)
    plot_data <- data.frame(
      Parameter = colnames(x),
      Point = apply(x, 2, point_est),
      True = true
    )
    if (!all_separate) {
      plot_data$Batch <- factor(batch, levels = unique(batch))
    } else {
      plot_data$Batch <-
        factor(colnames(x), levels = colnames(x)[as.integer(as.factor(batch))])
    }

    facet_args[["facets"]] <- ~ Batch
    if (is.null(facet_args[["strip.position"]]))
      facet_args[["strip.position"]] <- "top"
    if (is.null(facet_args[["scales"]]))
      facet_args[["scales"]] <- "free"

    graph <- ggplot(plot_data, aes_(x = ~ True, y = ~ Point)) +
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
      do.call("facet_wrap", facet_args) +
      labs(y = "Estimated", x = "True") +
      theme_default()

    if (length(unique(batch)) == 1) {
      g <- ggplot_build(graph)
      xylim <- g$layout$panel_ranges[[1]]
      xylim <- range(xylim$y.range, xylim$x.range)
      graph <- graph + coord_fixed(x = xylim, y = xylim)
    }

    if (!all_separate)
      return(graph + facet_text(FALSE))

    graph
  }
