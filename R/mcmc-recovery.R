#' Compare MCMC estimates to true parameter values
#'
#' See the \strong{Plot Descriptions} section, below, for details.
#'
#' @name MCMC-recovery
#' @family MCMC
#'
#' @template args-mcmc-x
#' @param true A numeric vector of true values for each of the parameters in
#'   \code{x}. There should be one value in \code{true} for each parameter
#'   included in \code{x} and the order of the parameters in \code{true} should
#'   be the same as the order of the parameters in \code{x}.
#' @param ... Currently unused.
#' @param prob The probability mass to include in the interval. The
#'   default is \code{0.9} (90\% interval).
#' @param point_est The point estimate to show. Either \code{"median"} (the
#'   default), \code{"mean"}, or \code{"none"}.
#' @param size Passed to \code{\link[ggplot2]{geom_point}}.
#'
NULL

#' @rdname MCMC-recovery
#' @export
mcmc_recover <-
  function(x,
           true,
           facet_args = list(),
           ...,
           prob = 0.5,
           point_est = c("median", "mean")) {
    x <- merge_chains(prepare_mcmc_array(x))
    alpha <- (1 - prob) / 2
    intervals <- t(apply(x, 2, quantile, probs = c(alpha, 1 - alpha)))
    colnames(intervals) <- c("lower", "upper")
    point <- apply(x, 2, match.arg(point_est))
    plot_data <- data.frame(Parameter = rownames(intervals),
                            intervals, point, true)

    facet_args[["facets"]] <- ~ Parameter
    if (is.null(facet_args[["strip.position"]]))
      facet_args[["strip.position"]] <- "top"
    if (is.null(facet_args[["scales"]]))
      facet_args[["scales"]] <- "free"
    # if (is.null(facet_args[["nrow"]]) && is.null(facet_args[["ncol"]]))
    #   facet_args[["nrow"]] <- 1

    ggplot(plot_data, aes_(x = 0, ymin = ~ lower, ymax = ~ upper)) +
      geom_linerange(color = get_color("l"), size = 1.5) +
      geom_segment(
        aes_(x = 0, xend = 0, y = ~ true, yend = ~ point),
        color = "gray85",
        linetype = 2,
        inherit.aes = FALSE
      ) +
      geom_point(aes_(y = ~ point, shape = "Estimated", color = "Estimated"),
                 size = rel(3)) +
      geom_point(aes_(y = ~ true, shape = "True", color = "True"),
                 size = rel(3)) +
      scale_color_manual("", values = c(Estimated = get_color("lh"), True = get_color("d"))) +
      scale_shape_manual("", values = c(Estimated = 16, True = 17)) +
      scale_x_continuous(limits = c(-0.5, 1)) +
      do.call("facet_wrap", facet_args) +
      labs(y = "Value", x = "Parameter") +
      theme_default() +
      theme(axis.line.x = element_blank()) +
      xaxis_ticks(FALSE) +
      xaxis_text(FALSE) +
      xaxis_title(FALSE) +
      yaxis_title(FALSE)
  }
