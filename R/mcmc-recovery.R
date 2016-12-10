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
#'   default is \code{0.5} (50\% interval).
#' @param point_est The point estimate to show. Either \code{"median"} (the
#'   default), \code{"mean"}, or \code{"none"}.
#' @param size Passed to \code{\link[ggplot2]{geom_point}}.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' alpha <- 1; beta <- c(-.5, .5); sigma <- 2
#' X <- matrix(rnorm(200), 100, 2)
#' y <- rnorm(100, mean = c(alpha + X %*% beta), sd = sigma)
#' fit <- stan_glm(y ~ X)
#' draws <- as.matrix(fit)
#' print(colnames(draws))
#' mcmc_recover(draws, true, prob = )
#' }
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
           prob_outer = 0.9,
           point_est = c("median", "mean")) {
    x <- merge_chains(prepare_mcmc_array(x))
    stopifnot(prob_outer >= prob)
    alpha1 <- (1 - prob) / 2
    alpha2 <- (1 - prob_outer) / 2
    probs <- sort(c(alpha1, 1 - alpha1, alpha2, 1 - alpha2))
    intervals <- t(apply(x, 2, quantile, probs = probs))
    colnames(intervals) <- c("ll", "l", "u", "uu")
    point <- apply(x, 2, match.arg(point_est))
    plot_data <- data.frame(Parameter = rownames(intervals),
                            intervals, point, true)
    facet_args[["facets"]] <- ~ Parameter
    if (is.null(facet_args[["strip.position"]]))
      facet_args[["strip.position"]] <- "top"
    if (is.null(facet_args[["scales"]]))
      facet_args[["scales"]] <- "free"

    plot_caption <- paste0("Showing ", round(prob * 100, 1), "% and ",
                           round(prob_outer * 100, 1), "% intervals")
    ggplot(plot_data, aes_(x = 0, xend = 0)) +
      geom_segment(
        aes_(y = ~ ll, yend = ~ uu),
        color = get_color("lh"),
        lineend = "round"
      ) +
      geom_segment(
        aes_(y = ~ l, yend = ~ u),
        color = get_color("l"),
        size = 2,
        lineend = "round"
      ) +
      geom_point(
        aes_(y = ~ point,
             shape = "Estimated",
             color = "Estimated",
             fill = "Estimated"),
        size = 4
      ) +
      geom_point(
        aes_(y = ~ true,
             shape = "True",
             color = "True",
             fill = "True"),
        size = 4
      ) +
      scale_color_manual("", values = c(Estimated = get_color("l"), True = get_color("dh"))) +
      scale_fill_manual("", values = c(Estimated = get_color("m"), True = get_color("dh"))) +
      scale_shape_manual("", values = c(Estimated = 21, True = 24)) +
      scale_x_continuous(limits = c(-0.5, 1)) +
      do.call("facet_wrap", facet_args) +
      labs(y = "Value", x = "Parameter", caption = plot_caption) +
      theme_default() +
      theme(axis.line.x = element_blank(),
            plot.caption = element_text(hjust = 0)) +
      xaxis_ticks(FALSE) +
      xaxis_text(FALSE) +
      xaxis_title(FALSE) +
      yaxis_title(FALSE)
  }
