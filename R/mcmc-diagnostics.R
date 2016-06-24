#' Rhat, effective sample size, Monte Carlo standard error
#'
#' @name MCMC-diagnostics
#' @family MCMC
#'
#' @param ... Currently ignored.
#'
#' @template return-ggplot
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{\code{mcmc_rhat_hist, mcmc_rhat_dot}}{
#'   }
#'   \item{\code{mcmc_neff_hist, mcmc_neff_dot}}{
#'   }
#'   \item{\code{mcmc_mcse_hist, mcmc_mcse_dot}}{
#'   }
#'   \item{\code{mcmc_diagnostics}}{
#'   }
#' }
#'
#' @template seealso-color-scheme
#'
NULL

#' @rdname MCMC-diagnostics
#' @export
#' @param rhat Vector of \code{\link[=r_hat]{Rhat}} estimates.
#' @template args-hist
mcmc_rhat_hist <- function(rhat, ..., binwidth = NULL) {
  ggplot(
    data.frame(x = rhat, lev = factor_rhat(rhat)),
    aes_(
      x = ~ x,
      y = ~ ..density..,
      color = ~ lev,
      fill = ~ lev
    )
  ) +
    geom_histogram(
      size = .25,
      na.rm = TRUE,
      binwidth = binwidth
    ) +
    scale_color_rhat() +
    scale_fill_rhat() +
    labs(x = bquote(hat(R)), y = NULL) +
    dont_expand_y_axis(c(0.005, 0)) +
    theme_default(y_text = FALSE)
}

#' @rdname MCMC-diagnostics
#' @export
#' @param size An optional value to override \code{\link[ggplot2]{geom_point}}'s
#'   default size.
#'
mcmc_rhat_dot <- function(rhat, ..., size = NULL) {
  stopifnot(length(rhat) > 1)

  frhat <- if (!is.null(names(rhat))) {
    factor(rhat, labels = names(rhat))
  } else {
    factor(rhat)
  }
  data <- data.frame(y = rhat, x = frhat)
  graph <- ggplot(data, aes_(x = ~ x, y = ~ y))

  if (any(rhat > 1.05))
    graph <- graph + geom_hline(
      yintercept = 1.05,
      color = "gray",
      linetype = 2,
      size = 0.25
    )
  if (any(rhat > 1.1))
    graph <- graph + geom_hline(
      yintercept = 1.1,
      color = "gray",
      linetype = 2,
      size = 0.25
    )

  .rhat_dots <- function(size = NULL) {
    args <- list(color = get_color("mid_highlight"),
                 fill = get_color("mid"),
                 shape = 21,
                 na.rm = TRUE)
    do.call("geom_point", c(args, size = size))
  }

  graph +
    geom_segment(
      aes_(xend = ~x, yend = ~1),
      color = get_color("light"),
      na.rm = TRUE
    ) +
    geom_hline(yintercept = 1) +
    .rhat_dots(size) +
    labs(x = NULL, y = bquote(hat(R))) +
    scale_fill_rhat() +
    scale_color_rhat() +
    theme_default(y_text = FALSE) +
    coord_flip()
}
