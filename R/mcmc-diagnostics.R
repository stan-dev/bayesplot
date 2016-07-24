#' Rhat and effective sample size
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
#'   \item{\code{mcmc_rhat_hist, mcmc_rhat_dots}}{
#'   }
#'   \item{\code{mcmc_neff_hist, mcmc_neff_dots}}{
#'   }
#'   \item{\code{mcmc_diagnostics}}{
#'   }
#' }
#'
#' @template seealso-color-scheme
#'
#' @examples
#'
#' # fake rhat values to use for demonstration
#' rhat <- c(runif(100, 1, 1.15))
#' mcmc_rhat_hist(rhat)
#' mcmc_rhat_dots(rhat)
#'
#' set_color_scheme("blue")
#' mcmc_rhat_dots(runif(1000, 1, 1.3))
#' mcmc_rhat_dots(runif(1000, 1, 1.07))
#'
#'
#' # fake neff ratio values to use for demonstration
#' ratio <- c(runif(100, 0, 1))
#' mcmc_neff_hist(ratio)
#' mcmc_neff_dots(ratio)
#' mcmc_neff_dots(ratio) + move_legend("top")
#'
#'
#'
NULL

#' @rdname MCMC-diagnostics
#' @export
#' @param rhat Vector of \code{\link[=rhat]{Rhat}} estimates.
#' @template args-hist
#'
mcmc_rhat_hist <- function(rhat, ..., binwidth = NULL) {
  ggplot(
    data.frame(x = rhat, lev = factor_rhat(rhat)),
    aes_(
      x = ~ x,
      # y = ~ ..density..,
      color = ~ lev,
      fill = ~ lev
    )
  ) +
    geom_histogram(
      size = .25,
      na.rm = TRUE,
      binwidth = binwidth
    ) +
    scale_color_diagnostic("rhat") +
    scale_fill_diagnostic("rhat") +
    labs(x = bquote(hat(R)), y = NULL) +
    dont_expand_y_axis(c(0.005, 0)) +
    theme_default(y_text = FALSE)
}

#' @rdname MCMC-diagnostics
#' @export
#' @param size An optional value to override \code{\link[ggplot2]{geom_point}}'s
#'   default size.
#'
mcmc_rhat_dots <- function(rhat, ..., size = NULL) {
  stopifnot(length(rhat) > 1)

  .rhat_dots <- function(size = NULL) {
    args <- list(shape = 21, na.rm = TRUE)
    do.call("geom_point", c(args, size = size))
  }

  # factor rhat by parameter instead of value
  frhat <- if (!is.null(names(rhat))) {
    factor(rhat, labels = names(rhat))
  } else {
    factor(rhat)
  }

  data <- data.frame(y = rhat, x = frhat)
  graph <- ggplot(data, aes_(
    x = ~ x, y = ~ y,
    color = ~factor_rhat(rhat),
    fill = ~factor_rhat(rhat)
  )) +
    hline_at(
      c(1.05, 1.1),
      color = "gray",
      linetype = 2,
      size = 0.25
    ) +
    geom_segment(
      mapping = aes_(
        xend = ~x,
        yend = ifelse(min(rhat) < 1, 1, -Inf),
        color = ~factor_rhat(rhat)
      ),
      na.rm = TRUE
    )

  if (min(rhat) < 1)
    graph <- graph +
      geom_hline(yintercept = 1, color = "gray", size = 1)

  graph +
    .rhat_dots(size) +
    labs(x = NULL, y = bquote(hat(R))) +
    scale_fill_diagnostic("rhat") +
    scale_color_diagnostic("rhat") +
    theme_default(y_text = FALSE) +
    coord_flip() +
    scale_y_continuous(breaks = c(1, 1.05, 1.1),
                       expand = c(0,.001))
}


#' @rdname MCMC-diagnostics
#' @export
#' @param ratio Vector of ratios of effective sample size (estimates) to total
#'   sample size. See \code{\link{neff_ratio}}.
#'
mcmc_neff_hist <- function(ratio, ..., binwidth = NULL) {
  ggplot(
    data.frame(x = ratio, lev = factor_neff(ratio)),
    aes_(
      x = ~ x,
      color = ~ lev,
      fill = ~ lev
    )
  ) +
    geom_histogram(
      size = .25,
      na.rm = TRUE,
      binwidth = binwidth
    ) +
    scale_color_diagnostic("neff") +
    scale_fill_diagnostic("neff") +
    labs(x = bquote(N[eff]/N), y = NULL) +
    dont_expand_y_axis(c(0.005, 0)) +
    theme_default(y_text = FALSE)
}

#' @rdname MCMC-diagnostics
#' @export
mcmc_neff_dots <- function(ratio, ..., size = NULL) {
  stopifnot(length(ratio) > 1)

  .neff_dots <- function(size = NULL) {
    args <- list(shape = 21, na.rm = TRUE)
    do.call("geom_point", c(args, size = size))
  }

  # factor neff ratio by parameter instead of value
  fratio <- if (!is.null(names(ratio))) {
    factor(ratio, labels = names(ratio))
  } else {
    factor(ratio)
  }
  data <- data.frame(y = ratio, x = fratio)
  ggplot(data, aes_(
    x = ~ x,
    y = ~ y,
    color = ~factor_neff(ratio),
    fill = ~factor_neff(ratio)
  )) +
    hline_at(
      c(0.1, 0.5, 1),
      color = "gray",
      linetype = 2,
      size = 0.25
    ) +
    geom_segment(
      aes_(xend = ~x, yend = -Inf, color = ~factor_neff(ratio)),
      na.rm = TRUE
    ) +
    .neff_dots(size) +
    labs(x = NULL, y = bquote(N[eff]/N)) +
    scale_fill_diagnostic("neff") +
    scale_color_diagnostic("neff") +
    theme_default(y_text = FALSE) +
    coord_flip() +
    scale_y_continuous(breaks = c(0.1, seq(0, 1, .25)),
                       expand = c(0,.01))
}
