#' Scatterplots
#'
#' Scatterplots of the observed data \eqn{y} vs. simulated/replicated data
#' \eqn{y^{rep}}{yrep} from the posterior predictive distribution.
#'
#' @export
#' @family PPCs
#'
#' @template args-ppc
#' @param average If \code{TRUE} (the default) then \eqn{y} is plotted against
#'   the average values of \eqn{y^{rep}}{yrep}, i.e., the points \eqn{(y_n,
#'   \bar{y}^{rep}_n),\, n = 1, \dots, N}{(y_n, mean(yrep_n)), n = 1,...,N},
#'   where each \eqn{y^{rep}_n}{yrep_n} is a vector of length equal to the
#'   number of posterior draws. If \code{FALSE} then each of the
#'   \code{nrow(yrep)} simulated datasets is plotted separately against \eqn{y}.
#'   Thus, if \code{average} is \code{FALSE}, \code{yrep} should only contain a
#'   small number of draws (\code{nrow(yrep)} should be small).
#' @param ... Optional arguments to \code{\link[ggplot2]{geom_point}} to control
#'   the appearance of the plotted points.
#'
#' @template details-ppc
#' @template return-ggplot
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#'
ppc_scatter <- function(y, yrep, average = TRUE, ...) {
  stopifnot(is.vector(y), is.matrix(yrep))
  if (ncol(yrep) != length(y))
    stop("ncol(yrep) should be equal to length(y).")

  defaults <- list(
    shape = 21,
    fill = .PP_FILL,
    color = "black",
    size = 2.5,
    alpha = 1
  )
  geom_args <- set_geom_args(defaults, ...)
  if (average) {
    avg_yrep <- colMeans(yrep)
    dat <- data.frame(x = y, y = avg_yrep, z = abs(y - avg_yrep))
    graph <- ggplot(dat, aes_string("x", "y")) +
      geom_abline(intercept = 0, slope = 1, linetype = 2) +
      call_geom("point", geom_args) +
      labs(x = "y", y = "Average yrep")
  } else {
    dat <- data.frame(
      melt_yrep(yrep),
      y = rep(y, each = nrow(yrep))
    )
    graph <- ggplot(dat, aes_string(x = "y", y = "value")) +
      geom_abline(intercept = 0, slope = 1, linetype = 2) +
      call_geom("point", geom_args) +
      labs(x = "y", y = "yrep") +
      facet_wrap("rep_id", scales = "free")
  }

  graph + pp_check_theme(no_y = FALSE)
}
