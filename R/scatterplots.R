#' Scatterplots
#'
#' Scatterplots of the observed data \eqn{y} vs. simulated/replicated data
#' \eqn{y^{rep}}{yrep} from the posterior predictive distribution. For
#' \code{ppc_scatter_average}, \eqn{y} is plotted against the average values of
#' \eqn{y^{rep}}{yrep}, i.e., the points \eqn{(y_n, \bar{y}^{rep}_n),\, n = 1,
#' \dots, N}{(y_n, mean(yrep_n)), n = 1,...,N}, where each
#' \eqn{y^{rep}_n}{yrep_n} is a vector of length equal to the number of
#' posterior draws. For \code{ppc_scatter_multiple}, each of the
#' \code{nrow(yrep)} simulated datasets is plotted separately against \eqn{y}
#' (and so \code{yrep} should only contain a small number of draws, i.e.,
#' \code{nrow(yrep)} should be small).
#'
#' @name scatterplots
#' @family PPCs
#'
#' @template args-ppc
#' @param ... Optional arguments to \code{\link[ggplot2]{geom_point}} to control
#'   the appearance of the plotted points.
#'
#' @template details-ppc
#' @template return-ggplot
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#'
#' @examples
#' y <- rnorm(100)
#' yrep <- matrix(rnorm(2500), ncol = 100)
#' (p1 <- ppc_scatter_average(y, yrep))
#' (p2 <- ppc_scatter_multiple(y, yrep[1:3, ]))
#'
#' lims <- ggplot2::lims(x = c(-3, 3), y = c(-3, 3))
#' p1 + lims
#' p2 + lims
#'
NULL

#' @export
#' @rdname scatterplots
ppc_scatter_average <- function(y, yrep, ...) {
  ppc_scatter(y, yrep, average = TRUE, ...)
}

#' @export
#' @rdname scatterplots
ppc_scatter_multiple <- function(y, yrep, ...) {
  ppc_scatter(y, yrep, average = FALSE, ...)
}



ppc_scatter <- function(y, yrep, average = TRUE, ...) {
  validate_y_and_yrep(y, yrep)

  scheme <- get_color_scheme()
  defaults <- list(
    shape = 21,
    fill = scheme[["mid"]],
    color = scheme[["mid_highlight"]],
    size = 2.5
  )
  geom_args <- set_geom_args(defaults, ...)
  if (average) {
    avg_yrep <- colMeans(yrep)
    base <- ggplot(
      data = data.frame(x = y, y = avg_yrep),
      mapping = aes_string("x", "y")
    )
  } else {
    base <- ggplot(
      data = data.frame(melt_yrep(yrep), y = rep(y, each = nrow(yrep))),
      mapping = aes_string(x = "y", y = "value")
    )
  }

  graph <- base +
    geom_abline(intercept = 0, slope = 1, linetype = 2) +
    call_geom("point", geom_args) +
    labs(x = "y", y = if (average) "Average yrep" else "yrep")

  if (!average)
    graph <- graph + facet_wrap("rep_id")

  graph + theme_ppc()
}
