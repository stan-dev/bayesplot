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
#' @param ... Currently unused.
#'
#' @template details-ppc
#' @template return-ggplot
#'
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#' @template seealso-color-scheme
#'
#' @examples
#' y <- rnorm(100)
#' yrep <- matrix(rnorm(2500), ncol = 100)
#' (p1 <- ppc_scatter_average(y, yrep))
#' (p2 <- ppc_scatter(y, yrep[1:3, ]))
#'
#' lims <- ggplot2::lims(x = c(-3, 3), y = c(-3, 3))
#' p1 + lims
#' p2 + lims
#'
NULL

#' @export
#' @rdname scatterplots
ppc_scatter_average <- function(y, yrep, ...) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  ppc_scatterplot(y, yrep, average = TRUE, ...)
}

#' @export
#' @rdname scatterplots
ppc_scatter <- function(y, yrep, ...) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  ppc_scatterplot(y, yrep, average = FALSE, ...)
}

# @param y, yrep Already validated y (numeric vector) and yrep (numeric matrix)
#   objects.
# @param average Plot y vs avg yrep? If FALSE y is plotted again each row of
#   yrep separately.
ppc_scatterplot <- function(y, yrep, average = TRUE, ...) {
  scheme <- get_color_scheme()

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
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = 2
    ) +
    geom_point(
      shape = 21,
      fill = scheme[["mid"]],
      color = scheme[["mid_highlight"]],
      size = 2.5
    ) +
    labs(
      x = "y",
      y = if (average) "Average yrep" else "yrep"
    )

  if (!average)
    graph <- graph + facet_wrap("rep_id")

  graph + theme_ppc()
}
