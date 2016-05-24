#' Scatterplots
#'
#' Scatterplots of the observed data \eqn{y} vs. simulated/replicated data
#' \eqn{y^{rep}}{yrep} from the posterior predictive distribution. For
#' \code{ppc_scatter_avg}, \eqn{y} is plotted against the average values of
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
#' @template args-y-yrep
#' @param ... Currently unused.
#'
#' @template details-binomial
#' @template return-ggplot
#'
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#' @template seealso-color-scheme
#'
#' @examples
#' y <- rnorm(100)
#' yrep <- matrix(rnorm(2500), ncol = 100)
#' (p1 <- ppc_scatter_avg(y, yrep))
#' (p2 <- ppc_scatter(y, yrep[1:3, ]))
#'
#' lims <- ggplot2::lims(x = c(-3, 3), y = c(-3, 3))
#' p1 + lims
#' p2 + lims
#'
#' group <- gl(5, 20, labels = month.abb[1:5])
#' ppc_scatter_avg_grouped(y, yrep, group)
#'
NULL

#' @export
#' @rdname scatterplots
ppc_scatter <- function(y, yrep, ...) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  scheme <- get_color_scheme()
  ggplot(
    data = data.frame(melt_yrep(yrep), y = rep(y, each = nrow(yrep))),
    mapping = aes_string(x = "y", y = "value")
  ) +
    .ppc_scatter_abline() +
    .ppc_scatter_points(scheme) +
    labs(x = y_label(), y = yrep_label()) +
    facet_wrap("rep_id", labeller = label_parsed) +
    theme_ppc()
}

#' @export
#' @rdname scatterplots
ppc_scatter_avg <- function(y, yrep, ...) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  scheme <- get_color_scheme()
  ggplot(
    data = data.frame(y, avg_y_rep = colMeans(yrep)),
    mapping = aes_string(x = "y", y = "avg_y_rep")
  ) +
    .ppc_scatter_abline() +
    .ppc_scatter_points(scheme) +
    labs(x = y_label(), y = yrep_avg_label()) +
    theme_ppc()
}

#' @export
#' @rdname scatterplots
#' @template args-group
#'
ppc_scatter_avg_grouped <- function(y, yrep, group, ...) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  group <- validate_group(group, y)
  scheme <- get_color_scheme()
  ggplot(
    data = data.frame(group, y, avg_yrep = colMeans(yrep)),
    mapping = aes_string(x = "y", y = "avg_yrep")
  ) +
    .ppc_scatter_abline() +
    .ppc_scatter_points(scheme) +
    labs(x = y_label(), y = yrep_avg_label()) +
    facet_wrap("group", scales = "free", labeller = label_both) +
    theme_ppc()
}


# helpers -----------------------------------------------------------------
.ppc_scatter_abline <- function() {
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = 2
  )
}

.ppc_scatter_points <- function(scheme, value = c("mid", "light", "dark")) {
  value <- match.arg(value)
  geom_point(
    shape = 21,
    fill = scheme[[value]],
    color = scheme[[paste0(value, "_highlight")]],
    size = 2.5
  )
}
