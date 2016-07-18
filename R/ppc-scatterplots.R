#' PPC scatterplots
#'
#' Scatterplots of the observed data \code{y} vs. simulated/replicated data
#' \code{yrep} from the posterior predictive distribution.
#'
#' @name PPC-scatterplots
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
#' @section Plot Descriptions:
#' \describe{
#'   \item{\code{ppc_scatter}}{
#'    For each dataset (row) in \code{yrep} a scatterplot is generated showing
#'    \code{y} against that row of \code{yrep}. For this plot \code{yrep} should
#'    only contain a small number of rows.
#'   }
#'   \item{\code{ppc_scatter_avg}}{
#'    A scatterplot of \code{y} against the average values of \code{yrep}, i.e.,
#'    the points \code{(y[n], mean(yrep[, n]))}, where each \code{yrep[, n]} is
#'    a vector of length equal to the number of posterior draws.
#'   }
#'   \item{\code{ppc_scatter_avg_grouped}}{
#'    The same as \code{ppc_scatter_avg}, but a separate plot is generated for
#'    each level of a grouping variable.
#'   }
#' }
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
#' @rdname PPC-scatterplots
#'
ppc_scatter <- function(y, yrep, ...) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)

  graph <- ppc_scatter_plotter(
    data = data.frame(melt_yrep(yrep), y = rep(y, each = nrow(yrep))),
    mapping = aes_(x = ~ y, y = ~ value),
    x_lab = y_label(),
    y_lab = yrep_label()
  )
  graph + facet_wrap_parsed("rep_id")
}

#' @export
#' @rdname PPC-scatterplots
#'
ppc_scatter_avg <- function(y, yrep, ...) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)

  ppc_scatter_plotter(
    data = data.frame(y, avg_y_rep = colMeans(yrep)),
    mapping = aes_(x = ~ y, y = ~ avg_y_rep),
    x_lab = y_label(),
    y_lab = yrep_avg_label()
  )
}

#' @export
#' @rdname PPC-scatterplots
#' @template args-group
#'
ppc_scatter_avg_grouped <- function(y, yrep, group, ...) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  group <- validate_group(group, y)

  graph <- ppc_scatter_plotter(
    data = data.frame(group, y, avg_yrep = colMeans(yrep)),
    mapping = aes_(x = ~ y, y = ~ avg_yrep),
    x_lab = y_label(),
    y_lab = yrep_avg_label()
  )
  graph + facet_wrap("group", scales = "free")
}


# helpers -----------------------------------------------------------------

ppc_scatter_plotter <-
  function(data,
           mapping,
           x_lab = "",
           y_lab = "") {

    ggplot(data, mapping) +
      geom_abline(
        intercept = 0,
        slope = 1,
        linetype = 2
      ) +
      geom_point(
        shape = 21,
        fill = get_color("m"),
        color = get_color("mh"),
        size = 2.5
      ) +
      labs(x = x_lab, y = y_lab) +
      theme_default()
  }
