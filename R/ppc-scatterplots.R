#' PPC scatterplots
#'
#' Scatterplots of the observed data \code{y} vs. simulated/replicated data
#' \code{yrep} from the posterior predictive distribution. See the \strong{Plot
#' Descriptions} and \strong{Details} sections, below.
#'
#' @name PPC-scatterplots
#' @family PPCs
#'
#' @template args-y-yrep
#' @param ... Currently unused.
#' @param size,alpha Arguments passed to \code{\link[ggplot2]{geom_point}} to
#'   control the appearance of the points.
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
#' y <- example_y_data()
#' yrep <- example_yrep_draws()
#' (p1 <- ppc_scatter_avg(y, yrep))
#' (p2 <- ppc_scatter(y, yrep[20:23, ], alpha = 0.5, size = 1.5))
#'
#' lims <- ggplot2::lims(x = c(0, 160), y = c(0, 160))
#' p1 + lims
#' p2 + lims
#'
#' group <- example_group_data()
#' ppc_scatter_avg_grouped(y, yrep, group, alpha = 0.7) + lims
#'
NULL

#' @export
#' @rdname PPC-scatterplots
#'
ppc_scatter <- function(y, yrep, ..., size = 1.5, alpha = 0.8) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)

  graph <- ppc_scatter_plotter(
    data = data.frame(melt_yrep(yrep), y = rep(y, each = nrow(yrep))),
    mapping = aes_(x = ~ value, y = ~ y),
    y_lab = y_label(),
    x_lab = yrep_label(),
    alpha = alpha,
    size = size
  )
  graph + facet_wrap_parsed("rep_id")
}

#' @export
#' @rdname PPC-scatterplots
#'
ppc_scatter_avg <- function(y, yrep, ..., size = 2.5, alpha = 0.8) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)

  ppc_scatter_plotter(
    data = data.frame(y, avg_y_rep = colMeans(yrep)),
    mapping = aes_(x = ~ avg_y_rep, y = ~ y),
    y_lab = y_label(),
    x_lab = yrep_avg_label(),
    alpha = alpha,
    size = size
  )
}

#' @export
#' @rdname PPC-scatterplots
#' @template args-group
#'
ppc_scatter_avg_grouped <- function(y, yrep, group, ...,
                                    size = 2.5, alpha = 0.8) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  group <- validate_group(group, y)

  graph <- ppc_scatter_plotter(
    data = data.frame(group, y, avg_yrep = colMeans(yrep)),
    mapping = aes_(x = ~ avg_yrep, y = ~ y),
    y_lab = y_label(),
    x_lab = yrep_avg_label(),
    alpha = alpha,
    size = size
  )
  graph + facet_wrap("group", scales = "free")
}


# helpers -----------------------------------------------------------------

ppc_scatter_plotter <-
  function(data,
           mapping,
           x_lab = "",
           y_lab = "",
           color = c("mid", "light"),
           size = 2.5,
           alpha = 1) {

    mid <- isTRUE(match.arg(color) == "mid")
    ggplot(data, mapping) +
      geom_abline(
        intercept = 0,
        slope = 1,
        linetype = 2,
        color = get_color("dh")
      ) +
      geom_point(
        shape = 21,
        fill = get_color(ifelse(mid, "m", "l")),
        color = get_color(ifelse(mid, "mh", "lh")),
        size = size,
        alpha = alpha
      ) +
      labs(x = x_lab, y = y_lab) +
      theme_default()
  }
