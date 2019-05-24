#' PPC scatterplots
#'
#' Scatterplots of the observed data `y` vs. simulated/replicated data
#' `yrep` from the posterior predictive distribution. See the
#' **Plot Descriptions** and **Details** sections, below.
#'
#' @name PPC-scatterplots
#' @family PPCs
#'
#' @template args-y-yrep
#' @param ... Currently unused.
#' @param size,alpha Arguments passed to [ggplot2::geom_point()] to control the
#'   appearance of the points.
#'
#' @template details-binomial
#' @template return-ggplot
#'
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{`ppc_scatter()`}{
#'    For each dataset (row) in `yrep` a scatterplot is generated showing `y`
#'    against that row of `yrep`. For this plot `yrep` should only contain a
#'    small number of rows.
#'   }
#'   \item{`ppc_scatter_avg()`}{
#'    A scatterplot of `y` against the average values of `yrep`, i.e.,
#'    the points `(mean(yrep[, n]), y[n])`, where each `yrep[, n]` is
#'    a vector of length equal to the number of posterior draws.
#'   }
#'   \item{`ppc_scatter_avg_grouped()`}{
#'    The same as `ppc_scatter_avg()`, but a separate plot is generated for
#'    each level of a grouping variable.
#'   }
#' }
#'
#' @examples
#' y <- example_y_data()
#' yrep <- example_yrep_draws()
#' p1 <- ppc_scatter_avg(y, yrep)
#' p1
#' p2 <- ppc_scatter(y, yrep[20:23, ], alpha = 0.5, size = 1.5)
#' p2
#'
#' # give x and y axes the same limits
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
ppc_scatter <-
  function(y,
           yrep,
           ...,
           size = 2.5,
           alpha = 0.8) {
    check_ignored_arguments(...)

    y <- validate_y(y)
    yrep <- validate_predictions(yrep, length(y))
    graph <- .ppc_scatter(
      data = data.frame(
        melt_predictions(yrep),
        y = rep(y, each = nrow(yrep))
      ),
      mapping = aes_(x = ~ value, y = ~ y),
      y_lab = y_label(),
      x_lab = yrep_label(),
      alpha = alpha,
      size = size
    )
    if (nrow(yrep) == 1)
      return(graph)

    graph +
      facet_wrap_parsed("rep_id") +
      force_axes_in_facets() +
      facet_text(FALSE) +
      facet_bg(FALSE)
  }

#' @export
#' @rdname PPC-scatterplots
#'
ppc_scatter_avg <-
  function(y,
           yrep,
           ...,
           size = 2.5,
           alpha = 0.8) {
    check_ignored_arguments(...)

    y <- validate_y(y)
    yrep <- validate_predictions(yrep, length(y))
    if (nrow(yrep) == 1)
      return(ppc_scatter(y, yrep, size = size, alpha = alpha, ...))

    .ppc_scatter(
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
ppc_scatter_avg_grouped <-
  function(y,
           yrep,
           group,
           ...,
           size = 2.5,
           alpha = 0.8) {
    check_ignored_arguments(...)

    y <- validate_y(y)
    yrep <- validate_predictions(yrep, length(y))
    ggplot(
      data = data.frame(
        y = y,
        avg_yrep = colMeans(yrep),
        group = validate_group(group, length(y))
      ),
      mapping = aes_(x = ~ avg_yrep, y = ~ y, group = ~ group)
    ) +
      geom_point(
        shape = 21,
        fill = get_color("m"),
        color = get_color("mh"),
        alpha = alpha,
        size = size
      ) +
      labs(
        y = y_label(),
        x = yrep_avg_label()
      ) +
      facet_wrap("group", scales = "free") +
      bayesplot_theme_get()
  }


# internal -----------------------------------------------------------------
.ppc_scatter <-
  function(data,
           mapping,
           x_lab = "",
           y_lab = "",
           color = c("mid", "light"),
           size = 2.5,
           alpha = 1,
           abline = TRUE) {
    mid <- isTRUE(match.arg(color) == "mid")
    graph <- ggplot(data, mapping)
    if (abline) {
      graph <- graph +
        geom_abline(
          intercept = 0,
          slope = 1,
          linetype = 2,
          color = get_color("dh")
        )
    }
    graph +
      geom_point(
        shape = 21,
        fill = get_color(ifelse(mid, "m", "l")),
        color = get_color(ifelse(mid, "mh", "lh")),
        size = size,
        alpha = alpha
      ) +
      labs(x = x_lab, y = y_lab) +
      bayesplot_theme_get()
  }
