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
#' @template args-group
#' @template args-facet_args
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
#'    A single scatterplot of `y` against the average values of `yrep`, i.e.,
#'    the points `(x,y) = (mean(yrep[, n]), y[n])`, where each `yrep[, n]` is
#'    a vector of length equal to the number of posterior draws. Unlike
#'    for `ppc_scatter()`, for `ppc_scatter_avg()` `yrep` should contain many
#'    draws (rows).
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
#'
#' p2 <- ppc_scatter(y, yrep[20:23, ], alpha = 0.5, size = 1.5)
#' p2
#'
#' # give x and y axes the same limits
#' lims <- ggplot2::lims(x = c(0, 160), y = c(0, 160))
#' p1 + lims
#' p2 + lims
#'
#' group <- example_group_data()
#' ppc_scatter_avg_grouped(y, yrep, group)
#'
#' # let x-axis vary but force y-axis to be the same
#' ppc_scatter_avg_grouped(y, yrep, group, facet_args = list(scales = "free_x"))
#'
NULL

#' @rdname PPC-scatterplots
#' @export
ppc_scatter <-
  function(y,
           yrep,
           ...,
           facet_args = list(),
           size = 2.5,
           alpha = 0.8) {
    check_ignored_arguments(...)

    if (nrow(yrep) == 1) {
      facet_layer <- geom_ignore()
    } else {
      facet_args[["facets"]] <- "rep_label"
      facet_layer <- do.call("facet_wrap_parsed", facet_args)
    }

    data <- ppc_scatter_data(y, yrep)
    ggplot(data, aes_(x = ~ value, y = ~ y_obs)) +
      geom_abline(
        intercept = 0,
        slope = 1,
        linetype = 2,
        color = get_color("dh")
      ) +
      geom_point(
        color = get_color("mh"),
        fill = get_color("m"),
        shape = 21,
        size = size,
        alpha = alpha
      ) +
      facet_layer +
      bayesplot_theme_get() +
      labs(x = yrep_label(), y = y_label()) +
      force_axes_in_facets() +
      facet_text(FALSE)
  }


#' @rdname PPC-scatterplots
#' @export
ppc_scatter_avg <-
  function(y,
           yrep,
           ...,
           size = 2.5,
           alpha = 0.8) {
    dots <- list(...)
    if (!from_grouped(dots)) {
      check_ignored_arguments(...)
      dots$group <- NULL
    }

    data <- ppc_scatter_avg_data(y, yrep, group = dots$group)
    if (is.null(dots$group) &&
        dplyr::n_distinct(data$rep_label) == 1) {
      inform("With only 1 row in 'yrep' ppc_scatter_avg is the same as ppc_scatter.")
    }

    ggplot(data, mapping = aes_(
      x = ~ value,
      y = ~ y_obs
    )) +
      geom_point(
        color = get_color("mh"),
        fill = get_color("m"),
        shape = 21,
        alpha = alpha,
        size = size
      ) +
      labs(
        x = yrep_avg_label(),
        y = y_label()
      ) +
      bayesplot_theme_get()
  }


#' @rdname PPC-scatterplots
#' @export
ppc_scatter_avg_grouped <-
  function(y,
           yrep,
           group,
           ...,
           facet_args = list(),
           size = 2.5,
           alpha = 0.8) {
    check_ignored_arguments(...)
    call <- match.call(expand.dots = FALSE)
    g <- eval(ungroup_call("ppc_scatter_avg", call), parent.frame())
    g + scatter_avg_group_facets(facet_args)
  }


#' @rdname PPC-scatterplots
#' @export
ppc_scatter_data <- function(y, yrep) {
  y <- validate_y(y)
  yrep <- validate_predictions(yrep, length(y))
  melt_predictions(yrep) %>%
    dplyr::arrange(.data$y_id) %>%
    tibble::add_column(
      y_obs = rep(y, each = nrow(yrep)),
      .before = "rep_id"
    )
}


#' @rdname PPC-scatterplots
#' @export
ppc_scatter_avg_data <- function(y, yrep, group = NULL) {
  y <- validate_y(y)
  yrep <- validate_predictions(yrep, length(y))
  if (!is.null(group)) {
    group <- validate_group(group, length(y))
  }

  data <- ppc_scatter_data(y = y, yrep = t(colMeans(yrep)))
  data$rep_id <- NA
  levels(data$rep_label) <- "mean(italic(y)[rep]))"

  if (!is.null(group)) {
    data <- tibble::add_column(data,
      group = group[data$y_id],
      .before = "y_id"
    )
  }

  data
}



# internal ----------------------------------------------------------------
scatter_avg_group_facets <- function(facet_args) {
  facet_args[["facets"]] <- "group"
  facet_args[["scales"]] <- facet_args[["scales"]] %||% "free"
  do.call("facet_wrap", facet_args)
}


