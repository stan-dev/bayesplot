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
#' @param stat A function or a string naming a function for computing the
#'   posterior average. In both cases, the function should take a vector input
#'   and return a scalar statistic. The function name is displayed in the
#'   axis-label, and the underlying `$rep_label` for `ppc_scatter_avg_data()`
#'   includes the function name. Defaults to `"mean"`.
#' @param size,alpha Arguments passed to [ggplot2::geom_point()] to control the
#'   appearance of the points.
#' @param ref_line If `TRUE` (the default) a dashed line with intercept 0 and
#'   slope 1 is drawn behind the scatter plot.
#'
#' @template details-binomial
#' @template return-ggplot-or-data
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
#'    the points `(x,y) = (average(yrep[, n]), y[n])`, where each `yrep[, n]` is
#'    a vector of length equal to the number of posterior draws and `average()`
#'    is a summary statistic. Unlike for `ppc_scatter()`, for
#'    `ppc_scatter_avg()` `yrep` should contain many draws (rows).
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
#' # don't draw line x=y
#' ppc_scatter_avg(y, yrep, ref_line = FALSE)
#'
#' p2 <- ppc_scatter(y, yrep[20:23, ], alpha = 0.5, size = 1.5)
#' p2
#'
#' # give x and y axes the same limits
#' lims <- ggplot2::lims(x = c(0, 160), y = c(0, 160))
#' p1 + lims
#' p2 + lims
#'
#' # "average" function is customizable
#' ppc_scatter_avg(y, yrep, stat = "median", ref_line = FALSE)
#'
#' # for ppc_scatter_avg_grouped the default is to allow the facets
#' # to have different x and y axes
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
           alpha = 0.8,
           ref_line = TRUE) {
    check_ignored_arguments(...)

    data <- ppc_scatter_data(y, yrep)
    if (nrow(yrep) == 1) {
      facet_layer <- geom_ignore()
    } else {
      facet_args[["facets"]] <- "rep_label"
      facet_layer <- do.call("facet_wrap_parsed", facet_args)
    }

    ggplot(data, scatter_aes(color = "yrep", fill = "yrep")) +
      scatter_ref_line(ref_line) +
      geom_point(
        size = size,
        alpha = alpha,
        shape = 21,
        show.legend = FALSE
      ) +
      # use ppd color scale since only need one color
      # (and legend is off so no label modification needed)
      scale_color_ppd() +
      scale_fill_ppd() +
      bayesplot_theme_get() +
      facet_layer +
      labs(x = yrep_label(), y = y_label()) +
      force_axes_in_facets() +
      facet_text(FALSE) +
      legend_none()
  }


#' @rdname PPC-scatterplots
#' @export
ppc_scatter_avg <-
  function(y,
           yrep,
           ...,
           stat = "mean",
           size = 2.5,
           alpha = 0.8,
           ref_line = TRUE) {
    dots <- list(...)
    stat <- as_tagged_function({{ stat }})

    if (!from_grouped(dots)) {
      check_ignored_arguments(...)
      dots$group <- NULL
    }

    data <- ppc_scatter_avg_data(y, yrep, group = dots$group, stat = stat)
    if (is.null(dots$group) && nrow(yrep) == 1) {
      inform(
        "With only 1 row in 'yrep' ppc_scatter_avg is the same as ppc_scatter."
      )
    }

    ggplot(data, scatter_aes(color = "yrep", fill = "yrep")) +
      scatter_ref_line(ref_line) +
      geom_point(
        alpha = alpha,
        size = size,
        shape = 21,
        show.legend = FALSE
      ) +
      # ppd instead of ppc (see comment in ppc_scatter)
      scale_color_ppd() +
      scale_fill_ppd() +
      labs(x = yrep_avg_label(stat), y = y_label()) +
      bayesplot_theme_get()
  }


#' @rdname PPC-scatterplots
#' @export
ppc_scatter_avg_grouped <-
  function(y,
           yrep,
           group,
           ...,
           stat = "mean",
           facet_args = list(),
           size = 2.5,
           alpha = 0.8,
           ref_line = TRUE) {
    check_ignored_arguments(...)
    call <- match.call(expand.dots = FALSE)
    g <- eval(ungroup_call("ppc_scatter_avg", call), parent.frame())
    g +
      scatter_avg_group_facets(facet_args) +
      force_axes_in_facets()
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
ppc_scatter_avg_data <- function(y, yrep, group = NULL, stat = "mean") {
  y <- validate_y(y)
  yrep <- validate_predictions(yrep, length(y))
  if (!is.null(group)) {
    group <- validate_group(group, length(y))
  }
  stat <- as_tagged_function({{ stat }})

  data <- ppc_scatter_data(y = y, yrep = t(apply(yrep, 2, FUN = stat)))
  data$rep_id <- NA_integer_
  levels(data$rep_label) <- yrep_avg_label(stat) |>
    as.expression() |>
    as.character()

  if (!is.null(group)) {
    data <- tibble::add_column(data,
      group = group[data$y_id],
      .before = "y_id"
    )
  }

  data
}

# internal ----------------------------------------------------------------

yrep_avg_label <- function(stat = NULL) {
  stat <- as_tagged_function({{ stat }}, fallback = "stat")
  e <- attr(stat, "tagged_expr")
  if (attr(stat, "is_anonymous_function")) {
    e <- sym("stat")
  }
  de <- deparse1(e)

  # create some dummy variables to pass the R package check for
  # global variables in the expression below
  italic <- sym("italic")
  y <- sym("y")

  expr(paste((!!de))*(italic(y)[rep]))
}

scatter_aes <- function(...) {
  aes(x = .data$value, y = .data$y_obs, ...)
}

scatter_avg_group_facets <- function(facet_args) {
  facet_args[["facets"]] <- "group"
  facet_args[["scales"]] <- facet_args[["scales"]] %||% "free"
  do.call("facet_wrap", facet_args)
}

scatter_ref_line <-
  function(ref_line,
           linetype = 2,
           color = get_color("dh"),
           ...) {
    if (!ref_line) {
      return(geom_ignore())
    }
    abline_01(linetype = 2, color = get_color("dh"), ...)
  }
