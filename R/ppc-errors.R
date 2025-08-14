#' PPC errors
#'
#' Various plots of predictive errors `y - yrep`. See the
#' **Details** and **Plot Descriptions** sections, below.
#'
#' @name PPC-errors
#' @family PPCs
#'
#' @template args-y-yrep
#' @template args-group
#' @template args-facet_args
#' @param x A numeric vector the same length as `y` to use as the x-axis variable.
#' @param ... Currently unused.
#' @param stat A function or a string naming a function for computing the
#' posterior average. In both cases, the function should take a vector input and
#' return a scalar statistic. The function name is displayed in the axis-label.
#' Defaults to `"mean"`.
#' @param size,alpha For scatterplots, arguments passed to
#'   [ggplot2::geom_point()] to control the appearance of the points. For the
#'   binned error plot, arguments controlling the size of the outline and
#'   opacity of the shaded region indicating the 2-SE bounds.
#'
#' @details
#' All of these functions (aside from the `*_scatter_avg` functions)
#' compute and plot predictive errors for each row of the matrix `yrep`, so
#' it is usually a good idea for `yrep` to contain only a small number of
#' draws (rows). See **Examples**, below.
#'
#' For binomial and Bernoulli data the `ppc_error_binned()` function can be used
#' to generate binned error plots. Bernoulli data can be input as a vector of 0s
#' and 1s, whereas for binomial data `y` and `yrep` should contain "success"
#' proportions (not counts). See the **Examples** section, below.
#'
#' @section Plot descriptions:
#' \describe{
#'   \item{`ppc_error_hist()`}{
#'    A separate histogram is plotted for the predictive errors computed from
#'    `y` and each dataset (row) in `yrep`. For this plot `yrep` should have
#'    only a small number of rows.
#'   }
#'   \item{`ppc_error_hist_grouped()`}{
#'    Like `ppc_error_hist()`, except errors are computed within levels of a
#'    grouping variable. The number of histograms is therefore equal to the
#'    product of the number of rows in `yrep` and the number of groups
#'    (unique values of `group`).
#'   }
#'   \item{`ppc_error_scatter()`}{
#'    A separate scatterplot is displayed for `y` vs. the predictive errors
#'    computed from `y` and each dataset (row) in `yrep`. For this plot `yrep`
#'    should have only a small number of rows.
#'   }
#'   \item{`ppc_error_scatter_avg()`}{
#'    A single scatterplot of `y` vs. the average of the errors computed from
#'    `y` and each dataset (row) in `yrep`. For each individual data point
#'    `y[n]` the average error is the average of the errors for `y[n]` computed
#'    over the the draws from the posterior predictive distribution.
#'
#'    When the optional `x` argument is provided, the average error is plotted
#'    on the y-axis and the predictor variable `x` is plotted on the x-axis.
#'   }
#'   \item{`ppc_error_scatter_avg_vs_x()`}{
#'    Deprecated. Use `ppc_error_scatter_avg(x = x)` instead.
#'   }
#'   \item{`ppc_error_binned()`}{
#'    Intended for use with binomial data. A separate binned error plot (similar
#'    to `arm::binnedplot()`) is generated for each dataset (row) in `yrep`. For
#'    this plot `y` and `yrep` should contain proportions rather than counts,
#'    and `yrep` should have only a small number of rows.
#'   }
#' }
#'
#' @template return-ggplot
#'
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#'
#' @examples
#' y <- example_y_data()
#' yrep <- example_yrep_draws()
#' ppc_error_hist(y, yrep[1:3, ])
#'
#' # errors within groups
#' group <- example_group_data()
#' (p1 <- ppc_error_hist_grouped(y, yrep[1:3, ], group))
#' p1 + yaxis_text() # defaults to showing counts on y-axis
#' \donttest{
#' table(group) # more obs in GroupB, can set freq=FALSE to show density on y-axis
#' (p2 <- ppc_error_hist_grouped(y, yrep[1:3, ], group, freq = FALSE))
#' p2 + yaxis_text()
#' }
#'
#' # scatterplots
#' ppc_error_scatter(y, yrep[10:14, ])
#' ppc_error_scatter_avg(y, yrep)
#'
#' x <- example_x_data()
#' ppc_error_scatter_avg(y, yrep, x)
#'
#' \dontrun{
#' # binned error plot with binomial model from rstanarm
#' library(rstanarm)
#' example("example_model", package = "rstanarm")
#' formula(example_model)
#'
#' # get observed proportion of "successes"
#' y <- example_model$y  # matrix of "success" and "failure" counts
#' trials <- rowSums(y)
#' y_prop <- y[, 1] / trials  # proportions
#'
#' # get predicted success proportions
#' yrep <- posterior_predict(example_model)
#' yrep_prop <- sweep(yrep, 2, trials, "/")
#'
#' ppc_error_binned(y_prop, yrep_prop[1:6, ])
#'
#' # plotting against a covariate on x-axis
#' herd <- as.numeric(example_model$data$herd)
#' ppc_error_binned(y_prop, yrep_prop[1:6, ], x = herd)
#' }
#'
NULL

#' @rdname PPC-errors
#' @export
#' @template args-hist
#' @template args-hist-freq
ppc_error_hist <-
  function(y,
           yrep,
           ...,
           facet_args = list(),
           binwidth = NULL,
           bins = NULL,
           breaks = NULL,
           freq = TRUE) {

    dots <- list(...)
    if (!from_grouped(dots)) {
      check_ignored_arguments(...)
      dots$group <- NULL
    }

    data <- ppc_error_data(y, yrep, group = dots$group)
    ggplot(data, set_hist_aes(freq)) +
      geom_histogram(
        fill = get_color("l"),
        color = get_color("lh"),
        linewidth = 0.25,
        binwidth = binwidth,
        bins = bins,
        breaks = breaks
      ) +
      xlab(error_label()) +
      bayesplot_theme_get() +
      dont_expand_y_axis() +
      error_hist_facets(
        facet_args,
        grouped = FALSE,
        ignore = nrow(yrep) == 1
      ) +
      force_axes_in_facets() +
      yaxis_title(FALSE) +
      yaxis_text(FALSE) +
      yaxis_ticks(FALSE) +
      facet_text(FALSE)
  }


#' @rdname PPC-errors
#' @export
ppc_error_hist_grouped <-
  function(y,
           yrep,
           group,
           ...,
           facet_args = list(),
           binwidth = NULL,
           bins = NULL,
           breaks = NULL,
           freq = TRUE) {

    check_ignored_arguments(...)
    call <- match.call(expand.dots = FALSE)
    g <- eval(ungroup_call("ppc_error_hist", call), parent.frame())
    g +
      error_hist_facets(facet_args, grouped = TRUE) +
      facet_text() +
      theme(strip.text.y = element_blank())
  }


#' @rdname PPC-errors
#' @export
ppc_error_scatter <-
  function(y,
           yrep,
           ...,
           facet_args = list(),
           size = 2.5,
           alpha = 0.8) {
    check_ignored_arguments(...)

    y <- validate_y(y)
    yrep <- validate_predictions(yrep, length(y))
    errors <- compute_errors(y, yrep)
    ppc_scatter(
      y = y,
      yrep = errors,
      facet_args = facet_args,
      size = size,
      alpha = alpha,
      ref_line = FALSE
    ) +
      labs(x = error_label(), y = y_label())
  }

#' @rdname PPC-errors
#' @export
ppc_error_scatter_avg <-
  function(y,
           yrep,
           x = NULL,
           ...,
           stat = "mean",
           size = 2.5,
           alpha = 0.8) {
    check_ignored_arguments(...)

    y <- validate_y(y)
    yrep <- validate_predictions(yrep, length(y))

    if (!missing(x)) {
      qx <- enquo(x)
      x <- validate_x(x, y)
    }
    errors <- compute_errors(y, yrep)

    stat <- as_tagged_function({{ stat }})

    ppc_scatter_avg(
      y = if (is_null(x)) y else x,
      yrep = errors,
      size = size,
      alpha = alpha,
      ref_line = FALSE,
      stat = stat
    ) +
      labs(
        x = error_avg_label(stat),
        y = if (is_null(x)) y_label() else as_label((qx))
        ) + if (is_null(x)) {
          NULL
        } else {
          coord_flip()
        }
  }


#' @rdname PPC-errors
#' @export
ppc_error_scatter_avg_grouped <-
  function(y,
           yrep,
           group,
           ...,
           stat = "mean",
           facet_args = list(),
           size = 2.5,
           alpha = 0.8) {
    check_ignored_arguments(...)

    y <- validate_y(y)
    yrep <- validate_predictions(yrep, length(y))
    stat <- as_tagged_function({{ stat }})

    errors <- compute_errors(y, yrep)
    ppc_scatter_avg_grouped(
      y = y,
      yrep = errors,
      group = group,
      size = size,
      alpha = alpha,
      facet_args = facet_args,
      ref_line = FALSE,
      stat = stat
    ) +
      labs(x = error_avg_label(stat), y = y_label())
  }


#' @rdname PPC-errors
#' @export
ppc_error_scatter_avg_vs_x <- function(
    y,
    yrep,
    x,
    ...,
    stat = "mean",
    size = 2.5,
    alpha = 0.8
) {
  check_ignored_arguments(...)

  .Deprecated(new = "ppc_error_scatter_avg(y, yrep, x)")

  y <- validate_y(y)
  yrep <- validate_predictions(yrep, length(y))
  qx <- enquo(x)
  x <- validate_x(x, y)
  stat <- as_tagged_function({{ stat }})
  errors <- compute_errors(y, yrep)
  ppc_scatter_avg(
    y = x,
    yrep = errors,
    size = size,
    alpha = alpha,
    ref_line = FALSE,
    stat = stat
  ) +
    labs(
      x = error_avg_label(stat),
      y = as_label((qx))
    ) +
    coord_flip()
}


#' @rdname PPC-errors
#' @export
#' @param bins For `ppc_error_binned()`, the number of bins to use (approximately).
ppc_error_binned <-
  function(y,
           yrep,
           x = NULL,
           ...,
           facet_args = list(),
           bins = NULL,
           size = 1,
           alpha = 0.25) {
    check_ignored_arguments(...)

    qx <- enquo(x)
    data <- ppc_error_binnned_data(y, yrep, x = x, bins = bins)
    facet_layer <- if (nrow(yrep) == 1) {
      geom_ignore()
    } else {
      facet_args[["facets"]] <- "rep_id"
      do.call("facet_wrap", facet_args)
    }

    mixed_scheme <- is_mixed_scheme(color_scheme_get())
    point_fill <- get_color(ifelse(mixed_scheme, "m", "d"))
    point_color <- get_color(ifelse(mixed_scheme, "mh", "dh"))

    ggplot(data, aes(x = .data$ey_bar)) +
      hline_0(linetype = 2, color = "black") +
      geom_ribbon(
        mapping = aes(ymax = .data$se2, ymin = -.data$se2),
        fill = get_color("l"),
        color = NA,
        alpha = alpha
      ) +
      geom_path(
        mapping = aes(y = .data$se2),
        color = get_color("l"),
        linewidth = size
      ) +
      geom_path(
        mapping = aes(y = -.data$se2),
        color = get_color("l"),
        linewidth = size
      ) +
      geom_point(
        mapping = aes(y = .data$err_bar),
        shape = 21,
        fill = point_fill,
        color = point_color
      ) +
      labs(
        x = if (is.null(x)) "Predicted proportion" else as_label((qx)),
        y = "Average Errors \n (with 2SE bounds)"
      ) +
      bayesplot_theme_get() +
      facet_layer +
      force_axes_in_facets() +
      facet_text(FALSE)
  }


#' @rdname PPC-errors
#' @export
ppc_error_data <- function(y, yrep, group = NULL) {
  y <- validate_y(y)
  yrep <- validate_predictions(yrep, length(y))
  if (!is.null(group)) {
    group <- validate_group(group, length(y))
  }
  errors <- compute_errors(y, yrep) %>% melt_predictions()
  errors <- tibble::add_column(errors, y_obs = y[errors$y_id], .before = "rep_id")
  if (!is.null(group)) {
    errors <- tibble::add_column(errors, group = group[errors$y_id], .before = "y_id")
  }
  errors
}


# internal ----------------------------------------------------------------

#' Compute predictive errors `y` - `yrep`
#' @noRd
#' @param y,yrep User's `y` and `yrep` arguments.
#' @return A matrix with the same dimensions as `yrep`
compute_errors <- function(y, yrep) {
  suggested_package("rstantools")
  rstantools::predictive_error(object = yrep, y = y)
}


#' Create facet layer for PPC error plots
#'
#' The default is to use `scales="fixed"` (which I think makes sense for looking
#' at errors, right?) if not specified in `facet_args`.
#'
#' @param User's `facet_args` argument.
#' @param grouped If `FALSE` then does faceting by `rep_id`, if `TRUE` then both
#'   `rep_id` and `group`.
#' @param ignore If `TRUE` then `geom_ignore()` is returned. This is intended to
#'   allow turning off facets if there is only one plot to make.
#' @param scales_default What to use for the `scales` argument to `facet_*()` if
#'   not specified in `facet_args`.
#' @return Object returned by `facet_wrap()` or `facet_grid()` (unless `ignore=TRUE`).
#' @noRd
error_hist_facets <-
  function(facet_args,
           grouped = FALSE,
           ignore = FALSE,
           scales_default = "fixed") {
    if (ignore) {
      return(geom_ignore())
    }

    if (grouped) {
      facet_fun <- "facet_grid"
      facet_args[["rows"]] <- vars(.data$rep_id)
      facet_args[["cols"]] <- vars(.data$group)
    } else {
      facet_fun <- "facet_wrap"
      facet_args[["facets"]] <- vars(.data$rep_id)
    }
    facet_args[["scales"]] <- facet_args[["scales"]] %||% scales_default

    do.call(facet_fun, facet_args)
  }


error_label <- function() {
  expression(italic(y) - italic(y)[rep])
}

error_avg_label <- function(stat = NULL) {
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

  expr(paste((!!de))*(italic(y) - italic(y)[rep]))
}


# Data for binned errors plots
ppc_error_binnned_data <- function(y, yrep, x = NULL, bins = NULL) {
  y <- validate_y(y)
  yrep <- validate_predictions(yrep, length(y))

  if (!is.null(x)) {
    x <- validate_x(x, y)
  }

  if (is.null(bins)) {
    bins <- n_bins(length(y))
  }

  errors <- compute_errors(y, yrep)
  binned_errs <- list()
  for (s in 1:nrow(errors)) {
    if (is.null(x)) {
      binned_errs[[s]] <-
        bin_errors(
          ey = yrep[s, ],
          r = errors[s, ],
          bins = bins,
          rep_id = s
        )
    } else {
      binned_errs[[s]] <-
        bin_errors(
          ey = x,
          r = errors[s, ],
          bins = bins,
          rep_id = s
        )
    }

  }

  binned_errs <- dplyr::bind_rows(binned_errs)
  tibble::as_tibble(binned_errs)
}

# calculate number of bins binned_error_data()
# @parmam N Number of data points, length(y)
n_bins <- function(N) {
  if (N <= 10) {
    return(floor(N / 2))
  } else if (N > 10 && N < 100) {
    return(10)
  } else { # N >= 100
    return(floor(sqrt(N)))
  }
}

bin_errors <- function(ey, r, bins, rep_id = NULL) {
  N <- length(ey)
  break_ids <- floor(N * (1:(bins - 1)) / bins)
  if (any(break_ids == 0)) {
    bins <- 1
  }
  if (bins == 1) {
    breaks <- c(-Inf, sum(range(ey)) / 2, Inf)
  } else {
    ey_sort <- sort(ey)
    breaks <- -Inf
    for (i in 1:(bins - 1)) {
      break_i <- break_ids[i]
      ey_range <- ey_sort[c(break_i, break_i + 1)]
      if (diff(ey_range) == 0) {
        if (ey_range[1] == min(ey)) {
          ey_range[1] <- -Inf
        } else {
          ey_range[1] <- max(ey[ey < ey_range[1]])
        }
      }
      breaks <- c(breaks, sum(ey_range) / 2)
    }
    breaks <- unique(c(breaks, Inf))
  }

  ey_binned <- as.numeric(cut(ey, breaks))
  bins <- length(breaks) - 1
  out <- matrix(NA, nrow = bins, ncol = 4)
  colnames(out) <- c("ey_bar", "err_bar", "se2", "bin")
  for (i in 1:bins) {
    mark <- which(ey_binned == i)
    ey_bar <- mean(ey[mark])
    r_bar <- mean(r[mark])
    s <- if (length(r[mark]) > 1) sd(r[mark]) else 0
    out[i, ] <- c(ey_bar, r_bar, 2 * s / sqrt(length(mark)), i)
  }
  out <- as.data.frame(out)
  if (!is.null(rep_id)) {
    out$rep_id <- as.integer(rep_id)
  }
  return(out)
}

