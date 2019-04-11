#' PPC errors
#'
#' Various plots of predictive errors \code{y} - \code{yrep}. See the
#' \strong{Details} and \strong{Plot Descriptions} sections, below.
#'
#' @name PPC-errors
#' @family PPCs
#'
#' @template args-y-yrep
#' @param ... Currently unused.
#' @param size,alpha For scatterplots, arguments passed to
#'   \code{\link[ggplot2]{geom_point}} to control the appearance of the
#'   points. For the binned error plot, arguments controlling the size of
#'   the outline and opacity of the shaded region indicating the 2-SE bounds.
#'
#' @details
#' All of these functions (aside from the \code{*_scatter_avg} functions)
#' compute and plot predictive errors for each row of the matrix \code{yrep}, so
#' it is usually a good idea for \code{yrep} to contain only a small number of
#' draws (rows). See \strong{Examples}, below.
#'
#' For binomial and Bernoulli data the \code{ppc_error_binned} function can be
#' used to generate binned error plots. Bernoulli data can be input as a vector
#' of 0s and 1s, whereas for binomial data \code{y} and \code{yrep} should
#' contain "success" proportions (not counts). See the \strong{Examples}
#' section, below.
#'
#' @section Plot descriptions:
#' \describe{
#'   \item{\code{ppc_error_hist}}{
#'    A separate histogram is plotted for the predictive errors computed from
#'    \code{y} and each dataset (row) in \code{yrep}. For this plot \code{yrep}
#'    should have only a small number of rows.
#'   }
#'   \item{\code{ppc_error_hist_grouped}}{
#'    Like \code{ppc_error_hist}, except errors are computed within levels of a
#'    grouping variable. The number of histograms is therefore equal to the
#'    product of the number of rows in \code{yrep} and the number of groups
#'    (unique values of \code{group}).
#'   }
#'   \item{\code{ppc_error_scatter}}{
#'    A separate scatterplot is displayed for \code{y} vs. the predictive errors
#'    computed from \code{y} and each dataset (row) in \code{yrep}. For this
#'    plot \code{yrep} should have only a small number of rows.
#'   }
#'   \item{\code{ppc_error_scatter_avg}}{
#'    A single scatterplot of \code{y} vs. the average of the errors computed
#'    from \code{y} and each dataset (row) in \code{yrep}. For each individual
#'    data point \code{y[n]} the average error is the average of the
#'    errors for \code{y[n]} computed over the the draws from the posterior
#'    predictive distribution.
#'   }
#'   \item{\code{ppc_error_scatter_avg_vs_x}}{
#'    Same as \code{ppc_error_scatter_avg}, except the average is plotted on the
#'    \eqn{y}-axis and a a predictor variable \code{x} is plotted on the
#'    \eqn{x}-axis.
#'   }
#'   \item{\code{ppc_error_binned}}{
#'    Intended for use with binomial data. A separate binned error plot (similar
#'    to \code{\link[arm]{binnedplot}}) is generated for each dataset (row) in
#'    \code{yrep}. For this plot \code{y} and \code{yrep} should contain
#'    proportions rather than counts, and \code{yrep} should have only a small
#'    number of rows.
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
#' ppc_error_scatter_avg_vs_x(y, yrep, x)
#'
#' # ppc_error_binned with binomial model from rstanarm
#' \dontrun{
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
#' }
#'
NULL

#' @rdname PPC-errors
#' @export
#' @template args-hist
#' @template args-hist-freq
#'
ppc_error_hist <-
  function(y,
           yrep,
           ...,
           binwidth = NULL,
           breaks = NULL,
           freq = TRUE) {
    check_ignored_arguments(...)

    y <- validate_y(y)
    yrep <- validate_yrep(yrep, y)

    if (nrow(yrep) == 1) {
      errors <- data.frame(value = y - as.vector(yrep))
      graph <- ggplot(errors, set_hist_aes(freq))
    } else {
      errors <- compute_errors(y, yrep)
      graph <-
        ggplot(melt_yrep(errors), set_hist_aes(freq)) +
        labs(y = NULL, x = expression(italic(y) - italic(y)[rep])) +
        facet_wrap(facets = ~ rep_id)
    }

    graph +
      geom_histogram(
        fill = get_color("l"),
        color = get_color("lh"),
        size = 0.25,
        binwidth = binwidth,
        breaks = breaks
      ) +
      bayesplot_theme_get() +
      xlab(expression(italic(y) - italic(y)[rep])) +
      dont_expand_y_axis() +
      force_axes_in_facets() +
      yaxis_title(FALSE) +
      yaxis_text(FALSE) +
      yaxis_ticks(FALSE) +
      facet_text(FALSE) +
      facet_bg(FALSE)
  }


#' @rdname PPC-errors
#' @export
#' @template args-group
#'
ppc_error_hist_grouped <-
  function(y,
           yrep,
           group,
           ...,
           binwidth = NULL,
           breaks = NULL,
           freq = TRUE) {
    check_ignored_arguments(...)

    y <- validate_y(y)
    yrep <- validate_yrep(yrep, y)
    group <- validate_group(group, y)
    errors <- grouped_error_data(y, yrep, group)

    ggplot(errors, set_hist_aes(freq)) +
      geom_histogram(
        fill = get_color("l"),
        color = get_color("lh"),
        size = 0.25,
        binwidth = binwidth,
        breaks = breaks
      ) +
      facet_grid(rep_id ~ group, scales = "free") +
      bayesplot_theme_get() +
      xlab(expression(italic(y) - italic(y)[rep])) +
      dont_expand_y_axis(c(0.005, 0)) +
      force_axes_in_facets() +
      yaxis_text(FALSE) +
      yaxis_ticks(FALSE) +
      yaxis_title(FALSE) +
      facet_bg(FALSE) +
      theme(strip.text.y = element_blank())
  }


#' @rdname PPC-errors
#' @export
ppc_error_scatter <-
  function(y,
           yrep,
           ...,
           size = 2.5,
           alpha = 0.8) {
    check_ignored_arguments(...)

    y <- validate_y(y)
    yrep <- validate_yrep(yrep, y)

    if (nrow(yrep) == 1) {
      return(
        .ppc_scatter(
          data = data.frame(y = y, x = y - as.vector(yrep)),
          mapping = aes_(x = ~ x, y = ~ y),
          x_lab = expression(italic(y) - italic(y)[rep]),
          y_lab = expression(italic(y)),
          size = size,
          alpha = alpha,
          abline = FALSE
        )
      )
    }

    errors <- compute_errors(y, yrep)
    .ppc_scatter(
      data = dplyr::left_join(
        melt_yrep(errors),
        data.frame(y = y, y_id = seq_along(y)),
        by = "y_id"
      ),
      mapping = aes_(x = ~ value, y = ~ y),
      y_lab = expression(italic(y)),
      x_lab = expression(italic(y) - italic(y)[rep]),
      size = size,
      alpha = alpha,
      abline = FALSE
    ) +
      facet_wrap(
        facets = ~ rep_id
        # labeller = label_bquote(italic(y) - italic(y)[rep](.(rep_id)))
      ) +
      force_axes_in_facets() +
      facet_text(FALSE) +
      facet_bg(FALSE)
  }

#' @rdname PPC-errors
#' @export
ppc_error_scatter_avg <-
  function(y,
           yrep,
           ...,
           size = 2.5,
           alpha = 0.8) {
    check_ignored_arguments(...)

    y <- validate_y(y)
    yrep <- validate_yrep(yrep, y)

    if (nrow(yrep) == 1)
      return(
        ppc_error_scatter(y, yrep,
                          size = size,
                          alpha = alpha, ...)
      )

    .ppc_scatter(
      data = data.frame(y, avg_error = y - colMeans(yrep)),
      mapping = aes_(x = ~ avg_error, y = ~ y),
      y_lab = y_label(),
      x_lab = "Average predictive error",
      alpha = alpha,
      size = size,
      abline = FALSE
    )
  }

#' @rdname PPC-errors
#' @export
#' @param x A numeric vector the same length as \code{y} to use as the x-axis
#'   variable.
#'
ppc_error_scatter_avg_vs_x <-
  function(y,
           yrep,
           x,
           ...,
           size = 2.5,
           alpha = 0.8) {
    check_ignored_arguments(...)

    y <- validate_y(y)
    yrep <- validate_yrep(yrep, y)
    x <- validate_x(x, y)
    .ppc_scatter(
      data = data.frame(x, avg_error = y - colMeans(yrep)),
      mapping = aes_(x = ~ x, y = ~ avg_error),
      x_lab = expression(italic(x)),
      y_lab = "Average predictive error",
      alpha = alpha,
      size = size,
      abline = FALSE
    )
  }


#' @rdname PPC-errors
#' @export
ppc_error_binned <- function(y, yrep, ..., size = 1, alpha = 0.25) {
  suggested_package("arm")
  check_ignored_arguments(...)

  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  errors <- compute_errors(y, yrep)

  N <- length(y)
  if (N >= 100) {
    nbins <- floor(sqrt(N))
  } else if (N > 10 && N < 100) {
    nbins <- 10
  } else {
    # if (N <= 10)
    nbins <- floor(N / 2)
  }

  S <- nrow(yrep)
  binned <- bin_errors(rep_id = 1, ey = yrep[1, ], r = errors[1, ],
                       nbins = nbins)
  if (S > 1) {
    for (i in 2:nrow(errors)) {
      binned_i <- bin_errors(rep_id = i, ey = yrep[i,], r = errors[i,],
                             nbins = nbins)
      binned <- rbind(binned, binned_i)
    }
  }

  mixed_scheme <- is_mixed_scheme(color_scheme_get())
  point_fill <- get_color(ifelse(mixed_scheme, "m", "d"))
  point_color <- get_color(ifelse(mixed_scheme, "mh", "dh"))
  graph <-
    ggplot(binned, aes_(x = ~ xbar)) +
    geom_hline(
      yintercept = 0,
      linetype = 2,
      color = "black"
    ) +
    geom_ribbon(
      aes_(ymax = ~ se2, ymin = ~ -se2),
      fill = get_color("l"),
      color = NA,
      alpha = alpha
    ) +
    geom_path(
      mapping = aes_(y = ~ se2),
      color = get_color("l"),
      size = size
    ) +
    geom_path(
      mapping = aes_(y = ~ -se2),
      color = get_color("l"),
      size = size
    ) +
    geom_point(
      mapping = aes_(y = ~ ybar),
      shape = 21,
      fill = point_fill,
      color = point_color
    ) +
    labs(
      x = "Predicted proportion",
      y = "Average Errors \n (with 2SE bounds)"
    ) +
    bayesplot_theme_get()

  if (S > 1) {
    graph <- graph +
      facet_wrap(
        facets = ~rep_id
        # labeller = label_bquote(italic(y)[rep](.(rep_id)))
      )
  }

  graph +
    force_axes_in_facets() +
    facet_text(FALSE) +
    facet_bg(FALSE)
}


# internal ----------------------------------------------------------------
compute_errors <- function(y, yrep) {
  errs <- sweep(yrep, MARGIN = 2L, STATS = as.array(y), FUN = "-")
  as.matrix(-1 * errs)
}

grouped_error_data <- function(y, yrep, group) {
  grps <- unique(group)
  errs <- list()
  for (j in seq_along(grps)) {
    g_j <- grps[j]
    err_j <- compute_errors(y[group == g_j], yrep[, group == g_j, drop=FALSE])
    errs[[j]] <- melt_yrep(err_j)
    errs[[j]]$group <- g_j
  }
  dat <- dplyr::bind_rows(errs)
  dat$y_id <- NULL
  dat
}


bin_errors <- function(rep_id, ey, r, nbins) {
  N <- length(ey)
  break_ids <- floor(N * (1:(nbins - 1)) / nbins)
  if (any(break_ids == 0)) {
    nbins <- 1
  }
  if (nbins == 1) {
    breaks <- c(-Inf, sum(range(ey)) / 2, Inf)
  } else {
    ey_sort <- sort(ey)
    breaks <- -Inf
    for (i in 1:(nbins - 1)) {
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

  nbins <- length(breaks) - 1
  ey_binned <- as.numeric(cut(ey, breaks))

  out <- matrix(NA, nrow = nbins, ncol = 3)
  for (i in 1:nbins) {
    mark <- which(ey_binned == i)
    ey_bar <- mean(ey[mark])
    r_bar <- mean(r[mark])
    s <- if (length(r[mark]) > 1) sd(r[mark]) else 0
    out[i, ] <- c(ey_bar, r_bar, 2 * s / sqrt(length(mark)))
  }
  out <- as.data.frame(out)
  colnames(out) <- c("xbar", "ybar", "se2")
  out$rep_id <- as.integer(rep_id)
  return(out)
}



