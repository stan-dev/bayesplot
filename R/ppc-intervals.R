#' PPC intervals
#'
#' Medians and central interval estimates of \code{yrep} with \code{y} overlaid.
#' See the \strong{Plot Descriptions} section, below.
#'
#' @name PPC-intervals
#' @family PPCs
#'
#' @template args-y-yrep
#' @param x A numeric vector the same length as \code{y} to use as the x-axis
#'   variable. For example, \code{x} could be a predictor variable from a
#'   regression model, a time variable for time-series models, etc. If \code{x}
#'   is missing or NULL, then \code{1:length(y)} is used for the x-axis.
#' @param ... Currently unused.
#' @param prob A value between 0 and 1 indicating the desired probability mass
#'   to include in the inner \code{yrep} intervals. The default is 0.9.
#' @param prob_outer The probability mass to include in the outer \code{yrep}
#'   interval. The default is 1.0.
#' @param alpha,size,fatten Arguments passed to geoms. For ribbon plots
#'   \code{alpha} and \code{size} are passed to
#'   \code{\link[ggplot2]{geom_ribbon}}. For interval plots \code{size} and
#'   \code{fatten} are passed to \code{\link[ggplot2]{geom_pointrange}}.
#'
#' @template return-ggplot-or-data
#'
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{\code{ppc_intervals, ppc_ribbon}}{
#'    \code{100*prob}\% central intervals for \code{yrep} at each \code{x}
#'    value. \code{ppc_intervals} plots intervals as vertical bars with points
#'    indicating \code{yrep} medians and darker points indicating observed
#'    \code{y} values. \code{ppc_ribbon} plots a ribbon of connected intervals
#'    with a line through the median of \code{yrep} and a darker line connecting
#'    observed \code{y} values. In both cases an optional \code{x} variable can
#'    also be specified for the x-axis variable.
#'
#'    Depending on the number of observations and the variability in the
#'    predictions at different values of \code{x}, one or the other of these
#'    plots may be easier to read than the other.
#'   }
#'   \item{\code{ppc_intervals_grouped, ppc_ribbon_grouped}}{
#'    Same as \code{ppc_intervals} and \code{ppc_ribbon}, respectively, but a
#'    separate plot (facet) is generated for each level of a grouping variable.
#'   }
#' }
#'
#' @examples
#' y <- rnorm(50)
#' yrep <- matrix(rnorm(5000, 0, 2), ncol = 50)
#'
#' color_scheme_set("brightblue")
#' ppc_ribbon(y, yrep)
#' ppc_intervals(y, yrep)
#'
#' # change x axis to y values (instead of indices) and add x = y line
#' ppc_intervals(y, yrep, x = y) + abline_01()
#'
#'
#' color_scheme_set("teal")
#' year <- 1950:1999
#' ppc_ribbon(y, yrep, x = year, alpha = 0, size = 0.75) + ggplot2::xlab("Year")
#'
#' color_scheme_set("pink")
#' year <- rep(2000:2009, each = 5)
#' group <- gl(5, 1, length = 50, labels = LETTERS[1:5])
#' ppc_ribbon_grouped(y, yrep, x = year, group) +
#'   ggplot2::scale_x_continuous(breaks = pretty)
#'
#' ppc_ribbon_grouped(
#'  y, yrep, x = year, group,
#'  facet_args = list(scales = "fixed"),
#'  alpha = 1,
#'  size = 2
#' ) +
#'  xaxis_text(FALSE) +
#'  xaxis_ticks(FALSE) +
#'  panel_bg(fill = "gray20")
#'
#' ppc_dat <- ppc_intervals_data(y, yrep, x = year, prob = 0.5)
#' ppc_group_dat <- ppc_intervals_data(y, yrep, x = year, group = group, prob = 0.5)
#'
#' \dontrun{
#' library("rstanarm")
#' fit <- stan_glmer(mpg ~ wt + (1|cyl), data = mtcars)
#' yrep <- posterior_predict(fit)
#'
#' color_scheme_set("purple")
#' with(mtcars, ppc_intervals(mpg, yrep, x = wt, prob = 0.5)) +
#'  panel_bg(fill="gray90", color = NA) +
#'  grid_lines(color = "white")
#'
#' ppc_intervals_grouped(y = mtcars$mpg, yrep, prob = 0.8,
#'                       x = mtcars$wt, group = mtcars$cyl)
#'
#'
#' color_scheme_set("gray")
#' ppc_intervals(mtcars$mpg, yrep, prob = 0.5) +
#'  ggplot2::scale_x_continuous(
#'    labels = rownames(mtcars),
#'    breaks = 1:nrow(mtcars)
#'  ) +
#'  xaxis_text(angle = -70, vjust = 1, hjust = 0)
#'
#' }
#'
#'
NULL

#' @rdname PPC-intervals
#' @export
ppc_intervals <- function(y, yrep, x = NULL, ..., prob = 0.9, prob_outer = 1.0,
                          size = 1, fatten = 3) {
  check_ignored_arguments(...)

  data <- ppc_intervals_data(
      y = y,
      yrep = yrep,
      x = x,
      group = NULL,
      prob = prob,
      prob_outer = prob_outer
  )

  .ppc_intervals(
    data = data,
    size = size,
    fatten = fatten,
    grouped = FALSE,
    style = "intervals",
    x_lab = label_x(x)
  )
}

#' @rdname PPC-intervals
#' @export
#' @template args-group
#' @param facet_args An optional list of  arguments (other than \code{facets})
#'   passed to \code{\link[ggplot2]{facet_wrap}} to control faceting.
#'
ppc_intervals_grouped <- function(y,
                                  yrep,
                                  x = NULL,
                                  group,
                                  facet_args = list(),
                                  ...,
                                  prob = 0.9,
                                  prob_outer = 1.0,
                                  size = 1,
                                  fatten = 3) {
  check_ignored_arguments(...)

  if (is.null(facet_args[["scales"]])) {
    facet_args[["scales"]] <- "free"
  }

  data <- ppc_intervals_data(
    y = y,
    yrep = yrep,
    x = x,
    group = group,
    prob = prob,
    prob_outer = prob_outer
  )

  .ppc_intervals(
    data = data,
    facet_args = facet_args,
    size = size,
    fatten = fatten,
    grouped = TRUE,
    style = "intervals",
    x_lab = label_x(x)
  )
}


#' @rdname PPC-intervals
#' @export
ppc_ribbon <- function(y,
                       yrep,
                       x = NULL,
                       ...,
                       prob = 0.9,
                       prob_outer = 1.0,
                       alpha = 0.33,
                       size = 0.25) {
  check_ignored_arguments(...)

  data <- ppc_intervals_data(
    y = y,
    yrep = yrep,
    x = x,
    group = NULL,
    prob = prob
  )

  .ppc_intervals(
    data = data,
    alpha = alpha,
    size = size,
    grouped = FALSE,
    style = "ribbon",
    x_lab = label_x(x)
  )
}


#' @export
#' @rdname PPC-intervals
ppc_ribbon_grouped <- function(y,
                               yrep,
                               x = NULL,
                               group,
                               facet_args = list(),
                               ...,
                               prob = 0.9,
                               prob_outer = 1.0,
                               alpha = 0.33,
                               size = 0.25) {
  check_ignored_arguments(...)

  if (is.null(facet_args[["scales"]])) {
    facet_args[["scales"]] <- "free"
  }

  data <- ppc_intervals_data(
    y = y,
    yrep = yrep,
    x = x,
    group = group,
    prob = prob,
    prob_outer = prob_outer
  )

  .ppc_intervals(
    data = data,
    facet_args = facet_args,
    alpha = alpha,
    size = size,
    grouped = TRUE,
    style = "ribbon",
    x_lab = label_x(x)
  )
}


#' @rdname PPC-intervals
#' @export
ppc_intervals_data <- function(y, yrep, x = NULL, group = NULL,
                               prob = 0.9, prob_outer = 1.0, ...) {
  check_ignored_arguments(...)
  .ppc_intervals_data(y = y, yrep = yrep, x = x, group = group,
                      prob = prob, prob_outer = prob_outer)
}


#' @rdname PPC-intervals
#' @export
ppc_ribbon_data <- ppc_intervals_data




# internal ----------------------------------------------------------------
label_x <- function(x) {
  if (missing(x)) "Index" else NULL
}

.ppc_intervals_data <- function(y, yrep, x = NULL, group = NULL,
                                prob = 0.8, prob_outer = 1.0) {
  grouped <- !is.null(group)
  stopifnot(prob > 0 && prob < 1)
  stopifnot(prob_outer > 0 && prob_outer <= 1)
  probs <- sort(c(prob, prob_outer))
  prob <- probs[1]
  prob_outer <- probs[2]

  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  x <- validate_x(x, y)

  long_d <- melt_and_stack(y, yrep)
  long_d$x <- x[long_d$y_id]
  long_d$y_obs <- y[long_d$y_id]

  molten_reps <- long_d[!as.logical(long_d[["is_y"]]), , drop = FALSE]
  molten_reps$is_y <- NULL

  if (grouped) {
    group <- validate_group(group, y)
    molten_reps$group <- group[molten_reps$y_id]
    group_vars <- syms(c("y_id", "y_obs", "group", "x"))
  } else {
    group_vars <- syms(c("y_id", "y_obs", "x"))
  }

  grouped_d <- dplyr::group_by(molten_reps, !!! group_vars)
  alpha <- (1 - probs) / 2
  probs <- sort(c(alpha, 0.5, 1 - alpha))

  val_col <- sym("value")
  dplyr::ungroup(dplyr::summarise(
    grouped_d,
    prob = prob,
    ll = quantile(!! val_col, prob = probs[1]),
    lo = quantile(!! val_col, prob = probs[2]),
    mid = quantile(!! val_col, prob = probs[3]),
    hi = quantile(!! val_col, prob = probs[4]),
    hh = quantile(!! val_col, prob = probs[5])
  ))
}

# Make intervals or ribbon plot
#
# @param data The object returned by .ppc_intervals_data
# @return A ggplot object
#
.ppc_intervals <-
  function(data,
           facet_args = list(),
           alpha = 0.33,
           fatten = 3,
           size = 1,
           grouped = FALSE,
           style = c("intervals", "ribbon"),
           x_lab = NULL) {

  style <- match.arg(style)

  graph <- ggplot(
    data = data,
    mapping = aes_(
      x = ~ x,
      y = ~ mid,
      ymin = ~ lo,
      ymax = ~ hi
    )
  )

  if (style == "ribbon") {
    graph <- graph +
      geom_ribbon(
        aes_(color = "yrep", fill = "yrep", ymin = ~ ll, ymax = ~ hh),
        alpha = alpha,
        size = size
      ) +
      geom_ribbon(
        aes_(color = "yrep", fill = "yrep"),
        alpha = alpha,
        size = size
      ) +
      geom_line(
        aes_(color = "yrep"),
        size = size/2
      ) +
      geom_blank(aes_(fill = "y")) +
      geom_line(
        aes_(y = ~ y_obs, color = "y"),
        size = 0.5
      )
  } else {
    graph <- graph +
      geom_pointrange(
        mapping = aes_(color = "yrep", fill = "yrep", ymin = ~ ll, ymax = ~ hh),
        shape = 21,
        alpha = alpha,
        size = size,
        fatten = fatten
      ) +
      geom_pointrange(
        mapping = aes_(color = "yrep", fill = "yrep"),
        shape = 21,
        size = size,
        fatten = fatten
      ) +
      geom_point(
        mapping = aes_(y = ~ y_obs, color = "y", fill = "y"),
        shape = 21,
        size = 1.5
      )
  }

  graph <- graph +
    scale_color_manual(
      name = "",
      values = setNames(get_color(c("lh", "dh")), c("yrep", "y")),
      labels = c(yrep = yrep_label(), y = y_label())
    ) +
    scale_fill_manual(
      name = "",
      values = c(yrep = get_color("l"),
                 y = if (style == "ribbon") NA else get_color("d")),
      labels = c(yrep = yrep_label(), y = y_label())
    )


  if (grouped) {
    facet_args[["facets"]] <- "group"
    if (is.null(facet_args[["scales"]])) {
      facet_args[["scales"]] <- "free"
    }
    graph <- graph + do.call("facet_wrap", facet_args)
  }

  graph +
    labs(y = NULL, x = x_lab %||% expression(italic(x)))
}

