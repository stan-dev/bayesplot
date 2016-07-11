#' PPC distributions
#'
#' Compare the empirical distribution of the data \code{y} to the distributions
#' of simulated/replicated data \code{yrep} from the posterior predictive
#' distribution.
#'
#' @name PPC-distributions
#' @family PPCs
#'
#' @template args-y-yrep
#' @template args-hist
#' @param ... Currently unused.
#'
#' @template details-binomial
#' @template return-ggplot
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{\code{ppc_dens_overlay}}{
#'    The kernel density estimate for \code{y} is plotted with the area under
#'    the curve shaded. The density estimates of each dataset (row) in
#'    \code{yrep} are then overlaid but unshaded.
#'   }
#'   \item{\code{ppc_hist}}{
#'    A separate histogram is plotted for \code{y} and each dataset (row) in
#'    \code{yrep}. For this plot \code{yrep} should therefore contain only a
#'    small number of rows.
#'   }
#'   \item{\code{ppc_dens}}{
#'    The same as \code{ppc_hist} but kernel density estimates are plotted
#'    instead of histograms.
#'   }
#'   \item{\code{ppc_violin_grouped}}{
#'    The density estimate of \code{yrep} within each level of a grouping
#'    variable is plotted as a violin with horizontal lines at
#'    notable quantiles. The points in \code{y} corresponding to
#'    each grouping level are then overlaid on top of the violins.
#'   }
#' }
#'
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#' @template seealso-color-scheme
#'
#' @examples
#' y <- rnorm(100)
#' yrep <- matrix(rnorm(2500), ncol = 100)
#' ppc_dens_overlay(y, yrep)
#' ppc_hist(y, yrep[1:8, ])
#'
#' group <- gl(4, 25, labels = LETTERS[1:4])
#' ppc_violin_grouped(y, yrep, group)
#'
NULL

#' @export
#' @rdname PPC-distributions
#'
ppc_hist <- function(y, yrep, ..., binwidth = NULL) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)

  ggplot(
    data = melt_and_stack(y, yrep),
    mapping = aes_(
      x = ~ value,
      y = ~ ..density..,
      fill = ~ is_y,
      color = ~ is_y
    )
  ) +
    geom_histogram(size = 0.25, binwidth = binwidth) +
    scale_fill_manual(values = get_color(c("d", "l"))) +
    scale_color_manual(values = get_color(c("dh", "lh"))) +
    facet_wrap_parsed("rep_id", switch = "x") +
    dont_expand_y_axis() +
    theme_default(y_text = FALSE, x_lab = FALSE)
}


#' @export
#' @rdname PPC-distributions
#'
ppc_dens <- function(y, yrep, ...) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)

  ggplot(
    data = melt_and_stack(y, yrep),
    mapping = aes_(
      x = ~ value,
      fill = ~ is_y,
      color = ~ is_y
    )
  ) +
    geom_density(size = 1) +
    scale_fill_manual(values = get_color(c("d", "l"))) +
    scale_color_manual(values = get_color(c("dh", "lh"))) +
    facet_wrap_parsed("rep_id", switch = "x") +
    dont_expand_y_axis() +
    theme_default(y_text = FALSE, x_lab = FALSE)
}

#' @export
#' @rdname PPC-distributions
#'
ppc_dens_overlay <- function(y, yrep, ...) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)

  ggplot(
    data = melt_and_stack(y, yrep),
    mapping = aes_(
      x = ~ value,
      group = ~ rep_id,
      color = ~ is_y,
      fill = ~ is_y,
      size = ~ is_y
    )
  ) +
    geom_density() +
    scale_color_manual(values = get_color(c("l", "dh"))) +
    scale_fill_manual(values = c(NA, get_color("d"))) +
    scale_size_manual(values = c(0.25, 1)) +
    xlab(y_label()) +
    dont_expand_axes() +
    theme_default(y_text = FALSE)
}

#' @export
#' @rdname PPC-distributions
#' @template args-group
#' @param probs A numeric vector passed to \code{\link[ggplot2]{geom_violin}}'s
#'   \code{draw_quantiles} argument to specify at which quantiles to draw
#'   horizontal lines. Set to \code{NULL} to remove the lines.
#'
ppc_violin_grouped <- function(y, yrep, group, ..., probs = c(0.1, 0.5, 0.9)) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  group <- validate_group(group, y)
  plot_data <- ppc_group_data(y, yrep, group, stat = NULL)
  is_y <- plot_data$variable == "y"

  ggplot(
    data = plot_data[!is_y,, drop = FALSE],
    mapping = aes_(
      x = ~ group,
      y = ~ value,
      fill = ~ variable
    )
  ) +
    geom_violin(
      fill = get_color("l"),
      color = get_color("lh"),
      draw_quantiles = probs
    ) +
    geom_point(
      data = plot_data[is_y,, drop = FALSE],
      color = get_color("dh"),
      shape = 21
    ) +
    scale_fill_manual(
      name = "",
      values = get_color("d"),
      labels = expression(italic(y))
    ) +
    labs(x = "Group", y = yrep_label()) +
    theme_default(legend_position = "right")
}
