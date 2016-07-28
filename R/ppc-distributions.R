#' PPC distributions
#'
#' Compare the empirical distribution of the data \code{y} to the distributions
#' of simulated/replicated data \code{yrep} from the posterior predictive
#' distribution. See the \strong{Plot Descriptions} section, below,
#' for details.
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
#'    Kernel density estimates of each dataset (row) in \code{yrep} are
#'    overlaid, with the distribution of \code{y} itself on top (and in a darker
#'    shade).
#'   }
#'   \item{\code{ppc_hist}}{
#'    A separate histogram is plotted for \code{y} and each dataset (row) in
#'    \code{yrep}. For this plot \code{yrep} should therefore contain only a
#'    small number of rows. See the \strong{Examples} section.
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
#' yrep <- matrix(rnorm(5000), ncol = 100)
#' dim(yrep)
#' ppc_dens_overlay(y, yrep[1:30, ])
#'
#' # for ppc_hist, definitely subset yrep so only some instead of
#' # nrow(yrep) histograms are plotted
#' ppc_hist(y, yrep[1:8, ])
#'
#' set_color_scheme("blue")
#' group <- gl(4, 25, labels = LETTERS[1:4])
#' (p <- ppc_violin_grouped(y, yrep, group))
#' p +
#'  yaxis_ticks(size = .75) +  # add tickmarks to y-axis
#'  xaxis_text(size = 15) +    # make x-axis labels bigger
#'  xaxis_title(on = FALSE)    # remove x-axis title
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
    data = melt_yrep(yrep),
    mapping = aes_(
      x = ~ value,
      group = ~ rep_id
    )
  ) +
    geom_density(
      color = get_color("l"),
      fill = NA,
      size = 0.25,
      alpha = 0.1
    ) +
    geom_density(
      data = data.frame(y = y),
      mapping = aes_(x = ~ y),
      inherit.aes = FALSE,
      color = get_color("d"),
      fill = NA,
      size = 1
    ) +
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
