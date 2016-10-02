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
#' @template args-dens
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
#'   \item{\code{ppc_ecdf_overlay}}{
#'    Empirical CDF estimates of each dataset (row) in \code{yrep} are overlaid,
#'    with the distribution of \code{y} itself on top (and in a darker shade).
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
#'
#' @examples
#' y <- example_y_data()
#' yrep <- example_yrep_draws()
#' dim(yrep)
#' ppc_dens_overlay(y, yrep[1:40, ])
#' ppc_ecdf_overlay(y, yrep[sample(nrow(yrep), 50), ])
#'
#' # for ppc_hist, definitely use a subset yrep rows so only
#' # a few (instead of nrow(yrep)) histograms are plotted
#' ppc_hist(y, yrep[1:8, ])
#'
#' set_color_scheme("blue")
#' group <- example_group_data()
#' (p <- ppc_violin_grouped(y, yrep, group))
#' p +
#'  yaxis_ticks(size = .75) +  # add tickmarks to y-axis
#'  xaxis_text(size = 15) +    # make x-axis labels bigger
#'  xaxis_title(on = FALSE)    # remove x-axis title
#'
NULL

#' @rdname PPC-distributions
#' @export
ppc_hist <- function(y, yrep, ..., binwidth = NULL) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)

  mixed_scheme <- is_mixed_scheme(get_color_scheme())
  hist_fills <- get_color(c(ifelse(mixed_scheme, "m", "d"), "l"))
  hist_colors <- get_color(c(ifelse(mixed_scheme, "mh", "dh"), "lh"))

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
    scale_fill_manual(values = hist_fills) +
    scale_color_manual(values = hist_colors) +
    facet_wrap_parsed("rep_id") +
    dont_expand_y_axis() +
    theme_default(y_text = FALSE, x_lab = FALSE)
}


#' @rdname PPC-distributions
#' @export
ppc_dens <- function(y, yrep, ..., trim = FALSE) {
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
    geom_density(size = 1, trim = trim) +
    scale_fill_manual(values = get_color(c("d", "l"))) +
    scale_color_manual(values = get_color(c("dh", "lh"))) +
    facet_wrap_parsed("rep_id") +
    dont_expand_y_axis() +
    theme_default(y_text = FALSE, x_lab = FALSE)
}

#' @rdname PPC-distributions
#' @export
#' @param size,alpha For \code{ppc_dens_overlay} and \code{ppc_ecdf_overlay},
#'   passed to \code{\link[ggplot2]{stat_density}} or
#'   \code{\link[ggplot2]{stat_ecdf}} to control the appearance of the
#'   \code{yrep} distributions.
ppc_dens_overlay <- function(y, yrep, ...,
                             size = 0.25, alpha = 0.7,
                             trim = FALSE) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)

  ggplot(melt_yrep(yrep), aes_(x = ~ value)) +
    stat_density(
      aes_(group = ~ rep_id),
      geom = "line",
      position = "identity",
      color = get_color("l"),
      size = size,
      alpha = alpha,
      trim = trim
    ) +
    stat_density(
      data = data.frame(value = y),
      geom = "line",
      position = "identity",
      color = get_color("dh"),
      size = 1,
      trim = trim
    ) +
    xlab(y_label()) +
    dont_expand_axes() +
    theme_default(y_text = FALSE)
}

#' @export
#' @rdname PPC-distributions
#' @param pad For \code{ppc_ecdf_overlay}, a logical scalar passed to
#'   \code{\link[ggplot2]{stat_ecdf}}.
ppc_ecdf_overlay <- function(y, yrep, ..., size = 0.25, alpha = 0.7, pad = TRUE) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)

  ggplot(melt_yrep(yrep), aes_(x = ~ value)) +
    hline_at(c(0, 0.5, 1), size = c(0.2, 0.1, 0.2),
             linetype = 2, color = get_color("dh")) +
    stat_ecdf(
      aes_(group = ~ rep_id),
      geom = "line",
      color = get_color("l"),
      size = size,
      alpha = alpha,
      pad = pad
    ) +
    stat_ecdf(
      data = data.frame(value = y),
      geom = c("line"),
      color = get_color("dh"),
      size = 1,
      pad = pad
    ) +
    xlab(y_label()) +
    scale_y_continuous(breaks = c(0, 0.5, 1)) +
    theme_default(y_lab = FALSE)
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
