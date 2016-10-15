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
#' @param size,alpha Passed to the appropriate geom to control the appearance of
#'   the \code{yrep} distributions.
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
#'    instead of histograms. These plots often look like wizard hats.
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
#' color_scheme_set("brightblue")
#' y <- example_y_data()
#' yrep <- example_yrep_draws()
#' dim(yrep)
#' ppc_dens_overlay(y, yrep[1:50, ])
#' ppc_ecdf_overlay(y, yrep[sample(nrow(yrep), 25), ])
#'
#' # for ppc_hist, definitely use a subset yrep rows so only
#' # a few (instead of nrow(yrep)) histograms are plotted
#' ppc_hist(y, yrep[1:8, ])
#'
#' # wizard hat plot
#' color_scheme_set("blue")
#' ppc_dens(y, yrep[200:202, ])
#'
#' color_scheme_set("gray")
#' group <- example_group_data()
#' ppc_violin_grouped(y, yrep, group, size = 1.5)
#' ppc_violin_grouped(y, yrep, group, alpha = 0)
#'
NULL

#' @rdname PPC-distributions
#' @export
ppc_hist <- function(y, yrep, ..., binwidth = NULL, freq = TRUE) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)

  ggplot(melt_and_stack(y, yrep),
         set_hist_aes(freq, fill = ~ is_y, color = ~ is_y)) +
    geom_histogram(size = 0.25, binwidth = binwidth) +
    scale_fill_manual(
      name = "",
      values = get_color(c("d", "l")),
      labels = c(expression(italic(y)), expression(italic(y)[rep]))
    ) +
    scale_color_manual(
      name = "",
      values = get_color(c("dh", "lh")),
      labels = c(expression(italic(y)), expression(italic(y)[rep]))
    ) +
    facet_wrap_parsed("rep_id") +
    force_axes_in_facets() +
    dont_expand_y_axis() +
    theme_default() +
    space_legend_keys() +
    yaxis_text(FALSE) +
    yaxis_title(FALSE) +
    yaxis_ticks(FALSE) +
    xaxis_title(FALSE) +
    facet_text(FALSE) +
    facet_bg(FALSE)
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
    geom_density(size = 0.5, trim = trim) +
    scale_fill_manual(
      name = "",
      values = get_color(c("d", "l")),
      labels = c(expression(italic(y)), expression(italic(y)[rep]))
    ) +
    scale_color_manual(
      name = "",
      values = get_color(c("dh", "lh")),
      labels = c(expression(italic(y)), expression(italic(y)[rep]))
    ) +
    facet_wrap_parsed("rep_id") +
    force_axes_in_facets() +
    dont_expand_y_axis() +
    theme_default() +
    space_legend_keys() +
    yaxis_text(FALSE) +
    yaxis_title(FALSE) +
    yaxis_ticks(FALSE) +
    xaxis_title(FALSE) +
    facet_text(FALSE) +
    facet_bg(FALSE)
}

#' @rdname PPC-distributions
#' @export
ppc_dens_overlay <- function(y, yrep, ...,
                             size = 0.25, alpha = 0.7,
                             trim = FALSE) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)

  ggplot(melt_yrep(yrep), aes_(x = ~ value)) +
    stat_density(
      aes_(group = ~ rep_id, color = "yrep"),
      geom = "line",
      position = "identity",
      size = size,
      alpha = alpha,
      trim = trim
    ) +
    stat_density(
      data = data.frame(value = y),
      aes_(color = "y"),
      geom = "line",
      position = "identity",
      size = 1,
      trim = trim
    ) +
    scale_color_manual(
      name = "",
      values = setNames(get_color(c("dh", "l")), c("y", "yrep")),
      labels = c(expression(italic(y)), expression(italic(y)[rep]))
    ) +
    xlab(y_label()) +
    dont_expand_axes() +
    theme_default() +
    yaxis_title(FALSE) +
    xaxis_title(FALSE) +
    yaxis_text(FALSE) +
    yaxis_ticks(FALSE)
}

#' @export
#' @rdname PPC-distributions
#' @param pad For \code{ppc_ecdf_overlay}, a logical scalar passed to
#'   \code{\link[ggplot2]{stat_ecdf}}.
ppc_ecdf_overlay <- function(y, yrep, ...,
                             size = 0.25, alpha = 0.7,
                             pad = TRUE) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)

  ggplot(melt_yrep(yrep), aes_(x = ~ value)) +
    hline_at(c(0, 0.5, 1), size = c(0.2, 0.1, 0.2),
             linetype = 2, color = get_color("dh")) +
    stat_ecdf(
      mapping = aes_(group = ~ rep_id, color = "yrep"),
      geom = "line",
      size = size,
      alpha = alpha,
      pad = pad
    ) +
    stat_ecdf(
      data = data.frame(value = y),
      mapping = aes_(color = "y"),
      geom = c("line"),
      size = 1,
      pad = pad
    ) +
    scale_color_manual(
      name = "",
      values = setNames(get_color(c("dh", "l")), c("y", "yrep")),
      labels = c(expression(italic(y)), expression(italic(y)[rep]))
    ) +
    xlab(y_label()) +
    scale_y_continuous(breaks = c(0, 0.5, 1)) +
    theme_default() +
    yaxis_title(FALSE) +
    xaxis_title(FALSE) +
    yaxis_ticks(FALSE)
}

#' @export
#' @rdname PPC-distributions
#' @template args-group
#' @param probs A numeric vector passed to \code{\link[ggplot2]{geom_violin}}'s
#'   \code{draw_quantiles} argument to specify at which quantiles to draw
#'   horizontal lines. Set to \code{NULL} to remove the lines.
#'
ppc_violin_grouped <- function(y, yrep, group, ...,
                               probs = c(0.1, 0.5, 0.9),
                               alpha = 1,
                               size = 1) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  group <- validate_group(group, y)
  plot_data <- ppc_group_data(y, yrep, group, stat = NULL)
  is_y <- plot_data$variable == "y"

  ggplot(
    plot_data[!is_y,, drop = FALSE],
    aes_(x = ~ group, y = ~ value)
  ) +
    geom_violin(
      aes_(fill = "yrep", color = "yrep"),
      draw_quantiles = probs,
      alpha = alpha
    ) +
    geom_point(
      data = plot_data[is_y,, drop = FALSE],
      aes_(fill = "y", color = "y"),
      shape = 21,
      alpha = 0.9,
      size = size
    ) +
    scale_fill_manual(
      name = "",
      values = setNames(c(NA, get_color(c("l"))), c("y", "yrep")),
      labels = c(expression(italic(y)), expression(italic(y)[rep]))
    ) +
    scale_color_manual(
      name = "",
      values = setNames(get_color(c("dh", "lh")), c("y", "yrep")),
      labels = c(expression(italic(y)), expression(italic(y)[rep]))
    ) +
    labs(x = "Group", y = yrep_label()) +
    theme_default() +
    yaxis_title(FALSE) +
    xaxis_title(FALSE)
}
