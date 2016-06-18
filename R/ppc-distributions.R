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
#'    variable is plotted as a violin with 10\%, 50\%, and 90\% quantiles
#'    indicated by horizontal lines. The points in \code{y} corresponding to
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
    scale_fill_manual(values = get_color(c("dark", "light"))) +
    scale_color_manual(values = get_color(c("dark_highlight", "light_highlight"))) +
    facet_wrap_parsed("rep_id", switch = "x") +
    dont_expand_y_axis() +
    theme_ppc(y_text = FALSE, x_lab = FALSE)
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
    scale_fill_manual(values = get_color(c("dark", "light"))) +
    scale_color_manual(values = get_color(c("dark_highlight", "light_highlight"))) +
    facet_wrap_parsed("rep_id", switch = "x") +
    dont_expand_y_axis() +
    theme_ppc(y_text = FALSE, x_lab = FALSE)
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
    scale_color_manual(values = get_color(c("light", "dark_highlight"))) +
    scale_fill_manual(values = c(NA, get_color("dark"))) +
    scale_size_manual(values = c(0.25, 1)) +
    xlab(y_label()) +
    dont_expand_axes() +
    theme_ppc(y_text = FALSE)
}

#' @export
#' @rdname PPC-distributions
#' @template args-group
#'
ppc_violin_grouped <- function(y, yrep, group, ...) {
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
      fill = get_color("light"),
      color = get_color("light_highlight"),
      draw_quantiles = c(0.1, 0.5, 0.9)
    ) +
    geom_point(
      data = plot_data[is_y,, drop = FALSE],
      color = get_color("dark_highlight"),
      shape = 21
    ) +
    scale_fill_manual(
      name = "",
      values = get_color("dark"),
      labels = expression(italic(y))
    ) +
    labs(x = "Group", y = yrep_label()) +
    theme_ppc(legend_position = "right")
}
