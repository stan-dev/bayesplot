#' Distributions
#'
#' Compare the empirical distribution of the data \code{y} to the distributions
#' of simulated/replicated data \code{yrep} from the posterior predictive
#' distribution.
#'
#' @name distributions
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
#'
#'    \if{html}{\figure{densOverlay.png}{options: width="30\%" alt="Figure: densOverlay.png"}}
#'    \if{latex}{\figure{densOverlay.png}{options: width=4cm}}
#'   }
#'   \item{\code{ppc_hist}}{
#'    A separate histogram is plotted for \code{y} and each dataset (row) in
#'    \code{yrep}. For this plot \code{yrep} should therefore contain only a
#'    small number of rows.
#'
#'    \if{html}{\figure{hist.png}{options: width="30\%" alt="Figure: hist.png"}}
#'    \if{latex}{\figure{hist.png}{options: width=4cm}}
#'   }
#'   \item{\code{ppc_dens}}{
#'    The same as \code{ppc_hist} but kernel density estimates are plotted
#'    instead of histograms.
#'
#'    \if{html}{\figure{dens.png}{options: width="30\%" alt="Figure: dens.png"}}
#'    \if{latex}{\figure{dens.png}{options: width=4cm}}
#'   }
#'   \item{\code{ppc_violin_grouped}}{
#'    The density estimate of \code{yrep} within each level of a grouping
#'    variable is plotted as a violin with 10\%, 50\%, and 90\% quantiles
#'    indicated by horizontal lines. The points in \code{y} corresponding to
#'    each grouping level are then overlaid on top of the violins.
#'
#'    \if{html}{\figure{violin.png}{options: width="30\%" alt="Figure: violin.png"}}
#'    \if{latex}{\figure{violin.png}{options: width=4cm}}
#'   }
#' }
#'
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#' @template seealso-color-scheme
#'
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
#' @rdname distributions
#'
ppc_hist <- function(y, yrep, ..., binwidth = NULL) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  plot_data <- ppc_dist_data(y, yrep)
  scheme <- get_color_scheme()
  fills <- c(scheme[["dark"]], scheme[["light"]])
  colors <- c(scheme[["dark_highlight"]], scheme[["light_highlight"]])
  ggplot(
    data = plot_data,
    mapping = aes_string(
      x = 'value',
      y = "..density..",
      fill = 'is_y',
      color = "is_y"
    )
  ) +
    geom_histogram(size = 0.25, binwidth = binwidth) +
    scale_fill_manual(values = fills) +
    scale_color_manual(values = colors) +
    facet_wrap("rep_id", switch = "x", labeller = label_parsed) +
    coord_cartesian(expand = FALSE) +
    theme_ppc(y_text = FALSE, x_lab = FALSE)
}


#' @export
#' @rdname distributions
#'
ppc_dens <- function(y, yrep, ...) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  plot_data <- ppc_dist_data(y, yrep)
  scheme <- get_color_scheme()
  fills <- c(scheme[["dark"]], scheme[["light"]])
  colors <- c(scheme[["dark_highlight"]], scheme[["light_highlight"]])

  ggplot(
    data = plot_data,
    mapping = aes_string(
      x = 'value',
      fill = 'is_y',
      color = "is_y"
    )
  ) +
    geom_density(size = 1) +
    scale_fill_manual(values = fills) +
    scale_color_manual(values = colors) +
    facet_wrap("rep_id", switch = "x", labeller = label_parsed) +
    coord_cartesian(expand = FALSE) +
    theme_ppc(y_text = FALSE, x_lab = FALSE)
}

#' @export
#' @rdname distributions
ppc_dens_overlay <- function(y, yrep, ...) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  plot_data <- ppc_dist_data(y, yrep)
  scheme <- get_color_scheme()
  colors <- c(scheme[["light"]], scheme[["dark_highlight"]])
  fills <- c(NA, scheme[["dark"]])

  ggplot(
    data = plot_data,
    mapping = aes_string(
      x = "value",
      group = "rep_id",
      color = "is_y",
      fill = "is_y",
      size = "is_y"
    )
  ) +
    geom_density() +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = fills) +
    scale_size_manual(values = c(0.25, 1)) +
    xlab(y_label()) +
    coord_cartesian(expand = FALSE) +
    theme_ppc(y_text = FALSE)
}

ppc_dist_data <- function(y, yrep) {
  yrep <- melt_yrep(yrep)
  yobs_lab <- "italic(y)"
  levels(yrep$rep_id) <- c(levels(yrep$rep_id), yobs_lab)
  ydat <- data.frame(
    rep_id = yobs_lab,
    y_id = seq_along(y),
    value = y
  )
  within(data = rbind(yrep, ydat), {
    rep_id <- relevel(rep_id, ref = yobs_lab)
    is_y <- rep_id == yobs_lab
  })
}


#' @export
#' @rdname distributions
#' @template args-group
#'
ppc_violin_grouped <- function(y, yrep, group, ...) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  group <- validate_group(group, y)
  plot_data <- ppc_group_data(y, yrep, group, stat = NULL)
  scheme <- get_color_scheme()

  is_y <- plot_data$variable == "y"
  ggplot(
    data = plot_data[!is_y,, drop = FALSE],
    mapping = aes_string(
      x = "group",
      y = "value",
      fill = "variable"
    )
  ) +
    geom_violin(
      fill = scheme[["light"]],
      color = scheme[["light_highlight"]],
      draw_quantiles = c(0.1, 0.5, 0.9)
    ) +
    geom_point(
      data = plot_data[is_y,, drop = FALSE],
      color = scheme[["dark_highlight"]],
      shape = 21
    ) +
    scale_fill_manual(
      name = "",
      values = scheme[["dark"]],
      labels = expression(italic(y))
    ) +
    labs(x = "Group", y = yrep_label()) +
    theme_ppc(legend_position = "right")
}
