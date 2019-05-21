# ggplot2 convenience functions for internal use --------------------------

#' Do nothing plotting geom
#'
#' Adding `geom_ignore()` to a ggplot will not affect its appearance. This
#' function is useful for when plot elements should appear conditionally.
#'
#' @param ... arguments to a ggplot2 function to be ignored
#' @noRd
#' @examples
#' # Draw a vertical line at zero (or do nothing)
#' xs <- -2:2
#' maybe_vertical_line <- if (0 > min(xs) && 0 < max(xs)) {
#'   vline_0(color = "gray90", size = 0.5)
#' } else {
#'   geom_ignore()
#' }
geom_ignore <- function(...) {
  geom_blank(
    mapping = NULL, data = NULL,
    show.legend = FALSE, inherit.aes = FALSE)
}

#' Wrappers for ggridges
#'
#' The "area ridges" are for use in `mcmc_areas()`. The scale of 1 and the
#' identity statistic prevent the ridges from overlapping.
#' `geom_density_ridges2()` draws closed polygons.
#'
#' @importFrom ggridges geom_density_ridges geom_density_ridges2
#' @noRd
geom_area_ridges <- function(...) {
  ggridges::geom_density_ridges(..., stat = "identity", scale = .95)
}

geom_area_ridges2 <- function(...) {
  ggridges::geom_density_ridges2(..., stat = "identity", scale = .95)
}


#' Add new aesthetic mappings to a list of aesthetic mappings
#'
#' @param mapping a list of `uneval` aesthetic mappings (created by `aes_()`)
#' @param ... additional mappings to add, e.g., `color = ~ parameter`
#' @return the updated list
#' @noRd
modify_aes_ <- function(mapping, ...) {
  utils::modifyList(mapping, aes_(...))
}



facet_wrap_parsed <- function(...) {
  facet_wrap(..., labeller = label_parsed)
}
dont_expand_y_axis <- function(expand = c(0,0)) {
  scale_y_continuous(expand = expand)
}
dont_expand_x_axis <- function(expand = c(0,0)) {
  scale_x_continuous(expand = expand)
}
dont_expand_axes <- function() {
  coord_cartesian(expand = FALSE)
}
force_axes_in_facets <- function() {
  thm <- bayesplot_theme_get()
  annotate(
    "segment",
    x = c(-Inf, -Inf), xend = c(Inf,-Inf),
    y = c(-Inf,-Inf), yend = c(-Inf, Inf),
    color = thm$axis.line$colour %||% thm$line$colour %||% "black",
    size = thm$axis.line$size %||% thm$line$size %||% 0.5
  )
}

force_x_axis_in_facets <- function() {
  thm <- bayesplot_theme_get()
  annotate(
    "segment",
    x = -Inf, xend = Inf,
    y = -Inf, yend = -Inf,
    color = thm$axis.line$colour %||% thm$line$colour %||% "black",
    size = thm$axis.line$size %||% thm$line$size %||% 0.5
  )
}

no_legend_spacing <- function() {
  theme(legend.spacing.y = unit(0, "cm"))
}
reduce_legend_spacing <- function(cm) {
  theme(legend.spacing.y = unit(-cm, "cm"))
}
space_legend_keys <- function(relative_size = 2, color = "white") {
  theme(legend.key = element_rect(size = rel(relative_size), color = color))
}


# set aesthetic mapping for histograms depending on freq argument
set_hist_aes <- function(freq = TRUE, ...) {
  if (freq)
    aes_(x = ~ value, ...)
  else
    aes_(x = ~ value, y = ~ ..density.., ...)
}
