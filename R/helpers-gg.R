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
#' style <- annotation_style()
#' maybe_vertical_line <- if (0 > min(xs) && 0 < max(xs)) {
#'   vline_0(color = style$color, linewidth = style$linewidth)
#' } else {
#'   geom_ignore()
#' }
geom_ignore <- function(...) {
  geom_blank(
    mapping = NULL, data = NULL,
    show.legend = FALSE, inherit.aes = FALSE)
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

#' Same as `modify_aes_` but using `aes()` instead of `aes_()` (now deprecated).
#' Often `...` will need to contain expression of the form `.data$x` to avoid R cmd check warnings
#' @noRd
modify_aes <- function(mapping, ...) {
  utils::modifyList(mapping, aes(...))
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
    linewidth = thm$axis.line$linewidth %||% thm$line$linewidth %||% 0.5
  )
}

force_x_axis_in_facets <- function() {
  thm <- bayesplot_theme_get()
  annotate(
    "segment",
    x = -Inf, xend = Inf,
    y = -Inf, yend = -Inf,
    color = thm$axis.line$colour %||% thm$line$colour %||% "black",
    linewidth = thm$axis.line$linewidth %||% thm$line$linewidth %||% 0.5
  )
}

# Derive annotation line aesthetics from the active theme's gridlines.
# When the theme has visible gridlines, the reference line inherits their
# color at twice the major gridline width. When gridlines are blank (e.g.
# bayesplot's default theme), falls back to a light gray.
annotation_style <- function() {
  thm <- bayesplot_theme_get()
  grid <- calc_element("panel.grid.major", thm)
  if (inherits(grid, "element_blank") || is.null(grid)) {
    return(list(color = "gray90", linewidth = 0.5))
  }
  minor <- calc_element("panel.grid.minor", thm)
  minor_lw <- if (!inherits(minor, "element_blank") && !is.null(minor$linewidth)) {
    minor$linewidth
  } else {
    0.125
  }
  major_lw <- grid$linewidth %||% (minor_lw * 2)
  list(
    color = grid$colour %||% "gray90",
    linewidth = major_lw * 2
  )
}

no_legend_spacing <- function() {
  theme(legend.spacing.y = unit(0, "cm"))
}
reduce_legend_spacing <- function(cm) {
  theme(legend.spacing.y = unit(-cm, "cm"))
}
space_legend_keys <- function(relative_size = 2, color = "white") {
  theme(legend.key = element_rect(linewidth = rel(relative_size), color = color))
}


# set aesthetic mapping for histograms depending on freq argument
set_hist_aes <- function(freq = TRUE, ...) {
  if (freq) {
    # aes_(x = ~ value, ...)
    aes(x = .data$value, ...)
  } else {
    # aes_(x = ~ value, y = ~ after_stat(density), ...)
    aes(x = .data$value, y = after_stat(density), ...)
  }
}

scale_color_ppc <-
  function(name = NULL,
           values = NULL,
           labels = NULL,
           ...) {
    scale_color_manual(
      name = name %||% "",
      values = values %||% get_color(c("dh", "lh")),
      labels = labels %||% c(y_label(), yrep_label()),
      ...
    )
  }

scale_fill_ppc <-
  function(name = NULL,
           values = NULL,
           labels = NULL,
           ...) {
    scale_fill_manual(
      name = name %||% "",
      values = values %||% get_color(c("d", "l")),
      labels = labels %||% c(y_label(), yrep_label()),
      ...
    )
  }

scale_color_ppd <-
  function(name = NULL,
           values = get_color("mh"),
           labels = ypred_label(),
           ...) {
    scale_color_ppc(name = name,
                    values = values,
                    labels = labels,
                    ...)
  }

scale_fill_ppd <-
  function(name = NULL,
           values = get_color("m"),
           labels = ypred_label(),
           ...) {
    scale_fill_ppc(name = name,
                   values = values,
                   labels = labels,
                   ...)
  }
