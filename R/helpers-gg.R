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
#'   vline_0(color = "gray90", linewidth = 0.5)
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
#' @param mapping a list of `uneval` aesthetic mappings (created by `aes()`)
#' @param ... additional mappings to add using `.data$` syntax
#' @return the updated list
#' @noRd
modify_aes <- function(mapping, ...) {
  utils::modifyList(mapping, aes(...))
}



facet_wrap_parsed <- function(...) {
  facet_wrap(..., labeller = label_parsed)
}
dont_expand_y_axis <- function(expand = expansion(0, 0)) {
  scale_y_continuous(expand = expand)
}
dont_expand_x_axis <- function(expand = expansion(0, 0)) {
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
      name = name,
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
      name = name,
      values = values %||% get_color(c("d", "l")),
      labels = labels %||% c(y_label(), yrep_label()),
      ...
    )
  }

scale_color_ppd <-
  function(name = NULL,
           values = NULL,
           labels = NULL,
           highlight = TRUE,
           show_marginal = FALSE,
           ...) {
    if (isTRUE(show_marginal)) {
      if (isTRUE(highlight)) {
        cl <- c("dh", "lh")
      } else {
        cl <- c("d", "l")
      }
      default_values <- setNames(get_color(cl), nm = c("PPD", "ypred"))
    } else {
      if (isTRUE(highlight)) {
        default_values <- get_color("mh")
      } else {
        default_values <- get_color("m")
      }
    }

    scale_color_ppc(
      name = name,
      values = values %||% default_values,
      labels = labels %||% ypred_label(),
      ...
    )
  }

scale_fill_ppd <-
  function(name = NULL,
           values = NULL,
           labels = NULL,
           show_marginal = FALSE,
           ...) {
    if (isTRUE(show_marginal)) {
      default_values <- setNames(get_color(c("d", "l")), nm = c("PPD", "ypred"))
    } else {
      default_values <- get_color("m")
    }

    scale_fill_ppc(
      name = name,
      values = values %||% default_values,
      labels = labels %||% ypred_label(),
      ...
    )
  }


scale_linetype_ppd <-
  function(name = NULL,
           values = NULL,
           labels = NULL,
           ...) {
    scale_linetype_manual(
      name = name,
      values = values %||% c(PPD = "5111", ypred = "solid"),
      labels = labels %||% ypred_label(),
      ...
    )
  }

scale_shape_ppd <-
  function(name = NULL,
           values = NULL,
           labels = NULL,
           ...) {
    scale_shape_manual(
      name = name,
      values = values %||% c(ypred = 21, PPD = 23),
      labels = labels %||% ypred_label(),
      ...
    )
  }
