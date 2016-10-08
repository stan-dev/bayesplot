# ggplot2 convenience functions for internal use --------------------------
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
  thm <- theme_default()
  annotate("segment",
           x = c(-Inf, -Inf), xend = c(Inf,-Inf),
           y = c(-Inf,-Inf), yend = c(-Inf, Inf),
           color = thm$axis.line$colour %||% "black",
           size = thm$axis.line$size %||% 0.5)
}
no_legend_spacing <- function() {
  theme(legend.spacing.y = unit(0, "cm"))
}
