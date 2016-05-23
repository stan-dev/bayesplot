# ggplot theme
#
#
# @param y_text Should y-axis text be displayed?
# @param legend_position A valid value to use for \code{legend.position} when
#   calling ggplot2::theme.
# @param ... Arguments passed to \code{\link[ggplot2]{theme}}.
#
theme_ppc <-
  function(y_text = TRUE,
           x_lab = TRUE,
           legend_position = "none",
           ...) {
    thm <- theme_classic() +
      theme(
        axis.line.x = element_line(size = 0.25),
        axis.line.y = element_line(size = 0.25),
        axis.ticks = element_blank(),
        legend.position = legend_position,
        strip.text = element_text(size = rel(0.75)),
        strip.background = element_rect(fill = "gray95", color = NA),
        ...
      )
    if (!y_text)
      thm <- thm %+replace%
        theme(axis.text.y = element_blank(), axis.title.y = element_blank())
    if (!x_lab)
      thm <- thm %+replace%
        theme(axis.title.x = element_blank())

    thm
  }
