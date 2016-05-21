# ggplot themes
#
#
# @param y_text Should y-axis text be displayed?
# @param legend_position A valid value to use for \code{legend.position} when
#   calling ggplot2::theme.
#
theme_ppc <- function(y_text = TRUE, legend_position = "none") {
  thm <- theme_classic() %+replace%
    theme(
      axis.line.x = element_line(size = 0.25),
      axis.line.y = element_line(size = 0.25),
      axis.ticks = element_blank(),
      legend.position = legend_position,
      strip.background = element_rect(fill = "gray95", color = NA)
    )
  if (y_text)
    return(thm)

  thm %+replace%
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
    )
}
