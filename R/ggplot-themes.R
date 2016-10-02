#' bayesplot's default ggplot theme
#'
#' @export
#' @param base_size,base_family Passed to \code{\link[ggplot2]{theme_classic}}.
#' @param x_text,y_text Toggle axis text.
#' @param x_lab,y_lab Toggle axis labels (titles). Overrided by \code{x_text},
#'   \code{y_text}.
#' @param x_ticks,y_ticks Toggle axis tick marks.
#' @param legend_position A valid value to use for \code{legend.position} to
#'   pass to \code{\link[ggplot2]{theme}}.
#' @param ... Other arguments to pass to \code{\link[ggplot2]{theme}}.
#'
#' @return A ggplot \code{\link[ggplot2]{theme}} object.
#'
theme_default <-
  function(base_size = 12,
           base_family = "",
           x_text = TRUE,
           y_text = TRUE,
           x_lab = TRUE,
           y_lab = TRUE,
           x_ticks = FALSE,
           y_ticks = FALSE,
           legend_position = "none",
           ...) {
    thm <-
      theme_classic(base_size = base_size,
                    base_family = base_family) +
      theme(
        axis.line = element_line(size = 0.3),
        axis.ticks = element_blank(),
        legend.position = legend_position,
        strip.text = element_text(size = rel(0.75)),
        strip.background = element_rect(fill = "gray95", color = NA),
        strip.placement = "outside",
        plot.caption = element_text(hjust = 0.5, size = rel(0.8)),
        ...
      )
    dotnames <- names(list(...))
    if (x_ticks)
      thm <- thm + theme(axis.ticks.x = element_line())
    if (y_ticks)
      thm <- thm + theme(axis.ticks.y = element_line())
    if (!"legend.text" %in% dotnames)
      thm <- thm + theme(legend.text = element_text(face = "bold"))
    if (!"legend.title" %in% names(list(...)))
      thm <-
        thm + theme(legend.title = element_text(size = rel(0.8)))
    if (!"legend.key" %in% names(list(...)))
      thm <-
        thm + theme(legend.key = element_rect(color = "gray95", fill = NA))

    if (!y_text) {
      y_lab <- FALSE
      thm <- thm %+replace% theme(axis.text.y = element_blank())
    }
    if (!x_text) {
      x_lab <- FALSE
      thm <- thm %+replace% theme(axis.text.x = element_blank())
    }
    if (!y_lab)
      thm <- thm %+replace% theme(axis.title.y = element_blank())
    if (!x_lab)
      thm <- thm %+replace% theme(axis.title.x = element_blank())

    thm
  }
