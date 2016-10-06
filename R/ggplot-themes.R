#' bayesplot's default ggplot theme
#'
#' A modified version of \code{\link[ggplot2]{theme_classic}}.
#'
#' @export
#' @param base_size,base_family Passed to \code{\link[ggplot2]{theme_classic}}.
#' @return A ggplot \code{\link[ggplot2]{theme}} object.
#'
#' @seealso The \link[=bayesplot-convenience]{bayesplot convenience functions},
#'   many of which provide shortcuts for tweaking theme elements after creating
#'   a plot.
#'
theme_default <- function(base_size = 11, base_family = "") {
    thm <-
      theme_classic(base_size = base_size,
                    base_family = base_family) +
      theme(
        axis.line = element_line(size = 0.3),
        axis.ticks = element_line(size = 0.25),
        legend.position = "right",
        strip.placement = "outside",
        strip.background = element_rect(fill = "gray95", color = NA),
        panel.spacing = unit(1.5, "lines"),
        plot.caption = element_text(hjust = 0.5, size = rel(0.8)),
        legend.text = element_text(face = "bold"),
        legend.key = element_rect(color = "gray95", fill = NA)
      )
  }
