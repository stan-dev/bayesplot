#' Default plotting theme
#'
#' The \code{\link{theme_default}} function returns the default ggplot
#' \link{theme} used by the \pkg{bayesplot} plotting functions. Many of the
#' individual plotting functions also make small alterations to the theme using
#' the \pkg{bayesplot} \link[=bayesplot-convenience]{convenience functions}.
#' To use a different theme simply add that theme to the ggplot objects created
#' by the \pkg{bayesplot} plotting functions (see \strong{Examples}, below).
#'
#' @export
#' @param base_size,base_family Base font size and family. Passed to
#'   \code{\link[ggplot2]{theme_classic}} (\pkg{ggplot2}).
#'
#' @return A ggplot \link[ggplot2]{theme} object.
#'
#' @seealso The \pkg{bayesplot} \link[=bayesplot-convenience]{convenience
#'   functions}, many of which provide shortcuts for tweaking theme elements
#'   after creating a plot.
#'
#' @examples
#' thm <- theme_default()
#' class(thm)
#' names(thm)
#'
#' # Using a different theme instead of theme_default()
#' x <- example_mcmc_draws()
#' mcmc_hist(x) # uses theme_default
#' mcmc_hist(x) + ggplot2::theme_gray() # use theme_gray() from ggplot2
#'
theme_default <- function(base_size = 11, base_family = "") {
    ggthemes::theme_tufte(
      base_size = base_size,
      base_family = base_family
    ) +
    theme(
      axis.line = element_line(size = 0.4),
      axis.ticks = element_line(size = 0.3),
      legend.position = "right",
      strip.text = element_text(size = rel(0.9)),
      strip.placement = "outside",
      # strip.background = element_rect(fill = "gray95", color = NA),
      panel.spacing = unit(1.5, "lines"),
      plot.caption = element_text(hjust = 0.5, size = rel(0.8)),
      legend.text.align = 0,
      legend.text = element_text(face = "bold"),
      legend.key = element_rect(color = "gray95", fill = NA)
    )
}
