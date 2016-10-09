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
#' @param base_size,base_family Base font size and family (passed to
#'   \code{\link[ggplot2]{theme_bw}}). It is possible to set
#'   \code{"bayesplot.base_size"} and \code{"bayesplot.base_family"} via
#'   \code{\link{options}} to change the defaults, which are \code{12} and
#'   \code{"serif"}, respectively.
#'
#' @return A ggplot \link[ggplot2]{theme} object.
#'
#' @template seealso-helpers
#' @template seealso-colors
#'
#' @examples
#' thm <- theme_default()
#' class(thm)
#' names(thm)
#'
#' # plot using the default theme
#' x <- example_mcmc_draws()
#' mcmc_hist(x)
#'
#' # change the default font size and family
#' options(bayesplot.base_size = 10,
#'         bayesplot.base_family = "sans")
#' mcmc_hist(x)
#' mcmc_areas(x, regex_pars = "beta")
#'
#' # change back
#' options(bayesplot.base_size = 12,
#'         bayesplot.base_family = "serif")
#' mcmc_areas(x, regex_pars = "beta")
#'
#' # use one of the themes included in ggplot2
#' mcmc_dens_overlay(x) + ggplot2::theme_gray()
#'
theme_default <- function(base_size = getOption("bayesplot.base_size", 12),
                          base_family = getOption("bayesplot.base_family", "serif")) {
    theme_bw(base_family = base_family, base_size = base_size) +
    theme(
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(size = 0.4),
      axis.ticks = element_line(size = 0.3),
      legend.position = "right",
      strip.background = element_blank(),
      strip.text = element_text(size = rel(0.9)),
      strip.placement = "outside",
      # strip.background = element_rect(fill = "gray95", color = NA),
      panel.spacing = unit(1.5, "lines"),
      legend.background = element_blank(),
      legend.text = element_text(size = 13),
      legend.text.align = 0,
      legend.key = element_blank()
    )
}
