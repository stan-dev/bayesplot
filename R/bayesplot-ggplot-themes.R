#' Default bayesplot plotting theme
#'
#' The \code{\link{theme_default}} function returns the default ggplot
#' \link{theme} used by the \pkg{bayesplot} plotting functions.
#' After loading the \pkg{bayesplot} package, this theme will be the default for
#' \emph{all} graphs made with \pkg{ggplot2}. See the \strong{Details} section,
#' below.
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
#' @details After loading \pkg{bayesplot}, if you subsequently change the
#' default \pkg{ggplot2} theme (i.e., by calling
#' \code{\link[ggplot2]{theme_set}} or loading a different package that changes
#' the theme) then \pkg{bayesplot} will use that theme instead. To change back
#' to the default \pkg{bayesplot} theme use \code{bayesplot::theme_default()} as
#' the argument to \code{\link[ggplot2]{theme_set}}.
#'
#' @template seealso-helpers
#' @template seealso-colors
#'
#' @examples
#' class(theme_default())
#'
#' # plot using the default theme
#' ggplot2::theme_set(theme_default())
#' x <- example_mcmc_draws()
#' mcmc_hist(x)
#'
#' # change the default font size and family
#' ggplot2::theme_set(theme_default(base_size = 8, base_family = "sans"))
#' mcmc_hist(x)
#' mcmc_areas(x, regex_pars = "beta")
#'
#' # change back
#' ggplot2::theme_set(theme_default())
#' mcmc_areas(x, regex_pars = "beta")
#'
#' # use one of the themes included in ggplot2
#' ggplot2::theme_set(ggplot2::theme_gray())
#' mcmc_dens_overlay(x)
#'
#' # change back to bayesplot default theme
#' ggplot2::theme_set(bayesplot::theme_default())
#' mcmc_dens_overlay(x)
#'
theme_default <-
  function(base_size = getOption("bayesplot.base_size", 12),
           base_family = getOption("bayesplot.base_family", "serif")) {

    theme_bw(
      base_family = base_family,
      base_size = base_size
    ) +
      theme(
        plot.background = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = 0.4),
        axis.ticks = element_line(size = 0.3),
        strip.background = element_blank(),
        strip.text = element_text(size = rel(0.9)),
        strip.placement = "outside",
        # strip.background = element_rect(fill = "gray95", color = NA),
        panel.spacing = unit(1.5, "lines"),
        legend.position = "right",
        legend.background = element_blank(),
        legend.text = element_text(size = 13),
        legend.text.align = 0,
        legend.key = element_blank()
      )
  }
