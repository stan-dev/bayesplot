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
#' # plot using the default theme automatically
#' x <- example_mcmc_draws()
#' mcmc_hist(x)
#'
#' # change the default font size and family for bayesplots
#' bayesplot_theme_set(theme_default(base_size = 8, base_family = "sans"))
#' mcmc_hist(x)
#' mcmc_areas(x, regex_pars = "beta")
#'
#' # change back
#' bayesplot_theme_set(theme_default())
#' mcmc_areas(x, regex_pars = "beta")
#'
#' # use one of the themes included in ggplot2
#' bayesplot_theme_set(ggplot2::theme_gray())
#' mcmc_dens_overlay(x)
#'
#' # change all ggplots to theme_default()
#' ggplot2::theme_set(theme_default())
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

bayes_theme_env <- new.env(parent = emptyenv())
bayes_theme_env$current <- theme_default()


#' Get, set, and modify the active bayesplot theme
#'
#' These functions are the \pkg{bayesplot} equivalent to
#' \code{\link[ggplot2]{theme_set}} and friends. They set, get, and update the
#' active theme but only apply them to \code{bayesplots}. The current/active
#' theme is automatically applied to every \code{bayesplot} you draw. Use
#' \code{bayesplot_theme_get} to get the current \pkg{bayesplot} theme, and
#' \code{bayesplot_theme_set} to change it. \code{bayesplot_theme_update} and
#' \code{bayesplot_theme_replace} are shorthands for changing individual
#' elements.
#'
#' @details \code{bayesplot_theme_set} and friends only apply to
#' \code{bayesplots}. Setting a theme other than the \pkg{ggplot2} default
#' (\code{\link[ggplot2]{theme_grey}}) will override any \pkg{bayesplot} themes.
#'
#' @inheritParams ggplot2::theme_set
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' # plot using the default theme automatically
#' x <- example_mcmc_draws()
#' mcmc_hist(x)
#'
#' # change the bayesplot theme and save the old theme
#' old <- bayesplot_theme_set(theme_minimal())
#' mcmc_hist(x)
#' mcmc_areas(x, regex_pars = "beta")
#' bayesplot_theme_set(old)
#'
#' # change the default font size and family for bayesplots
#' bayesplot_theme_update(text = element_text(size = 16))
#' mcmc_hist(x)
#' mcmc_areas(x, regex_pars = "beta")
#'
#' # change back to the default
#' bayesplot_theme_set(theme_default())
#' mcmc_areas(x, regex_pars = "beta")
#'
#' # change theme for all ggplots
#' theme_set(theme_dark())
#' mcmc_dens_overlay(x)
bayesplot_theme_get <- function() {
  if (identical(ggplot2::theme_gray(), ggplot2::theme_get())) {
    bayes_theme_env$current
  } else {
    ggplot2::theme_get()
  }
}

#' @rdname bayesplot_theme_get
#' @export
bayesplot_theme_set <- function(new) {
  missing <- setdiff(names(ggplot2::theme_gray()), names(new))
  if (length(missing) > 0) {
    warning("New theme missing the following elements: ",
            paste(missing, collapse = ", "), call. = FALSE)
  }

  old <- bayes_theme_env$current
  bayes_theme_env$current <- new
  invisible(old)
}

#' @rdname bayesplot_theme_get
#' @export
bayesplot_theme_update <- function(...) {
  bayesplot_theme_set(bayesplot_theme_get() + ggplot2::theme(...))
}

#' @rdname bayesplot_theme_get
#' @export
#' @importFrom ggplot2 %+replace%
bayesplot_theme_replace <- function(...) {
  bayesplot_theme_set(bayesplot_theme_get() %+replace% ggplot2::theme(...))
}
