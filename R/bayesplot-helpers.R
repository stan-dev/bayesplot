#' Convenience functions for adding or changing plot details
#'
#' Convenience functions for adding to (and changing details of) ggplot objects
#' (many of the objects returned by \pkg{bayesplot} functions). See the
#' \strong{Examples} section, below.
#'
#' @name bayesplot-helpers
#'
#' @param ... For the various \code{vline_} and \code{hline_} functions,
#'   \code{...} is passed to \code{\link[ggplot2]{geom_vline}} or
#'   \code{\link[ggplot2]{geom_hline}} to control the appearance of the line(s).
#'
#'   For functions ending in \code{_bg}, \code{...} is passed to
#'   \code{\link[ggplot2]{element_rect}}.
#'
#'   For functions ending in \code{_text} or \code{_title}, \code{...} is passed
#'   to \code{\link[ggplot2]{element_text}}.
#'
#'   For \code{xaxis_ticks} and \code{yaxis_ticks}, \code{...} is passed to
#'   \code{\link[ggplot2]{element_line}}.
#'
#'   For \code{overlay_function}, \code{...} is passed to
#'   \code{\link[ggplot2]{stat_function}}.
#'
#' @return
#' A \pkg{ggplot2} layer or \code{\link[ggplot2]{theme}} object that can be
#' added to existing ggplot objects, like those created by many of the
#' \pkg{bayesplot} plotting functions. See the \strong{Details} section.
#'
#' @details
#' \subsection{Add vertical or horizontal lines to plots at specified values}{
#' \itemize{
#' \item \code{vline_at} and \code{hline_at} return an object created by either
#' \code{geom_vline} or \code{geom_hline} that can be added to a ggplot object
#' to draw a vertical or horizontal line (at one or several values). If
#' \code{fun} is missing then the lines are drawn at the values in \code{v}. If
#' \code{fun} is specified then the lines are drawn at the values returned by
#' \code{fun(v)}.
#'
#' \item \code{vline_0} and \code{hline_0} are wrappers for \code{vline_at} and
#' \code{hline_at} with \code{v = 0} and \code{fun} missing.
#'
#' \item \code{lbub} returns a \emph{function} that takes a single argument
#' \code{x} and returns the lower and upper bounds (\code{lb}, \code{ub}) of the
#' \code{100*p}\% central interval of \code{x}, as well as the median (if
#' \code{med} is \code{TRUE}).
#' }
#' }
#' \subsection{Control appearance of facet strips}{
#' \itemize{
#' \item \code{facet_text} and \code{facet_bg} return ggplot2 theme objects that
#' can be added to an existing plot (ggplot object) to format the text and the
#' background for the facet strips.
#' }
#' }
#' \subsection{Move legend, remove legend, or style the legend text}{
#' \itemize{
#' \item \code{legend_move} and \code{legend_none} return a ggplot2 theme object
#' that can be added to an existing plot (ggplot object) in order to change the
#' position of the legend (\code{legend_move}) or remove the legend
#' (\code{legend_none}). \code{legend_text} works much like \code{facet_text},
#' except it controls the legend text.
#' }
#' }
#' \subsection{Control appearance of \eqn{x}-axis and \eqn{y}-axis features}{
#' \itemize{
#' \item \code{xaxis_title} and \code{yaxis_title} return a ggplot2 theme object
#' that can be added to an existing plot (ggplot object) in order to toggle or
#' format the titles displayed on the \code{x} or \code{y} axis. (To change
#' the titles themselves use \code{\link[ggplot2]{labs}}.)
#'
#' \item \code{xaxis_text} and \code{yaxis_text} return a ggplot2 theme object
#' that can be added to an existing plot (ggplot object) in order to toggle or
#' format the text displayed on the \code{x} or \code{y} axis (e.g. tick
#' labels).
#'
#' \item \code{xaxis_ticks} and \code{yaxis_ticks} return a ggplot2 theme object
#' that can be added to an existing plot (ggplot object) to change the
#' appearance of the axis tick marks.
#' }
#' }
#' \subsection{Customize plot background}{
#' \itemize{
#' \item \code{plot_bg} returns a ggplot2 theme object that can be added to an
#' existing plot (ggplot object) to format the background of the \emph{entire} plot.
#' \item \code{panel_bg} returns a ggplot2 theme object that can be added to an
#' existing plot (ggplot object) to format the background of the just the
#' plotting area.
#' \item \code{grid_lines} returns a ggplot2 theme object that can be added to
#' an existing plot (ggplot object) to add grid lines to the plot background.
#' }
#' }
#' \subsection{Superimpose a function on an existing plot}{
#' \itemize{
#' \item \code{overlay_function} is a simple wrapper for
#' \code{\link[ggplot2]{stat_function}} but with the \code{inherit.aes} argument
#' fixed to \code{FALSE}. Fixing \code{inherit.aes=FALSE} will avoid potential
#' errors due to the \code{\link[ggplot2]{aes}}thetic mapping used by certain
#' \pkg{bayesplot} plotting functions).
#' }
#' }
#'
#' @seealso \code{\link{theme_default}} for the default ggplot theme used by
#'   \pkg{bayesplot}.
#'
#' @examples
#' color_scheme_set("gray")
#' x <- example_mcmc_draws(chains = 1)
#' dim(x)
#' colnames(x)
#'
#'
#' ###################################
#' ### vertical & horizontal lines ###
#' ###################################
#' (p <- mcmc_intervals(x, regex_pars = "beta"))
#'
#' # vertical line at zero (with some optional styling)
#' p + vline_0()
#' p + vline_0(size = 0.25, color = "darkgray", linetype = 2)
#'
#' # vertical line(s) at specified values
#' v <- c(-0.5, 0, 0.5)
#' p + vline_at(v, linetype = 3, size = 0.25)
#'
#' my_lines <- vline_at(v, alpha = 0.25, size = 0.75 * c(1, 2, 1),
#'                      color = c("maroon", "skyblue", "violet"))
#' p + my_lines
#'
#' \donttest{
#' # add vertical line(s) at computed values
#' # (three ways of getting lines at column means)
#' color_scheme_set("brightblue")
#' p <- mcmc_intervals(x, regex_pars = "beta")
#' p + vline_at(x[, 3:4], colMeans)
#' p + vline_at(x[, 3:4], "colMeans", color = "darkgray",
#'              lty = 2, size = 0.25)
#' p + vline_at(x[, 3:4], function(a) apply(a, 2, mean),
#'              color = "orange",
#'              size = 2, alpha = 0.1)
#' }
#'
#' # using the lbub function to get interval lower and upper bounds (lb, ub)
#' color_scheme_set("pink")
#' parsed <- ggplot2::label_parsed
#' p2 <- mcmc_hist(x, pars = "beta[1]", binwidth = 1/20,
#'                 facet_args = list(labeller = parsed))
#' (p2 <- p2 + facet_text(size = 16))
#'
#' b1 <- x[, "beta[1]"]
#' p2 + vline_at(b1, fun = lbub(0.8), color = "gray20",
#'               size = 2 * c(1,.5,1), alpha = 0.75)
#' p2 + vline_at(b1, lbub(0.8, med = FALSE), color = "gray20",
#'               size = 2, alpha = 0.75)
#'
#' ##########################
#' ### format axis titles ###
#' ##########################
#' color_scheme_set("green")
#' y <- example_y_data()
#' yrep <- example_yrep_draws()
#' (p3 <- ppc_stat(y, yrep, stat = "median", binwidth = 1/4))
#'
#' # turn off the legend, turn on x-axis title
#' p3 +
#'  legend_none() +
#'  xaxis_title(size = 13, family = "sans") +
#'  ggplot2::xlab(expression(italic(T(y)) == median(italic(y))))
#'
#'
#' ################################
#' ### format axis & facet text ###
#' ################################
#' color_scheme_set("gray")
#' p4 <- mcmc_trace(example_mcmc_draws(), pars = c("alpha", "sigma"))
#'
#' myfacets <-
#'  facet_bg(fill = "gray30", color = NA) +
#'  facet_text(face = "bold", color = "skyblue", size = 14)
#' p4 + myfacets
#'
#' \donttest{
#' ##########################
#' ### control tick marks ###
#' ##########################
#' p4 +
#'  myfacets +
#'  yaxis_text(FALSE) +
#'  yaxis_ticks(FALSE) +
#'  xaxis_ticks(size = 1, color = "skyblue")
#' }
#'
#' ##############################
#' ### change plot background ###
#' ##############################
#' color_scheme_set("blue")
#'
#' # add grid lines
#' ppc_stat(y, yrep) + grid_lines()
#'
#' # panel_bg vs plot_bg
#' ppc_scatter_avg(y, yrep) + panel_bg(fill = "gray90")
#' ppc_scatter_avg(y, yrep) + plot_bg(fill = "gray90")
#'
#' color_scheme_set("yellow")
#' p5 <- ppc_scatter_avg(y, yrep, alpha = 1)
#' p5 + panel_bg(fill = "gray20") + grid_lines(color = "white")
#' \donttest{
#' color_scheme_set("purple")
#' ppc_dens_overlay(y, yrep[1:30, ]) +
#'  legend_text(size = 14) +
#'  legend_move(c(0.75, 0.5)) +
#'  plot_bg(fill = "gray90") +
#'  panel_bg(color = "black", fill = "gray99", size = 3)
#' }
#'
#'
#' ###############################################
#' ### superimpose a function on existing plot ###
#' ###############################################
#' # compare posterior of beta[1] to Gaussian with same posterior mean
#' # and sd as beta[1]
#' purple_gaussian <-
#'   overlay_function(
#'     fun = dnorm,
#'     args = list(mean(x[,, "beta[1]"]), sd(x[,, "beta[1]"])),
#'     color = "purple",
#'     size = 2
#'   )
#'
#' color_scheme_set("gray")
#' mcmc_hist(x, pars = "beta[1]") + purple_gaussian
#' \donttest{mcmc_dens(x, pars = "beta[1]") + purple_gaussian}
#'
NULL


# lines -------------------------------------------------------------------
#' @rdname bayesplot-helpers
#' @export
#' @param v Either a numeric vector specifying the value(s) at which to
#'   draw the vertical or horizontal line(s), or an object of any type to use as
#'   the first argument to \code{fun}.
#' @param fun A function, or the name of a function, that returns a numeric
#'   vector.
#' @param na.rm A logical scalar passed to the appropriate geom (e.g.
#'   \code{\link[ggplot2]{geom_vline}}). The default is \code{TRUE}.
#'
vline_at <- function(v, fun, ..., na.rm = TRUE) {
  geom_vline(xintercept = calc_v(v, fun),
             na.rm = na.rm,
             ...)
}

#' @rdname bayesplot-helpers
#' @export
hline_at <- function(v, fun, ..., na.rm = TRUE) {
  geom_hline(yintercept = calc_v(v, fun),
             na.rm = na.rm,
             ...)
}

#' @rdname bayesplot-helpers
#' @export
vline_0 <- function(..., na.rm = TRUE) {
  geom_vline(xintercept = 0,
             na.rm = na.rm,
             ...)
}

#' @rdname bayesplot-helpers
#' @export
#'
hline_0 <- function(..., na.rm = TRUE) {
  geom_hline(yintercept = 0,
             na.rm = na.rm,
             ...)
}


# intervals ---------------------------------------------------------------
#' @rdname bayesplot-helpers
#' @export
#' @param p The probability mass (in [0,1]) to include in the interval.
#' @param med Should the median also be included in addition to the lower
#' and upper bounds of the interval?
#'
lbub <- function(p, med = TRUE) {
  function(x) calc_intervals(x, p, med = med)
}

# internal
calc_v <- function(v, fun, fun_args, ...) {
  if (missing(v))
    stop("'v' can't be missing.", call. = FALSE)
  if (missing(fun))
    return(v)
  f <- match.fun(fun)
  if (missing(fun_args))
    return(f(v))

  do.call(f, c(list(v), fun_args))
}

calc_intervals <- function(x, p, med = TRUE, ...) {
  a <- (1 - p) / 2
  pr <- c(a, if (med) 0.5, 1 - a)
  quantile(x, pr, ...)
}


# legend stuff ------------------------------------------------------------
#' @rdname bayesplot-helpers
#' @export
#' @param position The position of the legend. Either a numeric vector (of
#'   length 2) giving the relative coordinates (between 0 and 1) for the legend,
#'   or a string among \code{"right"}, \code{"left"}, \code{"top"},
#'   \code{"bottom"}. Using \code{position = "none"} is also allowed and is
#'   equivalent to using \code{legend_none()}.
#'
legend_move <- function(position = "right") {
  theme(legend.position = position)
}
#' @rdname bayesplot-helpers
#' @export
legend_none <- function() {
  theme(legend.position = "none")
}
#' @rdname bayesplot-helpers
#' @export
legend_text <- function(...) {
  theme(legend.text = element_text(...))
}


# axis stuff --------------------------------------------------------------
#' @rdname bayesplot-helpers
#' @export
xaxis_title <- function(on = TRUE, ...) {
  if (!on)
    return(xlab(NULL))
  theme(axis.title.x = element_text(...))
}
#' @rdname bayesplot-helpers
#' @export
xaxis_text <- function(on = TRUE, ...) {
  theme(axis.text.x = if (on)
    element_text(...)
    else
      element_blank())
}
#' @rdname bayesplot-helpers
#' @export
xaxis_ticks <- function(on = TRUE, ...) {
  theme(axis.ticks.x = if (on)
    element_line(...)
    else
      element_blank())
}
#' @rdname bayesplot-helpers
#' @export
yaxis_title <- function(on = TRUE, ...) {
  if (!on)
    return(ylab(NULL))
  theme(axis.title.y = element_text(...))
}
#' @rdname bayesplot-helpers
#' @export
yaxis_text <- function(on = TRUE, ...) {
  theme(axis.text.y = if (on)
    element_text(...)
    else
      element_blank())
}
#' @rdname bayesplot-helpers
#' @export
yaxis_ticks <- function(on = TRUE, ...) {
  theme(axis.ticks.y = if (on)
    element_line(...)
    else
      element_blank())
}


# facet stuff -------------------------------------------------------------
#' @rdname bayesplot-helpers
#' @export
#' @param on For functions modifying ggplot \link[ggplot2]{theme} elements, set
#'   \code{on=FALSE} to set the element to \code{\link[ggplot2]{element_blank}}.
#'   For example, facet text can be removed by adding
#'   \code{facet_text(on=FALSE)}, or simply \code{facet_text(FALSE)} to a ggplot
#'   object. If \code{on=TRUE} (the default), then \code{...} can be used to
#'   customize the appearance of the theme element.
#'
facet_text <- function(on = TRUE, ...) {
  theme(strip.text = if (on)
    element_text(...)
    else
      element_blank())
}
#' @rdname bayesplot-helpers
#' @export
facet_bg <- function(on = TRUE, ...) {
  theme(strip.background = if (on)
    element_rect(...)
    else
      element_blank())
}

# plot background ---------------------------------------------------------
#' @rdname bayesplot-helpers
#' @export
panel_bg <- function(on = TRUE, ...) {
  theme(panel.background = if (on)
    element_rect(...)
    else
      element_blank())
}

#' @rdname bayesplot-helpers
#' @export
plot_bg <- function(on = TRUE, ...) {
  theme(plot.background = if (on)
    element_rect(...)
    else
      element_blank())
}

#' @rdname bayesplot-helpers
#' @export
#' @param color,size Passed to \code{\link[ggplot2]{element_line}}.
#'
grid_lines <- function(color = "gray50", size = 0.2) {
  theme(
    panel.grid.major = element_line(color = color, size = size),
    panel.grid.minor = element_line(color = color, size = size * 0.5)
  )
}



# overlay functions on an existing plot -----------------------------------
#' @rdname bayesplot-helpers
#' @export
overlay_function <- function(...) {
  stat_function(..., inherit.aes = FALSE)
}

