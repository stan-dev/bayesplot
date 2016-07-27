#' Convenience functions for adding or changing plot details
#'
#' Convenience functions for adding to (and changing details of) ggplot objects
#' (many of the objects returned by \pkg{bayesplot} functions). See the
#' \strong{Examples} section, below.
#'
#' @name bayesplot-convenience
#'
#' @param ... For the various \code{vline_} and \code{hline_} functions,
#'   \code{...} is passed to \code{\link[ggplot2]{geom_vline}} or
#'   \code{\link[ggplot2]{geom_hline}} to control the appearance of the line(s).
#'
#'   For \code{facet_text}, \code{xaxis_text}, \code{yaxis_text},
#'   \code{xaxis_title}, and \code{yaxis_title}, \code{...} is passed to
#'   \code{\link[ggplot2]{element_text}} if \code{on = TRUE}.
#'
#'   For \code{xaxis_ticks} and \code{yaxis_ticks}, \code{...} is passed to
#'   \code{\link[ggplot2]{element_line}} if \code{on = TRUE}.
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
#' \subsection{Move or remove plot legend}{
#' \itemize{
#' \item \code{move_legend} and \code{no_legend} return a ggplot2 theme object
#' that can be added to an existing plot (ggplot object) in order to change the
#' position of the legend (\code{move_legend}) or remove the legend
#' (\code{no_legend}).
#' }
#' }
#' \subsection{Control appearance of facet labels}{
#' \itemize{
#' \item \code{facet_text} returns a ggplot2 theme object that can be added to
#' an existing plot (ggplot object) to format the text in facet labels.
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
#'
#' @examples
#' set_color_scheme("gray")
#' x <- fake_draws(chains = 1)
#' dim(x)
#' colnames(x)
#'
#' (p <- mcmc_intervals(x))
#'
#' ### vertical line at 0
#' p + vline_0()
#' p + vline_0(size = 0.25, color = "darkgray", linetype = 2)
#'
#'
#' ### vertical line(s) at specified values
#' p + vline_at(c(-1, 0, 1), linetype = 3, size = 0.25)
#'
#' my_lines <- vline_at(
#'   v = c(-1, 0, 1),
#'   color = c("maroon", "skyblue", "violet"),
#'   size = 0.75 * c(1, 2, 1),
#'   alpha = 0.25
#' )
#' p + my_lines
#'
#' ### add vertical line(s) at computed values
#' # three ways of getting lines at column means
#' set_color_scheme("teal")
#' p <- mcmc_intervals(x)
#'
#' p + vline_at(x, colMeans)
#' p + vline_at(x, "colMeans", lty = 2, size = 0.25,
#'              color = get_color_scheme()[["mid"]])
#' p + vline_at(x, function(a) apply(a, 2, mean),
#'              size = 2, alpha = 0.1,
#'              color = get_color_scheme()[["dark"]])
#'
#'
#' ### using the lbub function
#' set_color_scheme("blue")
#' (p2 <- mcmc_hist(x, pars = "beta[1]"))
#'
#' b1 <- x[, "beta[1]"]
#' p2 + vline_at(b1, fun = lbub(0.8))
#' p2 + vline_at(b1, lbub(0.8, med = FALSE))
#'
#' p2 +
#'  vline_at(
#'    b1,
#'    lbub(0.5),
#'    color = get_color_scheme("pink")[["dark_highlight"]],
#'    alpha = 0.5,
#'    size = 1.5 * c(1,2,1)
#'  )
#'
#' ### control appearance of axis titles
#' set_color_scheme("pink")
#' y <- rnorm(100)
#' yrep <- t(replicate(150, rnorm(length(y), mean = y, sd = 5)))
#' (p3 <- ppc_stat(y, yrep, stat = "median", binwidth = 0.15))
#'
#' # reformat x-axis title
#' (p3 <- p3 + xaxis_title(size = 15, color = "darkgray"))
#'
#' # formatting stays even if we change the title content
#' p3 + ggplot2::xlab(expression(bolditalic(T): y %->% median(y)))
#'
#' # remove x axis title and turn on y-axis title
#' p3 +
#'  yaxis_title() +
#'  xaxis_title(on = FALSE)
#'
#'
#' ### control appearance facet and axis text
#' set_color_scheme("gray")
#' p4 <- mcmc_trace(fake_draws(), pars = c("alpha", "sigma"))
#'
#' myfacets <- facet_text(face = "bold", color = "purple4", size = 14)
#' p4 + myfacets
#'
#' # dont show y-axis text
#' p4 + myfacets + yaxis_text(FALSE)
#'
#' ### control axis tick marks
#' p4 +
#'  myfacets +
#'  yaxis_text(FALSE) +
#'  xaxis_ticks(size = .75, color = "purple4")
#'
NULL


# lines -------------------------------------------------------------------
#' @rdname bayesplot-convenience
#' @export
#' @param v Either a numeric vector specifying the \code{v}alue(s) at which to
#'   draw the vertical or horizontal line(s), or an object of any type to use as
#'   the first argument to \code{fun}.
#' @param fun A function, or the name of a function, that returns a numeric
#'   vector.
#'
vline_at <- function(v, fun, ...) {
  geom_vline(xintercept = calc_v(v, fun),
             ...)
}

#' @rdname bayesplot-convenience
#' @export
hline_at <- function(v, fun, ...) {
  geom_hline(yintercept = calc_v(v, fun),
             ...)
}

#' @rdname bayesplot-convenience
#' @export
vline_0 <- function(...) {
  geom_vline(xintercept = 0, ...)
}

#' @rdname bayesplot-convenience
#' @export
#'
hline_0 <- function(...) {
  geom_hline(yintercept = 0, ...)
}


# intervals ---------------------------------------------------------------
#' @rdname bayesplot-convenience
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


# move or remove legend ---------------------------------------------------
#' @rdname bayesplot-convenience
#' @export
no_legend <- function() {
  theme(legend.position = "none")
}

#' @rdname bayesplot-convenience
#' @export
#' @param position The position of the legend. Either a numeric vector (of
#'   length 2) giving the relative coordinates (between 0 and 1) for the legend,
#'   or a string among \code{"right"}, \code{"left"}, \code{"top"},
#'   \code{"bottom"}. Using \code{position = "none"} is also allowed and is
#'   equivalent to using \code{no_legend()}.
#'
move_legend <- function(position = "right") {
  theme(legend.position = position)
}


# axis and facet text --------------------------------------------------
#' @rdname bayesplot-convenience
#' @export
xaxis_title <- function(on = TRUE, ...) {
  theme(axis.title.x = if (on)
    element_text(...)
    else
      element_blank())
}
#' @rdname bayesplot-convenience
#' @export
yaxis_title <- function(on = TRUE, ...) {
  theme(axis.title.y = if (on)
    element_text(...)
    else
      element_blank())
}

#' @rdname bayesplot-convenience
#' @export
#' @param on On/off switch. On if \code{TRUE}.
#'
facet_text <- function(on = TRUE, ...) {
  theme(strip.text = if (on)
    element_text(...)
    else
      element_blank())
}
#' @rdname bayesplot-convenience
#' @export
xaxis_text <- function(on = TRUE, ...) {
  theme(axis.text.x = if (on)
    element_text(...)
    else
      element_blank())
}
#' @rdname bayesplot-convenience
#' @export
yaxis_text <- function(on = TRUE, ...) {
  theme(axis.text.y = if (on)
    element_text(...)
    else
      element_blank())
}


# tick marks --------------------------------------------------------------
#' @rdname bayesplot-convenience
#' @export
xaxis_ticks <- function(on = TRUE, ...) {
  theme(axis.ticks.x = if (on)
    element_line(...)
    else
      element_blank())
}
#' @rdname bayesplot-convenience
#' @export
yaxis_ticks <- function(on = TRUE, ...) {
  theme(axis.ticks.y = if (on)
    element_line(...)
    else
      element_blank())
}
