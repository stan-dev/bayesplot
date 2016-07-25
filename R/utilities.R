#' Convenience functions for adding or changing plot details
#'
#' Convenience functions for adding or changing details of plots that are ggplot
#' objects.
#'
#' @name bayesplot-utilities
#'
#' @param ... For the \code{vline_} and \code{hline_} functions, \code{...}
#'   is passed to \code{\link[ggplot2]{geom_vline}} or
#'   \code{\link[ggplot2]{geom_hline}} to control the appearance of the line(s).
#'
#'   For \code{facet_text}, \code{xaxis_text} and \code{yaxis_text}, \code{...}
#'   is passed to \code{\link[ggplot2]{element_text}} if \code{on = TRUE}.
#'
#'   For \code{xaxis_ticks} and \code{yaxis_ticks}, \code{...} is passed to
#'   \code{\link[ggplot2]{element_line}} if \code{on = TRUE}.
#'
#' @return
#' \describe{
#' \item{\code{vline_at}, \code{hline_at}}{
#' \code{vline_at} and \code{hline_at} return an object created by either
#' \code{geom_vline} or \code{geom_hline} that can be added to a ggplot object
#' to draw a vertical or horizontal line (at one or several values). If
#' \code{fun} is missing then the lines are drawn at the values in \code{v}. If
#' \code{fun} is specified then the lines are drawn at the values returned by
#' \code{fun(v)}.
#'
#' \code{vline_0} and \code{hline_0} are wrappers for \code{vline_at} and
#' \code{hline_at} with \code{v = 0} and \code{fun} missing.
#' }
#' \item{\code{lbub}}{
#' \code{lbub} returns a \emph{function} that takes a single
#' argument \code{x} and returns the lower and upper bounds (\code{lb},
#' \code{ub}) of the \code{100*p}\% central interval of \code{x}, as well as the
#' median (if \code{med} is \code{TRUE}).
#' }
#' \item{\code{move_legend}, \code{no_legend}}{
#' \code{move_legend} and \code{no_legend} return a ggplot2 theme object that
#' can be added to an existing plot (ggplot object) in order to change the
#' position of the legend (\code{move_legend}) or remove the legend
#' (\code{no_legend}).
#' }
#' \item{\code{facet_text}}{
#' \code{facet_text} returns a ggplot2 theme object that can be added to an
#' existing plot (ggplot object) to format the text in facet labels.
#' }
#' \item{\code{xaxis_text}, \code{yaxis_text}}{
#' \code{xaxis_text} and \code{yaxis_text} return a ggplot2 theme object that
#' can be added to an existing plot (ggplot object) in order to toggle or format
#' the text displayed on the \code{x} or \code{y} axis. These functions do not
#' affect the axis \emph{titles}.
#' }
#' \item{\code{xaxis_ticks}, \code{yaxis_ticks}}{
#' \code{xaxis_ticks} and \code{yaxis_ticks} return a ggplot2
#' theme object that can be added to an existing plot (ggplot object) to change
#' the appearance of the axis tick marks.
#' }
#' }
#'
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
#' p + vline_0(size = 0.15, color = "maroon")
#'
#'
#' ### vertical line(s) at specified values
#' p + vline_at(c(-1, 0, 1), linetype = 3)
#'
#' my_lines <- vline_at(
#'   v = c(-1, 0, 1),
#'   color = c("maroon", "skyblue", "violet"),
#'   size = 0.75 * c(1, 2, 1),
#'   alpha = 0.25
#' )
#' p + my_lines
#'
#' ### vertical line(s) at computed values
#' p + vline_at(x, colMeans)
#' p + vline_at(x, "colMeans",
#'              lty = 2, color = "seagreen", size = 0.25)
#' p + vline_at(x, function(a) apply(a, 2, mean),
#'              color = rainbow(ncol(x)), size = 0.25)
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
#'    color = "red4",
#'    lty = c(2, 1, 2),
#'    size = .5 * c(1,2,1)
#'  )
#'
#' ### control appearance facet and axis text
#' p2 + facet_text(face = "bold", color = "gray50", size = 14)
#' p2 + xaxis_text(FALSE)
#' p2 + xaxis_text(size = 14)
#'
#' ### control size of axis tick marks
#' p2 + xaxis_ticks(.25)
#'
NULL


# lines -------------------------------------------------------------------
#' @rdname bayesplot-utilities
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

#' @rdname bayesplot-utilities
#' @export
hline_at <- function(v, fun, ...) {
  geom_hline(yintercept = calc_v(v, fun),
             ...)
}

#' @rdname bayesplot-utilities
#' @export
vline_0 <- function(...) {
  geom_vline(xintercept = 0, ...)
}

#' @rdname bayesplot-utilities
#' @export
#'
hline_0 <- function(...) {
  geom_hline(yintercept = 0, ...)
}



# intervals ---------------------------------------------------------------
#' @rdname bayesplot-utilities
#' @export
#' @param p The probability mass (in [0,1]) to include in the interval.
#' @param med Should the median also be included in addition to the lower
#' and upper bounds of the interval?
#'
lbub <- function(p, med = TRUE) {
  function(x) calc_intervals(x, p, med = med)
}

# internal ---------------------------------------------------------------
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
#' @rdname bayesplot-utilities
#' @export
no_legend <- function() {
  theme(legend.position = "none")
}

#' @rdname bayesplot-utilities
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
#' @rdname bayesplot-utilities
#' @export
#' @param on On/off switch. On if \code{TRUE}.
#'
facet_text <- function(on = TRUE, ...) {
  theme(strip.text = if (on)
    element_text(...)
    else
      element_blank())
}
#' @rdname bayesplot-utilities
#' @export
xaxis_text <- function(on = TRUE, ...) {
  theme(axis.text.x = if (on)
    element_text(...)
    else
      element_blank())
}
#' @rdname bayesplot-utilities
#' @export
yaxis_text <- function(on = TRUE, ...) {
  theme(axis.text.y = if (on)
    element_text(...)
    else
      element_blank())
}


# tick marks --------------------------------------------------------------
#' @rdname bayesplot-utilities
#' @export
xaxis_ticks <- function(on = TRUE, ...) {
  theme(axis.ticks.x = if (on)
    element_line(...)
    else
      element_blank())
}
#' @rdname bayesplot-utilities
#' @export
yaxis_ticks <- function(on = TRUE, ...) {
  theme(axis.ticks.y = if (on)
    element_line(...)
    else
      element_blank())
}
