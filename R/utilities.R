#' Convenience functions for adding details to plots
#'
#' Convenience functions for adding details to plots (ggplot objects)
#'
#' @name bayesplot-utilities
#'
#' @details
#' \describe{
#' \item{\code{vline_at}, \code{hline_at}}{
#' Add a vertical or horizontal line at one or several value(s) to a ggplot
#' object.
#' }
#' \item{\code{vline0}, \code{hline0}}{
#' Add a vertical or horizontal line at zero to a ggplot object. These are
#' wrappers for \code{vline_at} and \code{hline_at} with \code{v = 0} and
#' \code{fun} missing.
#' }
#' }
#'
#' @examples
#' y <- rnorm(100)
#' yrep <- matrix(rnorm(10000), ncol = 100)
#' group <- gl(4, 25, labels = LETTERS[1:4])
#'
#' x <- cbind(alpha = rnorm(100),
#'            beta = rnorm(100, 0.5, 2),
#'            sigma = rexp(100))
#'
#' set_color_scheme("greys")
#' (p <- mcmc_intervals(x))
#'
#' # vertical line at 0
#' p + vline0()
#' p + vline0(size = 0.15, color = "maroon")
#'
#'
#' # vertical line(s) at specified values
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
#' # vertical line(s) at computed values
#' p + vline_at(x, colMeans)
#' p + vline_at(x, "colMeans",
#'              lty = 2, color = "seagreen", size = 0.25)
#' p + vline_at(x, function(x) apply(x, 2, mean),
#'              color = rainbow(ncol(x)), size = 0.25)
#'
NULL

#' @rdname bayesplot-utilities
#' @export
#' @param v Either a numeric vector specifying the value(s) at which to draw the
#'   vertical or horizontal line(s), or an object of any type to use as the
#'   first argument to \code{fun}.
#' @param fun A function, or the name of a function, that returns a numeric
#'   vector.
#' @param ... Arguments passed to \code{\link[ggplot2]{geom_vline}} or
#'   \code{\link[ggplot2]{geom_hline}} to control the appearance of the line(s).
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
vline0 <- function(...) {
  geom_vline(xintercept = 0, ...)
}

#' @rdname bayesplot-utilities
#' @export
#'
hline0 <- function(...) {
  geom_hline(yintercept = 0, ...)
}


calc_v <- function(v, fun, ...) {
  if (missing(fun))
    return(v)
  f <- match.fun(fun)
  f(v, ...)
}
