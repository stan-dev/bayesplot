#' Convenience functions for adding details to plots
#'
#' Convenience functions for adding details to plots (ggplot objects)
#'
#' @name bayesplot-utilities
#'
#' @return
#' \describe{
#' \item{\code{vline_at}, \code{hline_at}}{
#' These functions return an object created by either \code{geom_vline} or
#' \code{geom_hline} and are used to add a vertical or horizontal line (at one
#' or several values) to a ggplot object. If \code{fun} is missing then the
#' lines are drawn at the values in \code{v}. If \code{fun} is specified then
#' the lines are drawn at the values returned by \code{fun(v)}.
#'
#' \code{vline_0} and \code{hline_0} are wrappers for \code{vline_at} and
#' \code{hline_at} with \code{v = 0} and \code{fun} missing.
#' }
#' \item{\code{lbub}}{
#' The \code{lbub} function returns a \emph{function} that takes a single
#' argument \code{x} and returns the lower and upper bounds (\code{lb},
#' \code{ub}) of the \code{100*p}\% central interval of \code{x}, as well as the
#' median (if \code{med} is \code{TRUE}).
#' }
#' }
#'
#'
#' @examples
#' x <- cbind(alpha = rnorm(300),
#'            beta = rnorm(300, 0.5, 2),
#'            sigma = rexp(300))
#'
#' set_color_scheme("gray")
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
#' (p2 <- mcmc_hist(x, pars = "beta"))
#'
#' p2 + vline_at(x[, "beta"], fun = lbub(0.8))
#' p2 + vline_at(x[, "beta"], lbub(0.8, med = FALSE))
#'
#' p2 +
#'  vline_at(
#'    x[, "beta"],
#'    lbub(0.5),
#'    color = "red4",
#'    lty = c(2, 1, 2),
#'    size = .5 * c(1,2,1)
#'  )
#'
NULL


# lines -------------------------------------------------------------------
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
