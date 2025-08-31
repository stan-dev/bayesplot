#' Convenience functions for adding or changing plot details
#'
#' Convenience functions for adding to (and changing details of) ggplot objects
#' (many of the objects returned by **bayesplot** functions). See the
#' **Examples** section, below.
#'
#' @name bayesplot-helpers
#'
#' @param ... For the various `vline_`, `hline_`, and `abline_`
#'   functions, `...` is passed to [ggplot2::geom_vline()],
#'   [ggplot2::geom_hline()], and [ggplot2::geom_abline()],
#'   respectively, to control the appearance of the line(s).
#'
#'   For functions ending in `_bg`, `...` is passed to
#'   [ggplot2::element_rect()].
#'
#'   For functions ending in `_text` or `_title`, `...` is passed
#'   to [ggplot2::element_text()].
#'
#'   For `xaxis_ticks` and `yaxis_ticks`, `...` is passed to
#'   [ggplot2::element_line()].
#'
#'   For `overlay_function`, `...` is passed to
#'   [ggplot2::stat_function()].
#'
#' @return
#' A **ggplot2** layer or [ggplot2::theme()] object that can be
#' added to existing ggplot objects, like those created by many of the
#' **bayesplot** plotting functions. See the **Details** section.
#'
#' @details
#' \subsection{Add vertical, horizontal, and diagonal lines to plots}{
#' * `vline_at()` and `hline_at()` return an object created by either
#'   [ggplot2::geom_vline()] or [ggplot2::geom_hline()] that can be added to a
#'   ggplot object to draw a vertical or horizontal line (at one or several
#'   values). If `fun` is missing then the lines are drawn at the values in `v`.
#'   If `fun` is specified then the lines are drawn at the values returned by `fun(v)`.
#'
#' * `vline_0()` and `hline_0()` are wrappers for `vline_at()` and `hline_at()`
#'   with `v = 0` and `fun` missing.
#'
#' * `abline_01()` is a wrapper for [ggplot2::geom_abline()] with the intercept
#'   set to `0` and the slope set to `1`.
#'
#' * `lbub()` returns a _function_ that takes a single argument `x` and returns
#'   the lower and upper bounds (`lb`, `ub`) of the `100*p`\% central interval
#'   of `x`, as well as the median (if `med=TRUE`).
#' }
#'
#' \subsection{Control appearance of facet strips}{
#' * `facet_text()` returns ggplot2 theme objects that can be added to an
#'    existing plot (ggplot object) to format the text in facet strips.
#'
#' * `facet_bg()` can be added to a plot to change the background of the facet strips.
#' }
#'
#' \subsection{Move legend, remove legend, or style the legend text}{
#' * `legend_move()` and `legend_none()` return a ggplot2 theme object that can
#'   be added to an existing plot (ggplot object) in order to change the
#'   position of the legend or remove it.
#'
#' * `legend_text()` works much like `facet_text()` but for the legend.
#' }
#'
#' \subsection{Control appearance of \eqn{x}-axis and \eqn{y}-axis features}{
#' * `xaxis_title()` and `yaxis_title()` return a ggplot2 theme object
#'   that can be added to an existing plot (ggplot object) in order to toggle or
#'   format the titles displayed on the `x` or `y` axis. (To change
#'   the titles themselves use [ggplot2::labs()].)
#'
#' * `xaxis_text()` and `yaxis_text()` return a ggplot2 theme object
#'   that can be added to an existing plot (ggplot object) in order to toggle or
#'   format the text displayed on the `x` or `y` axis (e.g. tick
#'   labels).
#'
#' * `xaxis_ticks()` and `yaxis_ticks()` return a ggplot2 theme object
#'   that can be added to an existing plot (ggplot object) to change the
#'   appearance of the axis tick marks.
#' }
#'
#' \subsection{Customize plot background}{
#' * `plot_bg()` returns a ggplot2 theme object that can be added to an
#'   existing plot (ggplot object) to format the background of the *entire* plot.
#'
#' * `panel_bg()` returns a ggplot2 theme object that can be added to an
#'   existing plot (ggplot object) to format the background of the just the
#'   plotting area.
#'
#' * `grid_lines()` returns a ggplot2 theme object that can be added to
#'   an existing plot (ggplot object) to add grid lines to the plot background.
#' }
#'
#' \subsection{Superimpose a function on an existing plot}{
#' * `overlay_function()` is a simple wrapper for [ggplot2::stat_function()] but
#'   with the `inherit.aes` argument fixed to `FALSE`. Fixing `inherit.aes=FALSE`
#'   will avoid potential errors due to the [ggplot2::aes()]thetic mapping used by
#'   certain **bayesplot** plotting functions.
#' }
#'
#' @seealso [theme_default()] for the default ggplot theme used by
#'   **bayesplot**.
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
#' p + vline_0(linewidth = 0.25, color = "darkgray", linetype = 2)
#'
#' # vertical line(s) at specified values
#' v <- c(-0.5, 0, 0.5)
#' p + vline_at(v, linetype = 3, linewidth = 0.25)
#'
#' my_lines <- vline_at(v, alpha = 0.25, linewidth = 0.75 * c(1, 2, 1),
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
#'              lty = 2, linewidth = 0.25)
#' p + vline_at(x[, 3:4], function(a) apply(a, 2, mean),
#'              color = "orange",
#'              linewidth = 2, alpha = 0.1)
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
#'               linewidth = 2 * c(1,.5,1), alpha = 0.75)
#' p2 + vline_at(b1, lbub(0.8, med = FALSE), color = "gray20",
#'               linewidth = 2, alpha = 0.75)
#'
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
#'  xaxis_ticks(linewidth = 1, color = "skyblue")
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
#'  panel_bg(color = "black", fill = "gray99", linewidth = 3)
#' }
#'
#'
#' ###############################################
#' ### superimpose a function on existing plot ###
#' ###############################################
#' # compare posterior of beta[1] to Gaussian with same posterior mean
#' # and sd as beta[1]
#' x <- example_mcmc_draws(chains = 4)
#' dim(x)
#' purple_gaussian <-
#'   overlay_function(
#'     fun = dnorm,
#'     args = list(mean(x[,, "beta[1]"]), sd(x[,, "beta[1]"])),
#'     color = "purple",
#'     linewidth = 2
#'   )
#'
#' color_scheme_set("gray")
#' mcmc_hist(x, pars = "beta[1]", freq = FALSE) + purple_gaussian
#' \donttest{
#' mcmc_dens(x, pars = "beta[1]") + purple_gaussian
#' }
#'
NULL


# lines -------------------------------------------------------------------
#' @rdname bayesplot-helpers
#' @export
#' @param v Either a numeric vector specifying the value(s) at which to
#'   draw the vertical or horizontal line(s), or an object of any type to use as
#'   the first argument to `fun`.
#' @param fun A function, or the name of a function, that returns a numeric
#'   vector.
#' @param na.rm A logical scalar passed to the appropriate geom (e.g.
#'   [ggplot2::geom_vline()]). The default is `TRUE`.
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

#' @rdname bayesplot-helpers
#' @export
#'
abline_01 <- function(..., na.rm = TRUE) {
  geom_abline(intercept = 0,
              slope = 1,
              na.rm = na.rm,
              ...)
}


# intervals ---------------------------------------------------------------
#' @rdname bayesplot-helpers
#' @export
#' @param p The probability mass (in `[0,1]`) to include in the interval.
#' @param med Should the median also be included in addition to the lower
#' and upper bounds of the interval?
#'
lbub <- function(p, med = TRUE) {
  function(x) calc_intervals(x, p, med = med)
}

# internal
calc_v <- function(v, fun, fun_args, ...) {
  if (missing(v)) {
    abort("'v' can't be missing.")
  }
  if (missing(fun)) {
    return(v)
  }
  f <- match.fun(fun)
  if (missing(fun_args)) {
    return(f(v))
  }
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
#'   or a string among `"right"`, `"left"`, `"top"`,
#'   `"bottom"`. Using `position = "none"` is also allowed and is
#'   equivalent to using `legend_none()`.
#'
legend_move <- function(position = "right") {
  if (is.numeric(position) && "legend.position.inside" %in% fn_fmls_names(theme)) {
    theme(legend.position = "inside", legend.position.inside = position)
  } else {
    theme(legend.position = position)
  }
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
  if (!on) {
    return(xlab(NULL))
  }
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
#' @param on For functions modifying ggplot [theme][ggplot2::theme] elements,
#'   set `on=FALSE` to set the element to [ggplot2::element_blank()]. For
#'   example, facet text can be removed by adding `facet_text(on=FALSE)`, or
#'   simply `facet_text(FALSE)` to a ggplot object. If `on=TRUE` (the default),
#'   then `...` can be used to customize the appearance of the theme element.
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
#' @param color,size Passed to [ggplot2::element_line()].
#'
grid_lines <- function(color = "gray50", size = 0.2) {
  theme(
    panel.grid.major = element_line(color = color, linewidth = size),
    panel.grid.minor = element_line(color = color, linewidth = size * 0.5)
  )
}

grid_lines_y <- function(color = "gray50", size = 0.2) {
  theme(
    panel.grid.major.y = element_line(color = color, linewidth = size),
    panel.grid.minor.y = element_line(color = color, linewidth = size * 0.5)
  )
}


# overlay functions on an existing plot -----------------------------------
#' @rdname bayesplot-helpers
#' @export
overlay_function <- function(...) {
  stat_function(..., inherit.aes = FALSE)
}



# Resolve a function name and store the expression passed in by the user
#' @noRd
#' @param f a function-like thing: a string naming a function, a function
#'   object, an anonymous function object, a formula-based lambda, and `NULL`.
#' @param fallback character string providing a fallback function name
#' @return the function named in `f` with an added `"tagged_expr"` attribute
#' containing the expression to represent the function name and an
#' `"is_anonymous_function"` attribute to flag if the expression is a call to
#' `function()`.
as_tagged_function <- function(f = NULL, fallback = "func") {
  qf <- enquo(f)
  f <- eval_tidy(qf)
  if (!is.null(attr(f, "tagged_expr"))) return(f)

  f_expr <- quo_get_expr(qf)
  f_fn <- f

  if (is_character(f)) {        # f = "mean"
    # using sym() on the evaluated `f` means that a variable that names a
    # function string `x <- "mean"; as_tagged_function(x)` will be lost
    # but that seems okay
    f_expr <- sym(f)
    f_fn <- match.fun(f)
  } else if (is_null(f)) {      # f = NULL
    f_fn <- identity
    f_expr <- sym(fallback)
  } else if (is_callable(f)) {  # f = mean or f = function(x) mean(x)
    f_expr <- f_expr            # or f = ~mean(.x)
    f_fn <- as_function(f)
  }

  # Setting attributes on primitive functions is deprecated, so wrap them
  # and then tag
  if (is_primitive(f_fn)) {
    f_fn_old <- f_fn
    f_factory <- function(f) { function(...) f(...) }
    f_fn <- f_factory(f_fn_old)
  }

  attr(f_fn, "tagged_expr") <- f_expr
  attr(f_fn, "is_anonymous_function") <-
    is_call(f_expr, name = "function") || is_formula(f_expr)
  f_fn
}



