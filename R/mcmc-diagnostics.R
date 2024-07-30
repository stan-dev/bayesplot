#' General MCMC diagnostics
#'
#' Plots of Rhat statistics, ratios of effective sample size to total sample
#' size, and autocorrelation of MCMC draws. See the **Plot Descriptions**
#' section, below, for details. For models fit using the No-U-Turn-Sampler, see
#' also [MCMC-nuts] for additional MCMC diagnostic plots.
#'
#' @name MCMC-diagnostics
#' @family MCMC
#'
#' @template args-hist
#' @param size Optional values to override [ggplot2::geom_point()]'s
#'   default size (for `mcmc_rhat()`, `mcmc_neff()`) or
#'   [ggplot2::geom_line()]'s default line width (for `mcmc_acf()`).
#' @param ... Currently ignored.
#'
#' @template return-ggplot-or-data
#'
#' @section Plot Descriptions:
#' \describe{
#' \item{`mcmc_rhat()`, `mcmc_rhat_hist()`}{
#'   Rhat values as either points or a histogram. Values are colored using
#'   different shades (lighter is better). The chosen thresholds are somewhat
#'   arbitrary, but can be useful guidelines in practice.
#'   * _light_: below 1.05 (good)
#'   * _mid_: between 1.05 and 1.1 (ok)
#'   * _dark_: above 1.1 (too high)
#' }
#'
#' \item{`mcmc_neff()`, `mcmc_neff_hist()`}{
#'   Ratios of effective sample size to total sample size as either points or a
#'   histogram. Values are colored using different shades (lighter is better).
#'   The chosen thresholds are somewhat arbitrary, but can be useful guidelines
#'   in practice.
#'   * _light_: between 0.5 and 1 (high)
#'   * _mid_: between 0.1 and 0.5 (good)
#'   * _dark_: below 0.1 (low)
#' }
#'
#' \item{`mcmc_acf()`, `mcmc_acf_bar()`}{
#'   Grid of autocorrelation plots by chain and parameter. The `lags` argument
#'   gives the maximum number of lags at which to calculate the autocorrelation
#'   function. `mcmc_acf()` is a line plot whereas `mcmc_acf_bar()` is a
#'   barplot.
#' }
#'}
#'
#' @template reference-stan-manual
#' @references
#' Gelman, A. and Rubin, D. B. (1992). Inference from iterative
#' simulation using multiple sequences. *Statistical Science*. 7(4),
#' 457--472.
#'
#' @seealso
#' * The [Visual MCMC Diagnostics](https://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html)
#'   vignette.
#' * [MCMC-nuts] for additional MCMC diagnostic plots for models fit
#'   using the No-U-Turn-Sampler.
#'
#' @examples
#' # autocorrelation
#' x <- example_mcmc_draws()
#' dim(x)
#' dimnames(x)
#'
#' color_scheme_set("green")
#' mcmc_acf(x, pars = c("alpha", "beta[1]"))
#' \donttest{
#' color_scheme_set("pink")
#' (p <- mcmc_acf_bar(x, pars = c("alpha", "beta[1]")))
#'
#' # add horiztonal dashed line at 0.5
#' p + hline_at(0.5, linetype = 2, size = 0.15, color = "gray")
#' }
#'
#' # fake rhat values to use for demonstration
#' rhat <- c(runif(100, 1, 1.15))
#' mcmc_rhat_hist(rhat)
#' mcmc_rhat(rhat)
#'
#' # lollipops
#' color_scheme_set("purple")
#' mcmc_rhat(rhat[1:10], size = 5)
#'
#' color_scheme_set("blue")
#' mcmc_rhat(runif(1000, 1, 1.07))
#' mcmc_rhat(runif(1000, 1, 1.3)) + legend_move("top") # add legend above plot
#'
#' # fake neff ratio values to use for demonstration
#' ratio <- c(runif(100, 0, 1))
#' mcmc_neff_hist(ratio)
#' mcmc_neff(ratio)
#'
#' \dontrun{
#' # Example using rstanarm model (requires rstanarm package)
#' library(rstanarm)
#'
#' # intentionally use small 'iter' so there are some
#' # problems with rhat and neff for demonstration
#' fit <- stan_glm(mpg ~ ., data = mtcars, iter = 50, refresh = 0)
#' rhats <- rhat(fit)
#' ratios <- neff_ratio(fit)
#' mcmc_rhat(rhats)
#' mcmc_neff(ratios, size = 3)
#'
#' # there's a small enough number of parameters in the
#' # model that we can display their names on the y-axis
#' mcmc_neff(ratios) + yaxis_text(hjust = 1)
#'
#' # can also look at autocorrelation
#' draws <- as.array(fit)
#' mcmc_acf(draws, pars = c("wt", "cyl"), lags = 10)
#'
#' # increase number of iterations and plots look much better
#' fit2 <- update(fit, iter = 500)
#' mcmc_rhat(rhat(fit2))
#' mcmc_neff(neff_ratio(fit2))
#' mcmc_acf(as.array(fit2), pars = c("wt", "cyl"), lags = 10)
#' }
#'
NULL


# Rhat --------------------------------------------------------------------

#' @rdname MCMC-diagnostics
#' @export
#' @param rhat A vector of R-hat estimates.
#'
mcmc_rhat <- function(rhat, ..., size = NULL) {
  check_ignored_arguments(...)
  data <- mcmc_rhat_data(rhat)

  graph <- ggplot(
    data = data,
    mapping = aes(
      x = .data$value,
      y = .data$parameter,
      color = .data$rating,
      fill = .data$rating)) +
    geom_segment(
      mapping = aes(
        yend = .data$parameter,
        xend = ifelse(min(.data$value) < 1, 1, -Inf)),
      na.rm = TRUE,
      show.legend = TRUE) +
      bayesplot_theme_get()

  if (min(data$value) < 1) {
    graph <- graph +
      vline_at(1, color = "gray", linewidth = 1)
  }

  brks <- set_rhat_breaks(data$value)

  graph +
    diagnostic_points(size) +
    vline_at(
      brks[-1],
      color = "gray",
      linetype = 2,
      linewidth = 0.25) +
    labs(y = NULL, x = expression(hat(R))) +
    scale_fill_diagnostic("rhat") +
    scale_color_diagnostic("rhat") +
    scale_x_continuous(breaks = brks, expand = c(0, .01)) +
    scale_y_discrete(expand = c(.025,0)) +
    yaxis_title(FALSE) +
    yaxis_text(FALSE) +
    yaxis_ticks(FALSE)
}

#' @rdname MCMC-diagnostics
#' @export
mcmc_rhat_hist <- function(rhat, ..., binwidth = NULL, bins = NULL, breaks = NULL) {
  check_ignored_arguments(...)
  data <- mcmc_rhat_data(rhat)

  ggplot(
    data = data,
    mapping = aes(
      x = .data$value,
      color = .data$rating,
      fill = .data$rating)) +
    geom_histogram(
      linewidth = 0.25,
      na.rm = TRUE,
      binwidth = binwidth,
      bins = bins,
      breaks = breaks
    ) +
    scale_color_diagnostic("rhat") +
    scale_fill_diagnostic("rhat") +
    labs(x = expression(hat(R)), y = NULL) +
    dont_expand_y_axis(c(0.005, 0)) +
    bayesplot_theme_get() +
    yaxis_title(FALSE) +
    yaxis_text(FALSE) +
    yaxis_ticks(FALSE)
}

#' @rdname MCMC-diagnostics
#' @export
mcmc_rhat_data <- function(rhat, ...) {
  check_ignored_arguments(...)
  rhat <- drop_NAs_and_warn(new_rhat(rhat))
  diagnostic_data_frame(rhat)
}


# effective sample size ---------------------------------------------------

#' @rdname MCMC-diagnostics
#' @export
#' @param ratio A vector of *ratios* of effective sample size estimates to
#'   total sample size. See [neff_ratio()].
#'
mcmc_neff <- function(ratio, ..., size = NULL) {
  check_ignored_arguments(...)
  data <- mcmc_neff_data(ratio)

  max_ratio <- max(ratio, na.rm = TRUE)
  if (max_ratio < 1.25) {
    additional_breaks <- numeric(0)
  } else if (max_ratio < 1.5) {
    additional_breaks <- 1.25
    additional_labels <- "1.25"
  } else {
    additional_breaks <- seq(1.5, max_ratio, by = 0.5)
  }
  breaks <- c(0, 0.1, 0.25, 0.5, 0.75, 1, additional_breaks)

  ggplot(
    data,
    mapping = aes(
      x = .data$value,
      y = .data$parameter,
      color = .data$rating,
      fill = .data$rating)) +
    geom_segment(
      aes(yend = .data$parameter, xend = -Inf),
      na.rm = TRUE,
      show.legend = TRUE) +
    diagnostic_points(size) +
    vline_at(
      c(0.1, 0.5, 1),
      color = "gray",
      linetype = 2,
      linewidth = 0.25) +
    labs(y = NULL, x = expression(N[eff]/N)) +
    scale_fill_diagnostic("neff") +
    scale_color_diagnostic("neff") +
    scale_x_continuous(
      breaks = breaks,
      # as.character truncates trailing zeroes, while ggplot default does not
      labels = as.character(breaks),
      limits = c(0, max(1, max_ratio) + 0.05),
      expand = c(0, 0)) +
    bayesplot_theme_get() +
    yaxis_text(FALSE) +
    yaxis_title(FALSE) +
    yaxis_ticks(FALSE)
}

#' @rdname MCMC-diagnostics
#' @export
mcmc_neff_hist <- function(ratio, ..., binwidth = NULL, bins = NULL, breaks = NULL) {
  check_ignored_arguments(...)
  data <- mcmc_neff_data(ratio)

  ggplot(
    data,
    mapping = aes(
      x = .data$value,
      color = .data$rating,
      fill = .data$rating)) +
    geom_histogram(
      linewidth = 0.25,
      na.rm = TRUE,
      binwidth = binwidth,
      bins = bins,
      breaks = breaks) +
    scale_color_diagnostic("neff") +
    scale_fill_diagnostic("neff") +
    labs(x = expression(N[eff]/N), y = NULL) +
    dont_expand_y_axis(c(0.005, 0)) +
    yaxis_title(FALSE) +
    yaxis_text(FALSE) +
    yaxis_ticks(FALSE) +
    bayesplot_theme_get()
}

#' @rdname MCMC-diagnostics
#' @export
mcmc_neff_data <- function(ratio, ...) {
  check_ignored_arguments(...)
  ratio <- drop_NAs_and_warn(new_neff_ratio(ratio))
  diagnostic_data_frame(ratio)
}


# autocorrelation ---------------------------------------------------------

#' @rdname MCMC-diagnostics
#' @export
#' @template args-mcmc-x
#' @template args-pars
#' @template args-regex_pars
#' @template args-facet_args
#' @param lags The number of lags to show in the autocorrelation plot.
mcmc_acf <-
  function(x,
           pars = character(),
           regex_pars = character(),
           ...,
           facet_args = list(),
           lags = 20,
           size = NULL) {
    check_ignored_arguments(...)
    .mcmc_acf(
      x,
      pars = pars,
      regex_pars = regex_pars,
      facet_args = facet_args,
      lags = lags,
      size = size,
      style = "line"
    )
  }

#' @rdname MCMC-diagnostics
#' @export
mcmc_acf_bar <-
  function(x,
           pars = character(),
           regex_pars = character(),
           ...,
           facet_args = list(),
           lags = 20) {
    check_ignored_arguments(...)
    .mcmc_acf(
      x,
      pars = pars,
      regex_pars = regex_pars,
      facet_args = facet_args,
      lags = lags,
      style = "bar"
    )
  }




# internal ----------------------------------------------------------------


#' Convert numeric vector of diagnostic values to a factor
#'
#' @param x A numeric vector.
#' @param breaks A numeric vector of length two. The resulting factor variable
#'   will have three levels ('low', 'ok', and 'high') corresponding to (
#'   `x <= breaks[1]`, `breaks[1] < x <= breaks[2]`, `x > breaks[2]`).
#' @return A factor the same length as `x` with three levels.
#' @noRd
diagnostic_factor <- function(x, ...) {
  UseMethod("diagnostic_factor")
}

#' @export
diagnostic_factor.rhat <- function(x, ..., breaks = c(1.05, 1.1)) {
  cut(x, breaks = c(-Inf, breaks, Inf),
      labels = c("low", "ok", "high"),
      ordered_result = FALSE)
}

#' @export
diagnostic_factor.neff_ratio <- function(x, ..., breaks = c(0.1, 0.5)) {
  cut(x, breaks = c(-Inf, breaks, Inf),
      labels = c("low", "ok", "high"),
      ordered_result = FALSE)
}

diagnostic_data_frame <- function(x) {
  x <- auto_name(sort(x))
  stopifnot(!anyDuplicated(names(x)))
  diagnostic <- class(x)[1]

  d <- tibble::tibble(
    diagnostic = diagnostic,
    parameter = factor(seq_along(x), labels = names(x)),
    value = as.numeric(x),
    rating = diagnostic_factor(x))

  labels <- diagnostic_color_labels[[diagnostic]]
  d$description <- as.character(labels[d$rating])
  d
}

auto_name <- function(xs) {
  if (is.null(names(xs))) {
    names(xs) <- zero_pad_int(seq_along(xs))
  }
  xs
}

# c(1, 2, 10, 20, 100) => c("001", "002", "010", "020", "100")
zero_pad_int <- function(xs) {
  formatter <- paste0("%0", max(nchar(xs)), "d")
  sprintf(formatter, xs)
}

diagnostic_points <- function(size = NULL) {
  args <- list(shape = 21, na.rm = TRUE, show.legend = TRUE)
  do.call("geom_point", c(args, size = size))
}


# Functions wrapping around scale_color_manual() and scale_fill_manual(), used to
# color the intervals by rhat value
scale_color_diagnostic <- function(diagnostic = c("rhat", "neff")) {
  d <- match.arg(diagnostic)
  diagnostic_color_scale(d, aesthetic = "color")
}

scale_fill_diagnostic <- function(diagnostic = c("rhat", "neff")) {
  d <- match.arg(diagnostic)
  diagnostic_color_scale(d, aesthetic = "fill")
}

diagnostic_color_scale <- function(diagnostic = c("rhat", "neff_ratio"),
                                   aesthetic = c("color", "fill")) {
  diagnostic <- match.arg(diagnostic)
  aesthetic <- match.arg(aesthetic)
  dc <- diagnostic_colors(diagnostic, aesthetic)
  do.call(
    match.fun(paste0("scale_", aesthetic, "_manual")),
    list(
      name = NULL,
      drop = FALSE,
      values = dc$values,
      labels = dc$color_labels
    )
  )
}

diagnostic_colors <- function(diagnostic = c("rhat", "neff_ratio"),
                              aesthetic = c("color", "fill")) {
  diagnostic <- match.arg(diagnostic)
  aesthetic <- match.arg(aesthetic)
  color_levels <- c("light", "mid", "dark")
  if (diagnostic == "neff_ratio") {
    color_levels <- rev(color_levels)
  }
  if (aesthetic == "color") {
    color_levels <- paste0(color_levels, "_highlight")
  }

  color_labels <- diagnostic_color_labels[[diagnostic]]
  list(diagnostic = diagnostic,
       aesthetic = aesthetic,
       color_levels = color_levels,
       color_labels = color_labels,
       values = set_names(get_color(color_levels), c("low", "ok", "high")))
}

diagnostic_color_labels <- list(
  rhat = c(
    low  = expression(hat(R) <= 1.05),
    ok   = expression(hat(R) <= 1.10),
    high = expression(hat(R) > 1.10)
  ),
  neff_ratio = c(
    low  = expression(N[eff] / N <= 0.1),
    ok   = expression(N[eff] / N <= 0.5),
    high = expression(N[eff] / N > 0.5)
  )
)

# set x-axis breaks based on rhat values
set_rhat_breaks <- function(rhat) {
  br <- c(1, 1.05)
  if (any(rhat > 1.05)) {
    br <- c(br, 1.1)
  }

  for (k in c(1.5, 2)) {
    if (any(rhat > k)) {
      br <- c(br, k)
    }
  }
  if (max(rhat) >= max(br) + .1) {
    br <- c(br, round(max(rhat), 2))
  }
  br
}

# drop NAs from a vector and issue warning
drop_NAs_and_warn <- function(x) {
  is_NA <- is.na(x)
  if (anyNA(x)) {
    warn(paste0(
      "Dropped ", sum(is_NA), " NAs from '",
      deparse(substitute(x)), "'."
    ))
  }
  x[!is_NA]
}

# Autocorrelation plot (either bar or line)
# @param size passed to geom_line() if style="line"
.mcmc_acf <-
  function(x,
           pars = character(),
           regex_pars = character(),
           facet_args = list(),
           lags = 25,
           style = c("bar", "line"),
           size = NULL) {

    style <- match.arg(style)
    x <- prepare_mcmc_array(x, pars, regex_pars)
    plot_data <- acf_data(x = x, lags = lags)

    if (num_chains(x) > 1) {
      facet_args$rows <- vars(.data$Chain)
      facet_args$cols <- vars(.data$Parameter)
      facet_fun <- "facet_grid"
    } else { # 1 chain
      facet_args$facets <- "Parameter"
      facet_fun <- "facet_wrap"
    }

    graph <- ggplot(plot_data, aes(x = .data$Lag, y = .data$AC))  +
      bayesplot_theme_get()
    if (style == "bar") {
      graph <- graph +
        geom_bar(
          position = "identity",
          stat = "identity",
          linewidth = 0.2,
          fill = get_color("l"),
          color = get_color("lh"),
          width = 1
        ) +
        hline_0(linewidth = 0.25, color = get_color("dh"))
    } else {
      graph <- graph +
        hline_0(linewidth = 0.25, color = get_color("m")) +
        geom_segment(
          aes(xend = .data$Lag),
          yend = 0,
          color = get_color("l"),
          linewidth = 0.2
        ) +
        do.call(
          "geom_line",
          args = c(list(color = get_color("d")), linewidth = size)
        )
    }

    graph +
      do.call(facet_fun, facet_args) +
      scale_y_continuous(
        limits = c(min(0, plot_data$AC), 1.05),
        breaks = c(0, 0.5, 1)
      ) +
      scale_x_continuous(
        limits = c(-0.5, lags + 0.5),
        breaks = function(x) as.integer(pretty(x, n = 3)),
        expand = c(0, 0)
      ) +
      labs(x = "Lag", y = "Autocorrelation") +
      force_axes_in_facets()
  }

# Prepare data for autocorr plot
# @param x object returned by prepare_mcmc_array
# @param lags user's 'lags' argument
acf_data <- function(x, lags) {
  stopifnot(is_mcmc_array(x))
  n_iter <- num_iters(x)
  n_chain <- num_chains(x)
  n_param <- num_params(x)
  n_lags <- lags + 1
  if (n_lags >= n_iter) {
    abort(paste0("Too few iterations for lags=", lags, "."))
  }

  data <- melt_mcmc(x)
  ac_list <- tapply(
    data[["Value"]],
    # INDEX = list(data[["Chain"]], data[["Parameter"]]),
    INDEX = with(data, list(Chain, Parameter)),
    FUN = function(x, lag.max) {
      stats::acf(x, lag.max = lag.max, plot = FALSE)$acf[, , 1]
    },
    lag.max = lags,
    simplify = FALSE
  )

  data.frame(
    Chain = rep(rep(1:n_chain, each = n_lags), times = n_param),
    Parameter = factor(rep(1:n_param, each = n_chain * n_lags),
                       labels = levels(data[["Parameter"]])),
    Lag = rep(seq(0, lags), times = n_chain * n_param),
    AC = do.call("c", ac_list)
  )
}




## interal [classes / objects] ------------------------------------------------

new_rhat <- function(x) {
  # Convert a 1-d arrays to a vectors
  if (is.array(x) && length(dim(x)) == 1) {
    x <- as.vector(x)
  }
  validate_rhat(as_rhat(x))
}

validate_rhat <- function(x) {
  stopifnot(is.numeric(x), !is.list(x), !is.array(x))
  if (any(x < 0, na.rm = TRUE)) {
    abort("All 'rhat' values must be positive.")
  }
  x
}

as_rhat <- function(x) {
  structure(x, class = c("rhat", "numeric"), names = names(x))
}

#' Indexing method -- needed so that sort, etc. don't strip names.
#' @export
#' @keywords internal
#' @noRd
`[.rhat` <- function (x, i, j, drop = TRUE, ...) {
  as_rhat(NextMethod())
}

new_neff_ratio <- function(x) {
  # Convert a 1-d arrays to a vectors
  if (is.array(x) && length(dim(x)) == 1) {
    x <- as.vector(x)
  }
  as_neff_ratio(validate_neff_ratio(x))
}

validate_neff_ratio <- function(x) {
  stopifnot(is.numeric(x), !is.list(x), !is.array(x))
  if (any(x < 0, na.rm = TRUE)) {
    abort("All neff ratios must be positive.")
  }
  x
}

as_neff_ratio <- function(x) {
  structure(x, class = c("neff_ratio", "numeric"), names = names(x))
}

#' Indexing method -- needed so that sort, etc. don't strip names.
#' @export
#' @keywords internal
#' @noRd
`[.neff_ratio` <- function (x, i, j, drop = TRUE, ...) {
  as_neff_ratio(NextMethod())
}

