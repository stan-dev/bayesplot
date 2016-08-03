#' Autocorrelation, Rhat, and effective sample size
#'
#' Plots of Rhat statistics, ratios of effective sample size to total sample
#' size, and autocorrelation of MCMC draws. See the \strong{Plot Descriptions}
#' section, below, for details.
#'
#' @name MCMC-diagnostics
#' @family MCMC
#'
#' @template args-hist
#' @param size An optional value to override \code{\link[ggplot2]{geom_point}}'s
#'   default size (for \code{mcmc_rhat}, \code{mcmc_neff}) or
#'   \code{\link[ggplot2]{geom_line}}'s default size (for \code{mcmc_acf}).
#' @param ... Currently ignored.
#'
#' @template return-ggplot
#'
#' @section Plot Descriptions:
#' \describe{
#' \item{\code{mcmc_rhat, mcmc_rhat_hist}}{
#' Rhat values as either points or a histogram. Values are
#' colored using different shades (lighter is better):
#'  \itemize{
#'    \item \emph{light}: below 1.05 (good)
#'    \item \emph{mid}: between 1.05 and 1.1 (ok)
#'    \item \emph{dark}: above 1.1 (too high)
#'  }
#' }
#' \item{\code{mcmc_neff, mcmc_neff_hist}}{
#' Ratios of effective sample size to total sample size as either points or a
#' histogram. Values are colored using different shades (lighter is better):
#'  \itemize{
#'    \item \emph{light}: between 0.5 and 1 (good)
#'    \item \emph{mid}: between 0.1 and 0.5 (ok)
#'    \item \emph{dark}: below 0.1 (maybe too low)
#'  }
#' }
#' \item{\code{mcmc_acf}}{
#' Grid of autocorrelation plots by chain and parameter. The \code{lags}
#' argument gives the maximum number of lags at which to calculate the
#' autocorrelation function. \code{mcmc_acf} is a line plot whereas
#' \code{mcmc_acf_bar} is a barplot.
#' }
#'}
#'
#' @template seealso-color-scheme
#'
#' @references
#' Gelman, A. and Rubin, D. B. (1992). Inference from iterative
#' simulation using multiple sequences. \emph{Statistical Science}. 7(4),
#' 457--472.
#'
#' @template reference-stan-manual
#'
#' @examples
#' # autocorrelation
#' x <- example_mcmc_draws()
#' dim(x)
#' dimnames(x)
#'
#' set_color_scheme("green")
#' mcmc_acf(x, pars = c("alpha", "beta[1]"))
#' set_color_scheme("pink")
#' (p <- mcmc_acf_bar(x, pars = c("alpha", "beta[1]")))
#'
#' # add tick marks on y axis and horiztonal dashed line at 0.5
#' p +
#'  yaxis_ticks() +
#'  hline_at(0.5, linetype = 2, size = 0.15, color = "gray")
#'
#'
#' # fake rhat values to use for demonstration
#' rhat <- c(runif(100, 1, 1.15))
#' mcmc_rhat_hist(rhat)
#' mcmc_rhat(rhat)
#'
#' set_color_scheme("blue")
#' mcmc_rhat(runif(1000, 1, 1.07))
#' mcmc_rhat(runif(1000, 1, 1.3)) + move_legend("top") # add legend above plot
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
#' fit <- stan_glm(mpg ~ ., data = mtcars, iter = 50)
#' rhats <- rhat(fit)
#' ratios <- neff_ratio(fit)
#' mcmc_rhat(rhats)
#' mcmc_neff(ratios)
#'
#' # there's a small enough number of parameters in the
#' # model that we can display their names on the y-axis
#' mcmc_neff(ratios) + yaxis_text()
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
#' @param rhat A vector of \code{\link[=rhat]{Rhat}} estimates.
#'
mcmc_rhat <- function(rhat, ..., size = NULL) {
  rhat <- validate_rhat(rhat)
  graph <- ggplot(
    data = diagnostic_data_frame(
      x = rhat,
      diagnostic = "rhat"
    ),
    mapping = aes_(
      x = ~ value,
      y = ~ factor_by_name,
      color = ~ factor_by_value,
      fill = ~ factor_by_value
    )
  ) +
    geom_segment(
      mapping = aes_(
        yend = ~ factor_by_name,
        xend = ifelse(min(rhat) < 1, 1, -Inf)
      ),
      na.rm = TRUE
    )

  if (min(rhat) < 1)
    graph <- graph +
      vline_at(1, color = "gray", size = 1)

  brks <- set_rhat_breaks(rhat)
  graph +
    diagnostic_points(size) +
    vline_at(
      brks[-1],
      color = "gray",
      linetype = 2,
      size = 0.25
    ) +
    labs(y = NULL, x = bquote(hat(R))) +
    scale_fill_diagnostic("rhat") +
    scale_color_diagnostic("rhat") +
    theme_default(y_text = FALSE) +
    scale_x_continuous(breaks = brks,
                       expand = c(0,.01))
}

#' @rdname MCMC-diagnostics
#' @export
mcmc_rhat_hist <- function(rhat, ..., binwidth = NULL) {
  ggplot(
    data = diagnostic_data_frame(
      x = validate_rhat(rhat),
      diagnostic = "rhat"
    ),
    mapping = aes_(
      x = ~ value,
      color = ~ factor_by_value,
      fill = ~ factor_by_value
    )
  ) +
    geom_histogram(
      size = .25,
      na.rm = TRUE,
      binwidth = binwidth
    ) +
    scale_color_diagnostic("rhat") +
    scale_fill_diagnostic("rhat") +
    labs(x = bquote(hat(R)), y = NULL) +
    dont_expand_y_axis(c(0.005, 0)) +
    theme_default(y_text = FALSE)
}


# effective sample size ---------------------------------------------------
#' @rdname MCMC-diagnostics
#' @export
#' @param ratio A vector of \emph{ratios} of effective sample size estimates to
#'   total sample size. See \code{\link{neff_ratio}}.
#'
mcmc_neff <- function(ratio, ..., size = NULL) {
  ggplot(
    data = diagnostic_data_frame(
      x = validate_neff_ratio(ratio),
      diagnostic = "neff"
    ),
    mapping = aes_(
      x = ~ value,
      y = ~ factor_by_name,
      color = ~ factor_by_value,
      fill = ~ factor_by_value
    )
  ) +
    geom_segment(
      aes_(yend = ~factor_by_name, xend = -Inf),
      na.rm = TRUE
    ) +
    diagnostic_points(size) +
    vline_at(
      c(0.1, 0.5, 1),
      color = "gray",
      linetype = 2,
      size = 0.25
    ) +
    labs(y = NULL, x = bquote(N[eff]/N)) +
    scale_fill_diagnostic("neff") +
    scale_color_diagnostic("neff") +
    theme_default(y_text = FALSE) +
    # coord_flip() +
    scale_x_continuous(breaks = c(0.1, seq(0, 1, .25)),
                       limits = c(0, 1),
                       expand = c(0,.01))
}

#' @rdname MCMC-diagnostics
#' @export
mcmc_neff_hist <- function(ratio, ..., binwidth = NULL) {
  ggplot(
    data = diagnostic_data_frame(
      x = validate_neff_ratio(ratio),
      diagnostic = "neff"
    ),
    mapping = aes_(
      x = ~ value,
      color = ~ factor_by_value,
      fill = ~ factor_by_value
    )
  ) +
    geom_histogram(
      size = .25,
      na.rm = TRUE,
      binwidth = binwidth
    ) +
    scale_color_diagnostic("neff") +
    scale_fill_diagnostic("neff") +
    labs(x = bquote(N[eff]/N), y = NULL) +
    dont_expand_y_axis(c(0.005, 0)) +
    theme_default(y_text = FALSE)
}


# autocorrelation ---------------------------------------------------------
#' @rdname MCMC-diagnostics
#' @export
#' @template args-mcmc-x
#' @template args-pars
#' @template args-regex_pars
#' @param facet_args Arguments (other than \code{facets}) passed to
#'   \code{\link[ggplot2]{facet_grid}} to control faceting.
#' @param lags The number of lags to show in the autocorrelation plot.
#'
mcmc_acf <-
  function(x,
           pars = character(),
           regex_pars = character(),
           facet_args = list(),
           ...,
           lags = 20,
           size = NULL) {
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
           facet_args = list(),
           ...,
           lags = 20) {
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
diagnostic_points <- function(size = NULL) {
  args <- list(shape = 21, na.rm = TRUE)
  do.call("geom_point", c(args, size = size))
}

# @param x The object returned by validate_rhat or validate_neff_ratio
diagnostic_data_frame <- function(x, diagnostic = c("rhat", "neff")) {
  diagnostic <- match.arg(diagnostic)
  fac <- if (!is.null(names(x))) {
    factor(x, labels = names(sort(x)))
  } else {
    factor(x)
  }

  fun <- match.fun(paste0("factor_", diagnostic))
  d <- data.frame(
    value = x,
    factor_by_name = fac,
    factor_by_value = fun(x)
  )
  rownames(d) <- NULL
  return(d)
}


# set x-axis breaks based on rhat values
set_rhat_breaks <- function(rhat) {
  br <- c(1, 1.05)
  if (any(rhat > 1.05))
    br <- c(br, 1.1)
  for (k in c(1.5, 2)) {
    if (any(rhat > k))
      br <- c(br, k)
  }
  if (max(rhat) >= max(br) + .1)
    br <- c(br, round(max(rhat), 2))

  return(br)
}



# drop NAs from a vector and issue warning
drop_NAs_and_warn <- function(x) {
  if (!anyNA(x))
    return(x)

  is_NA <- is.na(x)
  warning(
    "Dropped ", sum(is_NA), " NAs from '",
    deparse(substitute(x)), "'."
  )
  x[!is_NA]
}

# either throws error or returns an rhat vector (dropping NAs)
validate_rhat <- function(rhat) {
  stopifnot(is_vector_or_1Darray(rhat))
  if (any(rhat < 0, na.rm = TRUE))
    stop("All 'rhat' values must be positive.")

  rhat <- setNames(as.vector(rhat), names(rhat))
  drop_NAs_and_warn(rhat)
}

# either throws error or returns as.vector(ratio)
validate_neff_ratio <- function(ratio) {
  stopifnot(is_vector_or_1Darray(ratio))
  if (any(ratio < 0 | ratio > 1, na.rm = TRUE))
    stop("All elements of 'ratio' must be between 0 and 1.")

  ratio <- setNames(as.vector(ratio), names(ratio))
  drop_NAs_and_warn(ratio)
}


# autocorr plot (either bar or line)
# @param size passed to geom_line if style="line"
.mcmc_acf <-
  function(x,
           pars = character(),
           regex_pars = character(),
           facet_args = list(),
           lags = 25,
           style = c("bar", "line"),
           size = NULL) {

    style <- match.arg(style)
    plot_data <- acf_data(
      x = prepare_mcmc_array(x, pars, regex_pars),
      lags = lags
    )
    if (dim(x)[2] > 1) { # multiple chains
      facet_args$facets <- "Chain ~ Parameter"
      facet_fun <- "facet_grid"
    } else { # 1 chain
      facet_args$facets <- "Parameter"
      facet_fun <- "facet_wrap"
    }

    graph <- ggplot(plot_data, aes_(x = ~ Lag, y = ~ AC))
    if (style == "bar") {
      graph <- graph +
        hline_0(size = 0.5, color = get_color("dh")) +
        geom_bar(
          position = "identity",
          stat = "identity",
          size = 0.2,
          fill = get_color("l"),
          color = get_color("lh"),
          width = 1
        )
    } else {
      graph <- graph +
        hline_0(size = 0.25, color = get_color("lh")) +
        geom_segment(
          aes_(xend = ~ Lag),
          yend = 0,
          color = get_color("l"),
          size = 0.2
        ) +
        do.call(
          "geom_line",
          args = c(list(color = get_color("d")), size = size)
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
      theme_default()
  }

# Prepare data for autocorr plot
# @param x object returned by prepare_mcmc_array
# @param lags user's 'lags' argument
acf_data <- function(x, lags) {
  stopifnot(is_mcmc_array(x))
  dims <- dim(x)
  n_iter <- dims[1]
  n_chain <- dims[2]
  n_param <- dims[3]
  n_lags <- lags + 1
  if (n_lags >= n_iter)
    stop("Too few iterations for lags=", lags, ".",
         call. = FALSE)

  data <- reshape2::melt(x, value.name = "Value")
  data$Chain <- factor(data$Chain)
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

