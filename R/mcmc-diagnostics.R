#' Rhat and effective sample size
#'
#' Plots of Rhat and ratio of effective sample size to total sample size. See
#' the \strong{Plot Descriptions} section, below, for details.
#'
#' @name MCMC-diagnostics
#' @family MCMC
#'
#' @template args-hist
#' @param size An optional value to override \code{\link[ggplot2]{geom_point}}'s
#'   default size.
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
#' # fake rhat values to use for demonstration
#' rhat <- c(runif(100, 1, 1.15))
#' mcmc_rhat_hist(rhat)
#' mcmc_rhat(rhat)
#'
#' set_color_scheme("blue")
#' mcmc_rhat(runif(1000, 1, 1.3))
#' mcmc_rhat(runif(1000, 1, 1.07))
#'
#' # fake neff ratio values to use for demonstration
#' ratio <- c(runif(100, 0, 1))
#' mcmc_neff_hist(ratio)
#' mcmc_neff(ratio)
#' mcmc_neff(ratio) + move_legend("top") # add legend above plot
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
      y = ~ value,
      x = ~ factor_by_name,
      color = ~ factor_by_value,
      fill = ~ factor_by_value
    )
  ) +
    geom_segment(
      mapping = aes_(
        xend = ~ factor_by_name,
        yend = ifelse(min(rhat) < 1, 1, -Inf)
      ),
      na.rm = TRUE
    )

  if (min(rhat) < 1)
    graph <- graph +
      hline_at(1, color = "gray", size = 1)

  brks <- c(1, 1.05, 1.1)
  for (k in c(1.5, 2)) {
    if (any(rhat > k))
      brks <- c(brks, k)
  }

  graph +
    diagnostic_points(size) +
    hline_at(
      brks[-1],
      color = "gray",
      linetype = 2,
      size = 0.25
    ) +
    labs(x = NULL, y = bquote(hat(R))) +
    scale_fill_diagnostic("rhat") +
    scale_color_diagnostic("rhat") +
    theme_default(y_text = FALSE) +
    coord_flip() +
    scale_y_continuous(breaks = brks,
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
      y = ~ value,
      x = ~ factor_by_name,
      color = ~ factor_by_value,
      fill = ~ factor_by_value
    )
  ) +
    geom_segment(
      aes_(xend = ~factor_by_name, yend = -Inf),
      na.rm = TRUE
    ) +
    diagnostic_points(size) +
    hline_at(
      c(0.1, 0.5, 1),
      color = "gray",
      linetype = 2,
      size = 0.25
    ) +
    labs(x = NULL, y = bquote(N[eff]/N)) +
    scale_fill_diagnostic("neff") +
    scale_color_diagnostic("neff") +
    theme_default(y_text = FALSE) +
    coord_flip() +
    scale_y_continuous(breaks = c(0.1, seq(0, 1, .25)),
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



# internal ----------------------------------------------------------------
diagnostic_points <- function(size = NULL) {
  args <- list(shape = 21, na.rm = TRUE)
  do.call("geom_point", c(args, size = size))
}

# data frame with y=rhat (or neff) and x = factor(rhat) or factor(neff) with
# factor labels as names instead of value
diagnostic_data_frame <- function(x, diagnostic = c("rhat", "neff")) {
  diagnostic <- match.arg(diagnostic)
  fac <- if (!is.null(names(x))) {
    factor(x, labels = names(x))
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
