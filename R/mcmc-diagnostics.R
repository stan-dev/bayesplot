#' Rhat and effective sample size
#'
#' Plots of Rhat and ratio of effective sample size to total sample size.
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
#' colored using different shades:
#'  \itemize{
#'    \item \emph{light}: below 1.05 (good)
#'    \item \emph{mid}: between 1.05 and 1.1 (ok)
#'    \item \emph{dark}: above 1.1 (too high)
#'  }
#' }
#' \item{\code{mcmc_neff, mcmc_neff_hist}}{
#' Ratios of effective sample size to total sample size as either points or a histogram.
#' Values are colored using different shades:
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
#' mcmc_neff(ratio) + move_legend("top")
#'
NULL


# Rhat --------------------------------------------------------------------
#' @rdname MCMC-diagnostics
#' @export
#' @param rhat A vector of \code{\link[=rhat]{Rhat}} estimates.
#'
mcmc_rhat <- function(rhat, ..., size = NULL) {
  rhat <- validate_rhat(rhat)

  # factor rhat by parameter instead of value
  frhat <- if (!is.null(names(rhat))) {
    factor(rhat, labels = names(rhat))
  } else {
    factor(rhat)
  }

  data <- data.frame(y = rhat, x = frhat)
  rownames(data) <- NULL
  graph <- ggplot(data, aes_(
    x = ~ x, y = ~ y,
    color = ~factor_rhat(rhat),
    fill = ~factor_rhat(rhat)
  )) +
    geom_segment(
      mapping = aes_(
        xend = ~x,
        yend = ifelse(min(rhat) < 1, 1, -Inf),
        color = ~factor_rhat(rhat)
      ),
      na.rm = TRUE
    )

  if (min(rhat) < 1)
    graph <- graph +
      hline_at(1, color = "gray", size = 1)

  graph +
    diagnostic_points(size) +
    hline_at(
      c(1.05, 1.1),
      color = "gray",
      linetype = 2,
      size = 0.25
    ) +
    labs(x = NULL, y = bquote(hat(R))) +
    scale_fill_diagnostic("rhat") +
    scale_color_diagnostic("rhat") +
    theme_default(y_text = FALSE) +
    coord_flip() +
    scale_y_continuous(breaks = c(1, 1.05, 1.1),
                       expand = c(0,.001))
}

#' @rdname MCMC-diagnostics
#' @export
mcmc_rhat_hist <- function(rhat, ..., binwidth = NULL) {
  rhat <- validate_rhat(rhat)
  ggplot(
    data.frame(x = rhat, lev = factor_rhat(rhat)),
    aes_(
      x = ~ x,
      color = ~ lev,
      fill = ~ lev
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
  .neff_dots <- function(size = NULL) {
    args <- list(shape = 21, na.rm = TRUE)
    do.call("geom_point", c(args, size = size))
  }

  ratio <- validate_neff_ratio(ratio)

  # factor neff ratio by parameter instead of value
  fratio <- if (!is.null(names(ratio))) {
    factor(ratio, labels = names(ratio))
  } else {
    factor(ratio)
  }
  data <- data.frame(y = ratio, x = fratio)
  rownames(data) <- NULL
  ggplot(data, aes_(
    x = ~ x,
    y = ~ y,
    color = ~factor_neff(ratio),
    fill = ~factor_neff(ratio)
  )) +
    geom_segment(
      aes_(xend = ~x, yend = -Inf, color = ~factor_neff(ratio)),
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
  ratio <- validate_neff_ratio(ratio)
  ggplot(
    data.frame(x = ratio, lev = factor_neff(ratio)),
    aes_(
      x = ~ x,
      color = ~ lev,
      fill = ~ lev
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

  rhat <- as.vector(rhat)
  drop_NAs_and_warn(rhat)
}

# either throws error or returns as.vector(ratio)
validate_neff_ratio <- function(ratio) {
  stopifnot(is_vector_or_1Darray(ratio))
  if (any(ratio < 0 | ratio > 1, na.rm = TRUE))
    stop("All elements of 'ratio' must be between 0 and 1.")

  ratio <- as.vector(ratio)
  drop_NAs_and_warn(ratio)
}
