#' Diagnostics for Simulation Based Calibration
#'
#' @export
#' @param ranks A list of matrices. See **Details**.
#' @param thin An integer indicating the thinning interval to use when plotting
#'   so that the histograms consist of (close to) independent realizations. Set
#'   the `thin` argument such that the resulting number of draws approximately
#'   matches the effective sample size.
#' @param per_bin An integer indicating the number of histogram entries to
#'   combine into a single bar. For best results, one plus the number of draws
#'   from the posterior should be evenly divisible by the number of histogram
#'   bins after thinning. For example, if there are `511` posterior draws (per
#'   matrix in `ranks`) and `thin=4`, then after thinning there will be `128`
#'   draws. If `per_bin=4`, then `128/4=32` histogram bars will be drawn.
#' @param worst How many parameters to show, or `NA` to plot all parameters. If
#'   `worst` is not `NA` then parameters are ordered by the degree of
#'   non-uniformity so, for example, `worst = 10` means to plot only the `10`
#'   worst parameters.
#' @param prob The size of the interval plotted to show the expected behavior
#'   under uniformity. The default is `prob=0.99`.
#' @param ... Currently ignored.
#' @template args-facet_args
#'
#' @details
#' Each element of the list `ranks` should be a matrix of rank comparison
#' results (encoded as `0` or `1`) associated with a single draw from the prior
#' distribution. (These are not actually "ranks" but can be used afterwards to
#' reconstruct (thinned) ranks.) The columns of each matrix are model parameters
#' and the rows are posterior draws from the model fit to the data generated
#' from the corresponding prior draw. An element is `0` or `1` depending on
#' whether the posterior draw is greater or less than the corresponding "true"
#' realization (from the prior). The matrices should have column names to
#' correctly label the parameters.
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{`sbc_hist()`}{
#'   SBC histogram from Talts et al. (2018). A separate plot (facet) is created
#'   for each parameter.
#'   }
#' }
#'
#' @template return-ggplot
#'
#' @references
#' Talts, S., Betancourt, M., Simpson, D., Vehtari, A., and Gelman, A. (2018).
#' Validating Bayesian Inference Algorithms with Simulation-Based Calibration.
#' arXiv preprint arXiv:1804.06788. \url{https://arxiv.org/abs/1804.06788}
#'
#' @seealso `rstan::sbc()`
#'
#' @examples
#' # create some fake inputs to use for sbc_hist()
#' set.seed(19)
#' pars <- paste0("beta[", 1:4, "]")
#' samples_per_prior <- 511
#' n_replications <- 500
#' ranks <- list()
#' for (n in 1:n_replications) {
#'   r1 <- matrix(0, nrow=samples_per_prior, ncol=length(pars),
#'                dimnames=list(NULL, pars))
#'   for (p1 in 1:length(pars)) {
#'     r1[sample.int(samples_per_prior, floor(runif(1, 0, samples_per_prior))), p1] <- 1
#'   }
#'   ranks[[n]] <- r1
#' }
#'
#' color_scheme_set("purple")
#' sbc_hist(ranks)
#' sbc_hist(ranks, worst = NA) # uses original parameter ordering
#' sbc_hist(ranks, worst = 3, facet_args = list(labeller = ggplot2::label_parsed))
#'
sbc_hist <- function(ranks,
                     ...,
                     thin = 4,
                     per_bin = 4,
                     worst = 16,
                     prob = 0.99,
                     facet_args = list()) {
  check_ignored_arguments(...)
  stopifnot(is.list(ranks), all(sapply(ranks, is.matrix)))
  rows <- sapply(ranks, nrow)
  cols <- sapply(ranks, ncol)
  if (any(rows != rows[1]) || any(cols != cols[1])) {
    stop("Not all matrices in 'ranks' have the same dimensions.")
  }

  num_prior_draws <- length(ranks)
  thinner <- seq(from = 1, to = nrow(ranks[[1]]), by = thin)
  thinned_sample_size <- length(thinner)
  u <- t(sapply(ranks, FUN = function(r) 1 + colSums(r[thinner, , drop = FALSE])))
  if (ncol(ranks[[1]]) == 1) {
    u <- t(u)
    dimnames(u) <- list(NULL, colnames(ranks[[1]]))
  }

  if (!is.na(worst)) {
    # order starting with worst (least uniform)
    kl <- apply(u, 2, function(v) kl_uniform(v, num_prior_draws, thinned_sample_size))
    filter <- order(-kl)[1:min(worst, ncol(u))]
    u <- u[, filter, drop = FALSE]
  }

  pars <- parameter_names(u)
  data <- data.frame(
    Parameter = ordered(rep(pars, each = nrow(u)), levels = pars),
    u = c(u)
  )

  num_bins <- thinned_sample_size / per_bin
  if (thinned_sample_size %% per_bin != 0) {
    warning(paste0("per_bin (", per_bin, ") does not evenly divide the ",
                   "number of samples per prior (", thinned_sample_size ,")."))
  }

  # data for polygon showing expected behavior under uniformity
  alpha <- 1 - prob
  CI <- qbinom(
    p = c(alpha / 2, 0.5, 1 - alpha / 2),
    size = num_prior_draws,
    prob = 1 / num_bins
  )
  CI <- CI + c(-0.5, 0, 0.5)
  offset <- 2 * per_bin
  polygon_data <- data.frame(
    x= c(-offset, 0, -offset, thinned_sample_size + offset,
         thinned_sample_size, thinned_sample_size + offset, -offset),
    y = c(CI[1], CI[2], CI[3], CI[3], CI[2], CI[1], CI[1])
  )

  graph <- ggplot(data, aes_(x = ~ u)) +
    geom_polygon(
      aes_(x = ~ x, y = ~ y),
      data = polygon_data,
      fill = "lightgray",
      color = NA,
      alpha = 1
    ) +
    geom_segment(
      x = -offset,
      xend = thinned_sample_size + offset,
      y = CI[3],
      yend = CI[3],
      color = get_color("mid_highlight"),
      alpha = 0.5,
      size = 0.2
    ) +
    geom_histogram(
      bins = num_bins,
      fill = get_color("mid"),
      color = get_color("mid_highlight"),
      size = 0.25,
      na.rm = TRUE
    ) +
    geom_segment(
      x = -offset,
      xend = thinned_sample_size + offset,
      y = CI[1],
      yend = CI[1],
      color = get_color("mid_highlight"),
      alpha = 0.1,
      size = 0.2
    ) +
    scale_x_continuous(
      name = "Rank statistic",
      breaks = c(0, round(thinned_sample_size / 2), thinned_sample_size)
    ) +
    coord_cartesian(expand = FALSE)

  facet_args[["facets"]] <- ~ Parameter
  graph +
    do.call("facet_wrap", facet_args) +
    bayesplot_theme_get() +
    yaxis_text(FALSE) +
    yaxis_title(FALSE) +
    yaxis_ticks(FALSE)
}



# internal ----------------------------------------------------------------
kl_uniform <- function(v, num_prior_draws, samples_per_prior) {
  # D_{KL}(v || uniform)
  # https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence
  expected_pr <- 1.0 / samples_per_prior
  observed_pr <- table(v) / num_prior_draws
  sum(observed_pr * log(observed_pr / expected_pr))
}
