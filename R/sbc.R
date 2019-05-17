klUniform <- function(v, numPriorDraws, samplesPerPrior) {
  # D_{KL}(v || uniform)
  # https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence
  expectedPr <- 1.0/samplesPerPrior
  observedPr <- table(v) / numPriorDraws
  sum(observedPr * log(observedPr/expectedPr))
}

#' Histograms for Simulation Based Calibration
#'
#' @param ranks A list of sampling realizations.
#' @param thin An integer vector of length one indicating the thinning interval
#' when plotting
#' @param perBin Number of histogram entries to combine into a single bar.
#' @param worst If NA, plots all parameters. Otherwise how many parameters to show.
#' Parameters are ordered by the degree of non-uniformity.
#' @param alpha Uncertainty interval probability for a false positive (alpha level).
#' @param hideAxes Whether to hide the plot axes.
#'
#' Each list element of \code{ranks} should be a matrix of rank
#' comparison results (encoded as 0 or 1) associated with a single
#' draw from the prior distribution. Each draw from the posterior is
#' in the row and parameters are in columns. The matrix should have
#' column names to correctly label the parameters.
#'
#' So that the histograms consist of independent realizations,
#' draws from the posterior should be thinned to remove
#' autocorrelation. Set \code{thin} such that the number of
#' draws approximately matches the effective sample size.
#'
#' For best results, one plus the number of draws from the posterior
#' should be evenly divisible by the number of histogram bins after
#' thinning. For example, 511 draws after thinning results in 128
#' draws. If perBin is set to 4 then 32 histogram bars are drawn.
#' 
#' @template return-ggplot
#'
#' @references
#'   Talts, S., Betancourt, M., Simpson, D., Vehtari, A., and Gelman, A. (2018).
#'   Validating Bayesian Inference Algorithms with Simulation-Based Calibration.
#'   arXiv preprint arXiv:1804.06788. \url{https://arxiv.org/abs/1804.06788}
#' @seealso
#' \link[rstan]{sbc}
#' @examples
#' pars <- paste0('parameter',1:2)
#' samplesPerPrior <- 511
#' ranks <- list()
#' for (px in 1:500) {
#'   r1 <- matrix(0, nrow=samplesPerPrior, ncol=length(pars),
#'          dimnames=list(NULL, pars))
#'   for (p1 in 1:length(pars)) {
#'     r1[sample.int(samplesPerPrior,
#'                   floor(runif(1, 0, samplesPerPrior))), p1] <- 1
#'   }
#'   ranks[[px]] <- r1
#' }
#' sbc_hist(ranks)
#' @export

sbc_hist <- function(ranks, thin = 4, perBin=4, worst=16, ...,
                     alpha = 0.01, hideAxes=TRUE) {
  numPriorDraws <- length(ranks)
  thinner <- seq(from = 1, to = nrow(ranks[[1]]), by = thin)
  samplesPerPrior <- length(thinner)
  u <- t(sapply(ranks, FUN = function(r) 1 + colSums(r[thinner, , drop = FALSE])))
  if (ncol(ranks[[1]]) == 1) {
    u <- t(u)
    dimnames(u) <- list(NULL, colnames(ranks[[1]]))
  }
  
  if (!is.na(worst)) {
    kl <- apply(u, 2, function(v) klUniform(v, numPriorDraws, samplesPerPrior))
    filter <- order(-kl)[1:min(worst,ncol(u))]
#    print(filter)
    u <- u[, filter, drop=FALSE ]
  }
  
  parameter <- ordered(rep(colnames(u), each = nrow(u)),
                       levels=colnames(u))
  d <- data.frame(u = c(u), parameter)
  if (samplesPerPrior %% perBin != 0) {
    warning(paste("perBin (", perBin, ") does not evenly divide the",
      "number of samples per prior (",samplesPerPrior,")"))
  }
  numBins <- samplesPerPrior/perBin
  CI <- qbinom(c(alpha/2,0.5,1-alpha/2), numPriorDraws, numBins^-1) + c(-.5,0,.5)
  offset <- perBin*2
  pl <- ggplot(d, aes(x = u)) + 
    geom_polygon(data=data.frame(x=c(-offset,0,-offset,samplesPerPrior + offset,
      samplesPerPrior, samplesPerPrior + offset,-offset),
      y=c(CI[1],CI[2],CI[3],CI[3],CI[2],CI[1],CI[1])),
      aes(x=x,y=y),fill="grey45",color="grey25",alpha=0.5) +
    geom_histogram(bins=numBins, na.rm=TRUE) +
#    xlim(1,samplesPerPrior) +
    # https://github.com/tidyverse/ggplot2/issues/3332
    facet_wrap("parameter") +
    geom_hline(yintercept=CI[1], color='green', linetype="dotted", alpha=.5) +
  geom_hline(yintercept=CI[3], color='green', linetype="dotted", alpha=.5)
  if (hideAxes) {
    pl <- pl + theme(axis.text.x=element_blank(),
      axis.text.y=element_blank(),axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank())
    }
  pl
}
