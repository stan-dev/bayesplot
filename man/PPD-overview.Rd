% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ppd-overview.R
\name{PPD-overview}
\alias{PPD-overview}
\alias{PPD}
\title{Plots of posterior or prior predictive distributions}
\description{
The \strong{bayesplot} PPD module provides various plotting functions
for creating graphical displays of simulated data from the posterior or
prior predictive distribution. These plots are essentially the same as the
corresponding \link{PPC} plots but without showing any observed data. Because
these are not "checks" compared to data we use PPD (for prior/posterior
predictive distribution) instead of PPC (for prior/posterior predictive
check).
}
\section{PPD plotting functions}{
 The functions for plotting prior and
posterior predictive distributions without observed data each have the
prefix \code{ppd_} and all have a required argument \code{ypred} (a matrix of
predictions). The plots are organized into several categories, each with
its own documentation:
\itemize{
\item \link{PPD-distributions}: Histograms, kernel density estimates, boxplots, and
other plots of multiple simulated datasets (rows) in \code{ypred}. These are the
same as the plots in \link{PPC-distributions} but without including any
comparison to \code{y}.
\item \link{PPD-intervals}: Interval estimates for each predicted observations
(columns) in \code{ypred}. The x-axis variable can be optionally specified by
the user (e.g. to plot against against a predictor variable or over
time).These are the same as the plots in \link{PPC-intervals} but without
including any comparison to \code{y}.
\item \link{PPD-test-statistics}: The distribution of a statistic, or a pair of
statistics, over the simulated datasets (rows) in \code{ypred}. These are the
same as the plots in \link{PPC-test-statistics} but without including any
comparison to \code{y}.
}
}

\references{
Gabry, J. , Simpson, D. , Vehtari, A. , Betancourt, M. and
Gelman, A. (2019), Visualization in Bayesian workflow.
\emph{J. R. Stat. Soc. A}, 182: 389-402. doi:10.1111/rssa.12378.
(\href{https://rss.onlinelibrary.wiley.com/doi/full/10.1111/rssa.12378}{journal version},
\href{https://arxiv.org/abs/1709.01449}{arXiv preprint},
\href{https://github.com/jgabry/bayes-vis-paper}{code on GitHub})
}
\seealso{
Other PPDs: 
\code{\link{PPD-distributions}},
\code{\link{PPD-intervals}},
\code{\link{PPD-test-statistics}}
}
\concept{PPDs}
