% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayesplot-extractors.R
\name{bayesplot-extractors}
\alias{bayesplot-extractors}
\alias{log_posterior}
\alias{log_posterior.stanfit}
\alias{log_posterior.stanreg}
\alias{neff_ratio}
\alias{neff_ratio.stanfit}
\alias{neff_ratio.stanreg}
\alias{nuts_params}
\alias{nuts_params.list}
\alias{nuts_params.stanfit}
\alias{nuts_params.stanreg}
\alias{rhat}
\alias{rhat.stanfit}
\alias{rhat.stanreg}
\title{Extract quantities needed for plotting from model objects}
\usage{
log_posterior(object, ...)

nuts_params(object, ...)

rhat(object, ...)

neff_ratio(object, ...)

\method{log_posterior}{stanfit}(object, inc_warmup = FALSE, ...)

\method{log_posterior}{stanreg}(object, inc_warmup = FALSE, ...)

\method{nuts_params}{list}(object, pars = NULL, ...)

\method{nuts_params}{stanfit}(object, pars = NULL, inc_warmup = FALSE, ...)

\method{nuts_params}{stanreg}(object, pars = NULL, inc_warmup = FALSE, ...)

\method{rhat}{stanfit}(object, pars = NULL, ...)

\method{rhat}{stanreg}(object, pars = NULL, regex_pars = NULL, ...)

\method{neff_ratio}{stanfit}(object, pars = NULL, ...)

\method{neff_ratio}{stanreg}(object, pars = NULL, regex_pars = NULL, ...)
}
\arguments{
\item{object}{The object to use.}

\item{...}{Arguments passed to individual methods.}

\item{inc_warmup}{A logical scalar (defaulting to \code{FALSE}) indicating
whether to include warmup draws, if applicable.}

\item{pars}{An optional character vector of parameter names. For
\code{nuts_params} these will be NUTS sampler parameter names rather than
model parameters. If \code{pars} is omitted all parameters are included.}

\item{regex_pars}{An optional \link[=grep]{regular expression} to use for
parameter selection. Can be specified instead of \code{pars} or in addition
to \code{pars}.}
}
\value{
\describe{
\item{\code{log_posterior}}{
\code{log_posterior} methods return a molten data frame (see
\code{\link[reshape2]{melt}}). If the model represented by \code{object} was
fit via MCMC the molten data frame should have columns \code{"Iteration"}
(integer), \code{"Chain"} (integer), and \code{"Value"} (numeric). For models
fit using other methods, \code{log_posterior} methods can return a data frame
with a single column \code{"Value"}.
}
\item{\code{nuts_params}}{
\code{nuts_params} methods return a molten data frame (see
\code{\link[reshape2]{melt}}). The molten data frame should have columns
\code{"Parameter"} (factor), \code{"Iteration"} (integer), \code{"Chain"}
(integer), and \code{"Value"} (numeric), in any order.
}
\item{\code{rhat}, \code{neff_ratio}}{
Methods return (named) vectors.
}
}
}
\description{
Generics and methods for extracting quantities needed for plotting from
various types of model objects. Currently methods are only provided for
stanfit (\pkg{rstan}) and stanreg (\pkg{rstanarm}) objects, but adding new
methods should be relatively straightforward.
}
