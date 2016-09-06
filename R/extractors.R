#' Extract quantities needed for plotting from model objects
#'
#' Generics and methods for extracting quantities needed for plotting from
#' various types of model objects. Currently methods are only provided for
#' stanfit (\pkg{rstan}) and stanreg (\pkg{rstanarm}) objects, but adding new
#' methods should be relatively straightforward.
#'
#' @name extractors
#' @param object The object to use.
#' @param ... Arguments passed to individual methods.
#' @param pars An optional character vector of parameter names. For
#'   \code{nuts_params} these will be NUTS sampler parameter names rather than
#'   model parameters. If \code{pars} is omitted all parameters are included.
#'
#' @return
#' \describe{
#' \item{\code{log_posterior}}{
#' \code{log_posterior} methods return a molten data frame
#' (\code{\link[reshape2]{melt}}). If the model represented by \code{object} was
#' fit via MCMC the molten data frame should have columns \code{"Iteration"}
#' (integer), \code{"Chain"} (integer), and \code{"Value"} (numeric).
#' For models fit using other methods, \code{log_posterior} methods can
#' return a data frame with a single column \code{"Value"}.
#' }
#' \item{\code{nuts_params}}{
#' \code{nuts_params} methods return a molten data frame
#' (\code{\link[reshape2]{melt}}). The molten data frame should have columns
#' \code{"Parameter"} (factor), \code{"Iteration"} (integer), \code{"Chain"}
#' (integer), and \code{"Value"} (numeric), in any order.
#' }
#' \item{\code{rhat}, \code{neff_ratio}}{
#' Methods return (named) vectors.
#' }
#' }
#'
NULL


# log_posterior -----------------------------------------------------------
#' @rdname extractors
#' @export
log_posterior <- function(object, ...) {
  UseMethod("log_posterior")
}
# nuts_params -------------------------------------------------------------
#' @rdname extractors
#' @export
nuts_params <- function(object, ...) {
  UseMethod("nuts_params")
}
# rhat -------------------------------------------------------------
#' @rdname extractors
#' @export
rhat <- function(object, ...) {
  UseMethod("rhat")
}
# neff_ratio -------------------------------------------------------------
#' @rdname extractors
#' @export
neff_ratio <- function(object, ...) {
  UseMethod("neff_ratio")
}



#' @rdname extractors
#' @export
#' @method log_posterior stanfit
#' @param inc_warmup A logical scalar (defaulting to \code{FALSE}) indicating
#'   whether to include warmup draws, if applicable.
#'
log_posterior.stanfit <- function(object, inc_warmup = FALSE, ...) {
  lp <- rstan::get_logposterior(object,
                                inc_warmup = inc_warmup,
                                ...)
  lp <- lapply(lp, as.array)
  lp <- setNames(reshape2::melt(lp),
                 c("Iteration", "Value", "Chain"))
  validate_df_classes(lp, c("integer", "numeric", "integer"))
}

#' @rdname extractors
#' @export
#' @method log_posterior stanreg
#'
log_posterior.stanreg <- function(object, inc_warmup = FALSE, ...) {
  log_posterior.stanfit(object$stanfit,
                        inc_warmup = inc_warmup,
                        ...)
}

#' @rdname extractors
#' @export
#' @method nuts_params list
#'
nuts_params.list <- function(object, pars = NULL, ...) {
  if (length(pars))
    object <- lapply(object, function(x)
      x[, pars, drop = FALSE])

  object <- setNames(reshape2::melt(object),
                     c("Iteration", "Parameter", "Value", "Chain"))
  validate_df_classes(object, c("integer", "factor", "numeric", "integer"))
}

#' @rdname extractors
#' @export
#' @method nuts_params stanfit
#'
nuts_params.stanfit <-
  function(object,
           pars = NULL,
           inc_warmup = FALSE,
           ...) {
    suggested_package("rstan")

    np <- rstan::get_sampler_params(object, inc_warmup = inc_warmup)
    nuts_params.list(np, pars = pars, ...)
  }

#' @rdname extractors
#' @export
#' @method nuts_params stanreg
#'
nuts_params.stanreg <-
  function(object,
           pars = NULL,
           inc_warmup = FALSE,
           ...) {
    nuts_params.stanfit(object$stanfit,
                        pars = pars,
                        inc_warmup = inc_warmup,
                        ...)
  }


#' @rdname extractors
#' @export
#' @method rhat stanfit
#'
rhat.stanfit <- function(object, pars = NULL, ...) {
  suggested_package("rstan")
  s <- if (!is.null(pars)) {
    rstan::summary(object, pars = pars, ...)
  } else {
    rstan::summary(object, ...)
  }

  s$summary[, "Rhat"]
}

#' @rdname extractors
#' @export
#' @method rhat stanreg
#' @template args-regex_pars
#'
rhat.stanreg <- function(object, pars = NULL, regex_pars = NULL, ...) {
  suggested_package("rstanarm")
  r <- summary(object, pars = pars, regex_pars = regex_pars, ...)[, "Rhat"]
  if (!is.null(pars) || !is.null(regex_pars))
    return(r)

  r[!names(r) %in% c("mean_PPD", "log-posterior")]
}


#' @rdname extractors
#' @export
#' @method neff_ratio stanfit
#'
neff_ratio.stanfit <- function(object, pars = NULL, ...) {
  suggested_package("rstan")
  s <- if (!is.null(pars)) {
    rstan::summary(object, pars = pars, ...)
  } else {
    rstan::summary(object, ...)
  }
  tss <- nrow(as.matrix(object, pars = "lp__"))
  jitter_neff(s$summary[, "n_eff"]) / tss
}

#' @rdname extractors
#' @export
#' @method neff_ratio stanreg
#'
neff_ratio.stanreg <- function(object, pars = NULL, regex_pars = NULL, ...) {
  suggested_package("rstanarm")
  s <- summary(object, pars = pars, regex_pars = regex_pars, ...)
  ess <- s[, "n_eff"]
  tss <- attr(s, "posterior_sample_size")
  ratio <- jitter_neff(ess) / tss
  if (!is.null(pars) || !is.null(regex_pars))
    return(ratio)

  ratio[!names(ratio) %in% c("mean_PPD", "log-posterior")]
}


# internals ---------------------------------------------------------------

# Check that variables in a data.frame have certain classes
# @param x data.frame
# @param classes character vector of classes (in the order of the column in x)
# @return x, unless an error is thrown
#
validate_df_classes <- function(x, classes = character()) {
  stopifnot(
    is.data.frame(x),
    is.character(classes),
    ncol(x) == length(classes)
  )
  for (j in 1:ncol(x)) {
    if (!inherits(x[, j], classes[j]))
      stop(colnames(x)[j], " does not have class ", classes[j],
           call. = FALSE)
  }
  return(x)
}

# for jittering neff estimates so no duplicate values
jitter_neff <- function(x) {
  x - abs(rnorm(length(x), sd = 0.0001))
}

