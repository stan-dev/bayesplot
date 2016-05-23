#' Posterior predictive checks (S3 generic)
#'
#' This is an S3 generic function. The \pkg{ppcheck} package itself provides no
#' methods accompanying the \code{pp_check} generic. The intent is to provide a
#' generic so authors of other \R packages who wish to provide interfaces to the
#' functions in \pkg{ppcheck} will be encouraged to include \code{pp_check}
#' methods in their package, preserving the same naming conventions for
#' posterior predictive checking across many \R packages for Bayesian inference.
#' This is for the convenience of both users and developers. See the Details and
#' Examples sections, below, and the package vignettes for examples of defining
#' \code{pp_check} methods.
#'
#' @export
#' @param object A fitted model object.
#' @param ... Other arguments for use by methods.
#' @return The exact form of the value returned by \code{pp_check} may vary by
#'   the class of \code{object}, but for consistency we encourage authors of
#'   methods to always return the \code{ggplot} object created by one of
#'   \pkg{ppcheck}'s plotting functions.
#'
#' @details A package that creates fitted model objects of class \code{"foo"}
#'   can include a method \code{pp_check.foo} that prepares the appropriate
#'   inputs (\code{y}, \code{yrep}, etc.) for the \pkg{ppcheck} functions. The
#'   \code{pp_check.foo} method may, for example, let the user choose between
#'   various \pkg{ppcheck} plots, calling the functions from \pkg{ppcheck}
#'   internally as needed. See Examples, below, and the package vignettes.
#'
#' @examples
#' x <- list(y = rnorm(50), yrep = matrix(rnorm(5000), nrow = 100, ncol = 50))
#' class(x) <- "foo"
#' pp_check.foo <- function(object, ..., type = c("multiple", "overlaid")) {
#'   y <- object[["y"]]
#'   yrep <- object[["yrep"]]
#'   switch(match.arg(type),
#'          multiple = ppc_hist(y, yrep[1:min(8, nrow(yrep)),, drop = FALSE]),
#'          overlaid = ppc_dens_overlay(y, yrep)
#'   )
#' }
#' pp_check(x)
#' pp_check(x, type = "overlaid")
#'
pp_check <- function(object, ...) {
  UseMethod("pp_check")
}
