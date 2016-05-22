#' Posterior predictive checks
#'
#' This is an S3 generic function. The intent is to provide a generic so that
#' any packages authors who wish to provide interfaces to the functions in this
#' package will be encouraged to use the same naming conventions to minimize the
#' confusion of users, and avoid unnecessary function masking. For example, a
#' package that creates fitted model objects of class \code{"x"} can define a
#' \code{ppc.x} method that prepares the appropriate inputs (\code{y},
#' \code{yrep}, etc.) and lets the user choose between the various plots,
#' calling the functions from \pkg{ppcheck} internally as needed.
#'
#' @export
#' @param object A fitted model object.
#' @param ... Other arguments for use by methods.
#' @return The exact form of the value returned by \code{ppc} may vary by the
#'   class of \code{object}, but for consistency we encourage authors of methods
#'   to always return the \code{ggplot} object created by one of \pkg{ppcheck}'s
#'   plotting functions.
#'
ppc <- function(object, ...) {
  UseMethod("ppc")
}
