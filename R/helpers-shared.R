#' Check for suggested package with `requireNamespace` and throw error if necessary
#'
#' @noRd
#' @param pkg Package name as a string.
#' @param min_version Optionally, a minimum version number as a string.
#' @return `TRUE`, invisibly, if no error is thrown.
#'
suggested_package <- function(pkg, min_version = NULL) {
  stopifnot(length(pkg) == 1, is.character(pkg))
  if (!requireNamespace(pkg, quietly = TRUE)) {
    abort(paste("Please install the", pkg, "package to use this function."))
  }
  if (!is.null(min_version)) {
    stopifnot(is.character(min_version))
    if (utils::packageVersion(pkg) < package_version(min_version)) {
      abort(paste(
        "Version >= ", min_version, "of the",
        pkg, "package is required to use this function."
      ))
    }
  }
  invisible(TRUE)
}

# Return x if not NULL, otherwise y
`%||%` <- function(x, y) if (!is.null(x)) x else y

#' Warn about ignored arguments
#'
#' @param ... The `...` arguments from the calling function.
#' @param ok_args A character vector of argument names to ignore.
#' @return Nothing, but a warning may be thrown.
#' @noRd
check_ignored_arguments <- function(..., ok_args = character()) {
  dots <- list(...)
  nms <- names(dots)
  if (length(dots)) {
    unrecognized <- if (!length(ok_args)) nms else setdiff(nms, ok_args)
    if (length(unrecognized)) {
      warn(paste(
        "The following arguments were unrecognized and ignored:",
        paste(unrecognized, collapse = ", ")
      ))
    }
  }
}
