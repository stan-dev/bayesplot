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

#' Handle size -> linewidth deprecation
#'
#' @param size User's `size` argument (deprecated for lines).
#' @param linewidth User's `linewidth` argument (replacement).
#' @param default_linewidth Default linewidth value if neither is specified.
#' @param calling_fn Name of the calling function for the deprecation message.
#' @return The resolved linewidth value.
#' @noRd
resolve_linewidth <- function(size, linewidth, default_linewidth, calling_fn = NULL) {
  if (!is.null(size)) {
    lifecycle::deprecate_warn(
      when = "1.16.0",
      what = paste0(calling_fn %||% "fn", "(size)"),
      with = paste0(calling_fn %||% "fn", "(linewidth)")
    )
    return(size)
  }
  linewidth %||% default_linewidth
}


#' Handle fatten deprecation
#'
#' @param fatten User's `fatten` argument (deprecated).
#' @param size User's `size` argument.
#' @param default_size Default size when neither `fatten` nor `size` is given.
#' @param calling_fn Name of the calling function for the deprecation message.
#' @return The resolved point `size` value.
#' @noRd
resolve_fatten <- function(fatten, size, default_size, calling_fn = NULL) {
  if (!lifecycle::is_present(fatten)) {
    return(size %||% default_size)
  }

  lifecycle::deprecate_warn(
    when = "1.16.0",
    what = paste0(calling_fn %||% "fn", "(fatten)"),
    details = paste0(
      "The point size is now controlled directly by `size`. ",
      "The `fatten` argument will be removed in a future release."
    )
  )
  size %||% default_size
}


#' Validate bounds passed to stat_density/geom_density wrappers
#' @noRd
validate_density_bounds <- function(bounds) {
  if (is.null(bounds)) {
    return(NULL)
  }
  if (!is.numeric(bounds) || length(bounds) != 2 || anyNA(bounds)) {
    abort("`bounds` must be a numeric vector of length 2.")
  }
  if (bounds[1] >= bounds[2]) {
    abort("`bounds` must satisfy bounds[1] < bounds[2].")
  }
  bounds
}
