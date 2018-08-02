#' Check for suggested package (requireNamespace) and throw error if necessary
#'
#' @noRd
#' @param pkg Package name as a string.
#' @param min_version Optionally, a minimum version number as a string.
#' @return TRUE, invisibly, if no error is thrown.
#'
suggested_package <- function(pkg, min_version = NULL) {
  stopifnot(length(pkg) == 1, is.character(pkg))
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Please install the ",
      pkg, " package to use this function.",
      call. = FALSE
    )
  }
  if (!is.null(min_version)) {
    stopifnot(is.character(min_version))
    if (utils::packageVersion(pkg) < package_version(min_version)) {
      stop(
        "Version >=", min_version, " of the ",
        pkg, " package is required to use this function.",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

#' Explicit and/or regex parameter selection
#'
#' @noRd
#' @param explicit Character vector of selected parameter names.
#' @param patterns Character vector of regular expressions.
#' @param complete Character vector of all possible parameter names.
#' @return Characeter vector of combined explicit and matched (via regex)
#'   parameter names, unless an error is thrown.
#'
select_parameters <-
  function(explicit = character(),
           patterns = character(),
           complete = character()) {

    stopifnot(is.character(explicit),
              is.character(patterns),
              is.character(complete))

    if (!length(explicit) && !length(patterns))
      return(complete)

    if (length(explicit)) {
      if (!all(explicit %in% complete)) {
        not_found <- which(!explicit %in% complete)
        stop(
          "Some 'pars' don't match parameter names: ",
          paste(explicit[not_found], collapse = ", ")
        )
      }
    }

    if (!length(patterns)) {
      return(unique(explicit))
    } else {
      regex_pars <-
        unlist(lapply(seq_along(patterns), function(j) {
          grep(patterns[j], complete, value = TRUE)
        }))
      if (!length(regex_pars))
        stop("No matches for 'regex_pars'.", call. = FALSE)
    }

    unique(c(explicit, regex_pars))
  }


# Return x if not NULL, otherwise y
`%||%` <- function(x, y) if (!is.null(x)) x else y

# Check for ignored arguments
check_ignored_arguments <- function(..., ok_args = character()) {
  dots <- list(...)
  if (length(dots)) {
    unrecognized <- if (!length(ok_args))
      names(dots) else setdiff(names(dots), ok_args)
    if (length(unrecognized)) {
      warning(
        "The following arguments were unrecognized and ignored: ",
        paste(unrecognized, collapse = ", "),
        call. = FALSE
      )
    }
  }
}
