# Check for suggested package (requireNamespace)
#
# @param pkg Package name as a string
#
suggested_package <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop(
        "Please install the ", pkg, " package to use this function.",
        call. = FALSE
      )
  }
}

# Explicit and/or regex parameter selection
#
# @param explicit Character vector of selected parameter names.
# @param patterns Character vector of regular expressions.
# @param complete Character vector of all possible parameter names.
# @return Characeter vector of combined explicit and matched (via regex)
#   parameter names, unless an error is thrown.
#
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

# named lists
nlist <- function (...) {
  m <- match.call()
  out <- list(...)
  no_names <- is.null(names(out))
  has_name <- if (no_names)
    FALSE else nzchar(names(out))
  if (all(has_name))
    return(out)
  nms <- as.character(m)[-1L]
  if (no_names) {
    names(out) <- nms
  } else {
    names(out)[!has_name] <- nms[!has_name]
  }

  return(out)
}
