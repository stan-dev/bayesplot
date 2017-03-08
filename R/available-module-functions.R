#' Get or view the names of available plotting functions
#'
#' @export
#' @param pattern,fixed,invert Passed to \code{\link[base]{grep}}.
#' @return A possibly empty character vector of function names with several
#'   additional attributes (for use by a custom print method). If \code{pattern}
#'   is missing then the returned object contains the names of all available
#'   plotting functions in the \link{MCMC} or \link{PPC} module, depending on
#'   which function is called. If \code{pattern} is specified then a subset of
#'   function names is returned.
#'
#' @examples
#' available_mcmc()
#' available_mcmc("nuts")
#' available_mcmc("rhat|neff")
#' available_ppc("grouped")
#' available_ppc("grouped", invert = TRUE)
#'
available_ppc <- function(pattern, fixed = FALSE, invert = FALSE) {
  .list_module_functions("ppc", .pattern = pattern,
                         fixed = fixed, invert = invert)
}

#' @rdname available_ppc
#' @export
available_mcmc <- function(pattern, fixed = FALSE, invert = FALSE) {
  .list_module_functions("mcmc", .pattern = pattern,
                         fixed = fixed, invert = invert)
}

#' @export
print.bayesplot_function_list <- function(x, ...) {
  atts <- attributes(x)
  cat("bayesplot", toupper(atts[["module"]]), "module:\n")
  if (!is.null(atts[["pattern"]]))
    cat(paste0(
      "(",
      ifelse(atts[["inverted"]], "excluding", "matching"),
      " pattern '",
      atts[["pattern"]],
      "')"
    ), "\n")

  cat(paste0("  ", x), sep = "\n")
  invisible(x)
}


# internal ----------------------------------------------------------------
.list_module_functions <-
  function(.module = c("ppc", "mcmc"),
           .pattern,
           fixed = FALSE,
           invert = FALSE) {

    .module <- match.arg(.module)
    if (missing(.pattern))
      .pattern <- NULL

    funs <- sort(grep(
      paste0("^", .module, "_"),
      getNamespaceExports("bayesplot"),
      value = TRUE
    ))
    structure(
      .Data =
        if (is.null(.pattern))
          funs
        else
          grep(
            pattern = .pattern,
            x = funs,
            value = TRUE,
            fixed = fixed,
            invert = invert
          ),
      class = c("bayesplot_function_list", "character"),
      module = .module,
      pattern = .pattern,
      inverted = invert
    )
  }
