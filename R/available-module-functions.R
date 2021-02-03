#' Get or view the names of available plotting functions
#'
#' @export
#' @param pattern,fixed,invert Passed to [base::grep()].
#' @return A possibly empty character vector of function names with several
#'   additional attributes (for use by a custom print method). If `pattern`
#'   is missing then the returned object contains the names of all available
#'   plotting functions in the [MCMC] or [PPC] module, depending on
#'   which function is called. If `pattern` is specified then a subset of
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
  .list_module_functions("ppc",
                         .pattern = pattern,
                         fixed = fixed,
                         invert = invert)
}

#' @rdname available_ppc
#' @export
available_mcmc <- function(pattern, fixed = FALSE, invert = FALSE) {
  .list_module_functions("mcmc",
                         .pattern = pattern,
                         fixed = fixed,
                         invert = invert)
}

#' @export
print.bayesplot_function_list <- function(x, ...) {
  atts <- attributes(x)
  cat("bayesplot", toupper(atts[["module"]]), "module:\n")
  if (!is.null(atts[["pattern"]])) {
    msg <- paste0("(", ifelse(atts[["inverted"]], "excluding", "matching"),
                  " pattern '", atts[["pattern"]], "')")
    cat(msg, "\n")
  }
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
    if (missing(.pattern)) {
      .pattern <- NULL
    }

    all_funs <- grep(
      pattern = paste0("^", .module, "_"),
      x = getNamespaceExports("bayesplot"),
      value = TRUE
    )
    return_funs <- sort(all_funs)

    if (!is.null(.pattern)) {
      return_funs <- grep(
        pattern = .pattern,
        x = return_funs,
        value = TRUE,
        fixed = fixed,
        invert = invert
      )
    }
    structure(
      return_funs,
      class = c("bayesplot_function_list", "character"),
      module = .module,
      pattern = .pattern,
      inverted = invert
    )
  }
