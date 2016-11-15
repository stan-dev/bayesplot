#' Get or view the names of available plotting functions
#'
#' @export
#' @param pattern An optional \link[=regex]{regular expression}.
#' @return A possibly empty character vector of function names with several
#'   additional attributes (for use by a custom print method). If \code{pattern}
#'   is missing then the returned object contains the names of all available
#'   plotting functions in the \link{MCMC} or \link{PPC} module, depending on
#'   which function is called. If \code{pattern} is specified then the subset of
#'   function names matching pattern is returned.
#'
#' @examples
#' available_mcmc()
#' available_mcmc("nuts")
#' available_mcmc("rhat|neff")
#' available_ppc("grouped")
#'
available_ppc <- function(pattern) {
  .list_module_functions("ppc", .pattern = pattern)
}

#' @rdname available_ppc
#' @export
available_mcmc <- function(pattern) {
  .list_module_functions("mcmc", .pattern = pattern)
}

#' @export
print.bayesplot_function_list <- function(x, ...) {
  atts <- attributes(x)
  cat("bayesplot", toupper(atts[["module"]]), "module:\n")
  if (!is.null(atts[["pattern"]]))
    cat(paste0("(matching pattern '", atts[["pattern"]], "')"), "\n")

  cat(paste0("  ", x), sep = "\n")
  invisible(x)
}


# internal ----------------------------------------------------------------
.list_module_functions <- function(.module = c("ppc", "mcmc"), .pattern) {
  .module <- match.arg(.module)
  if (missing(.pattern))
    .pattern <- NULL

  funs <- objects("package:bayesplot", pattern = paste0("^", .module, "_"))
  structure(
    .Data = if (is.null(.pattern))
      funs else grep(.pattern, funs, value = TRUE),
    class = c("bayesplot_function_list", "character"),
    module = .module,
    pattern = .pattern
  )
}
