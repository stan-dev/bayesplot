#' Get or view the names of available plotting or data functions
#'
#' @export
#' @param pattern,fixed,invert Passed to [base::grep()].
#' @param plots_only If `TRUE` (the default) only plotting functions are
#'   searched for. If `FALSE` then functions that return data for plotting
#'   (functions ending in `_data()`) are also included.
#' @return A possibly empty character vector of function names with several
#'   additional attributes (for use by a custom print method). If `pattern`
#'   is missing then the returned object contains the names of all available
#'   plotting functions in the [MCMC], [PPC], or [PPD] module, depending on
#'   which function is called. If `pattern` is specified then a subset of
#'   function names is returned.
#'
#' @examples
#' available_mcmc()
#' available_mcmc("nuts")
#' available_mcmc("rhat|neff")
#'
#' available_ppc()
#' available_ppc("grouped")
#' available_ppc("grouped", invert = TRUE)
#'
#' available_ppd()
#' available_ppd("grouped")
#'
#' # can also see which functions that return data are available
#' available_ppc(plots_only = FALSE)
#'
#' # only show the _data functions
#' available_ppc("_data", plots_only = FALSE)
#' available_ppd("_data", plots_only = FALSE)
#' available_mcmc("_data", plots_only = FALSE)
#'
available_ppc <-
  function(pattern = NULL,
           fixed = FALSE,
           invert = FALSE,
           plots_only = TRUE) {
    .list_module_functions(
      .module = "ppc",
      .pattern = pattern,
      fixed = fixed,
      invert = invert,
      plots_only = plots_only
    )
  }

#' @rdname available_ppc
#' @export
available_ppd <-
  function(pattern = NULL,
           fixed = FALSE,
           invert = FALSE,
           plots_only = TRUE) {
    .list_module_functions(
      .module = "ppd",
      .pattern = pattern,
      fixed = fixed,
      invert = invert,
      plots_only = plots_only
    )
  }

#' @rdname available_ppc
#' @export
available_mcmc <-
  function(pattern = NULL,
           fixed = FALSE,
           invert = FALSE,
           plots_only = TRUE) {
    .list_module_functions(
      .module = "mcmc",
      .pattern = pattern,
      fixed = fixed,
      invert = invert,
      plots_only = plots_only
    )
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
  function(.module = c("ppc", "ppd", "mcmc"),
           .pattern,
           fixed = FALSE,
           invert = FALSE,
           plots_only = TRUE) {

    .module <- match.arg(.module)

    all_funs <- grep(
      pattern = paste0("^", .module, "_"),
      x = getNamespaceExports("bayesplot"),
      value = TRUE
    )
    return_funs <- sort(all_funs)

    if (plots_only) {
      # drop _data() functions
      return_funs <-
        grep(
          pattern = "_data()",
          x = return_funs,
          invert = TRUE,
          value = TRUE
        )
    }

    if (!is.null(.pattern)) {
      return_funs <- grep(
        pattern = .pattern,
        x = return_funs,
        value = TRUE,
        fixed = fixed,
        invert = invert
      )
    }

    # remove deprecated functions
    return_funs <- setdiff(return_funs, "ppc_loo_pit")

    structure(
      return_funs,
      class = c("bayesplot_function_list", "character"),
      module = .module,
      pattern = .pattern,
      inverted = invert
    )
  }
