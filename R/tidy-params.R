#' Tidy parameter selection
#'
#' @name tidy-params
#'
#' @description As of version `1.7.0`, **bayesplot** allows the `pars` argument
#'   for [MCMC plots][bayesplot::MCMC-overview] to be used for so-called 'tidy'
#'   variable selection (in the style of the **dplyr** package).
#'   The [`vars()`][dplyr::vars] function is re-exported from **dplyr**
#'   for this purpose. See the **Examples** section, below.
#'
#'   When using `pars` for tidy parameter selection, the `regex_pars` argument
#'   is ignored because **bayesplot** supports using
#'   [tidyselect helper functions][tidyselect::select_helpers]
#'   (`starts_with()`, `contains()`, `num_range()`, etc.) for the same purpose.
#'   **bayesplot** also exports some additional helper functions
#'   to help with parameter selection:
#'   * `param_range()`: like [tidyselect::num_range()] but used when parameter
#'     indexes are in brackets (e.g. `beta[2]`).
#'   * `param_glue()`: for more complicated parameter names with multiple
#'     indexes (including variable names) inside the brackets
#'     (e.g., `beta[(Intercept) age_group:3]`).
#'
#'   These functions can be used inside of `vars()`, `dplyr::select()`,
#'   and similar functions just like the
#'   [tidyselect helper functions][tidyselect::select_helpers].
#'   See the **Examples** section.
#'
#' @examples
#' x <- example_mcmc_draws(params = 6)
#' dimnames(x)
#' mcmc_hex(x, pars = vars(alpha, `beta[2]`))
#' mcmc_dens(x, pars = vars(sigma, contains("beta")))
#' mcmc_hist(x, pars = vars(-contains("beta")))
#'
#' # using the param_range() helper
#' mcmc_hist(x, pars = vars(param_range("beta", c(1, 3, 4))))
#'
#' \donttest{
#' #############################
#' ## Examples using rstanarm ##
#' #############################
#' if (requireNamespace("rstanarm", quietly = TRUE)) {
#'   # see ?rstanarm::example_model
#'   fit <- example("example_model", package = "rstanarm", local=TRUE)$value
#'   print(fit)
#'   posterior <- as.data.frame(fit)
#'   str(posterior)
#'
#'   color_scheme_set("brightblue")
#'   mcmc_hist(posterior, pars = vars(size, contains("period")))
#'
#'   # same as previous but using dplyr::select() and piping
#'   library("dplyr")
#'   posterior %>%
#'     select(size, contains("period")) %>%
#'     mcmc_hist()
#'
#'   mcmc_intervals(posterior, pars = vars(contains("herd")))
#'   mcmc_intervals(posterior, pars = vars(contains("herd"), -contains("Sigma")))
#'
#'   bayesplot_theme_set(ggplot2::theme_dark())
#'   color_scheme_set("viridisC")
#'   mcmc_areas_ridges(posterior, pars = vars(starts_with("b[")))
#'
#'   bayesplot_theme_set()
#'   color_scheme_set("purple")
#'   not_789 <- vars(starts_with("b["), -matches("[7-9]"))
#'   mcmc_intervals(posterior, pars = not_789)
#'
#'   # using the param_glue() helper
#'   just_149 <- vars(param_glue("b[(Intercept) herd:{level}]", level = c(1,4,9)))
#'   mcmc_intervals(posterior, pars = just_149)
#'
#'   # same but using param_glue() with dplyr::select()
#'   # before passing to bayesplot
#'   posterior %>%
#'     select(param_glue("b[(Intercept) herd:{level}]",
#'                       level = c(1, 4, 9))) %>%
#'     mcmc_intervals()
#' }
#'}
#'
NULL

# re-export vars for tidy parameter selection
#' @importFrom dplyr vars
#' @export
dplyr::vars

#' @rdname tidy-params
#' @export
#' @param vars `NULL` or a character vector of parameter names to choose from.
#'   This is only needed for the atypical use case of calling the function as a
#'   standalone function outside of `vars()`, `select()`, etc. Typically this is
#'   left as `NULL` and will be set automatically for the user.
#' @param prefix,range For `param_range()` only, `prefix` is a string naming a
#'   parameter and `range` is an integer vector providing the indices of a
#'   subset of elements to select. For example, using
#'
#'       param_range("beta", c(1,2,8))
#'
#'   would select parameters named `beta[1]`, `beta[2]`, and `beta[8]`.
#'   `param_range()` is only designed for the case that the indices are integers
#'   surrounded by brackets. If there are no brackets use
#'   [num_range()][tidyselect::select_helpers].
#'
param_range <- function(prefix, range, vars = NULL) {
  if (!is.null(vars) && !is.character(vars)) {
    abort("'vars' must be NULL or a character vector.")
  }
  nms <- paste0(prefix, "[", range, "]")
  param_matches <- match(nms, vars %||% tidyselect::peek_vars())
  param_matches[!is.na(param_matches)]
}

#' @rdname tidy-params
#' @export
#' @param pattern,... For `param_glue()` only, `pattern` is a string containing
#'   expressions enclosed in braces and `...` should be named arguments
#'   providing one character vector per expression in braces in `patten`.
#'   It easiest to describe how to use these arguments with an example:
#'
#'     param_glue("beta_{var}[{level}]",
#'                var = c("age", "income"),
#'                level = c(3,8))
#'
#'   would select parameters with names
#'   `"beta_age[3]"`, `"beta_income[3]"`, `"beta_age[8]"`, `"beta_income[8]"`.
#'   See the **Examples** section below for demonstrations.
#'
#' @examples
#' \dontrun{
#' ###################################
#' ## More examples of param_glue() ##
#' ###################################
#' library(dplyr)
#' posterior <-
#'  structure(list(
#'    b_Intercept = rnorm(1000),
#'    sd_condition__Intercept = rexp(1000),
#'    sigma = rexp(1000),
#'    `r_condition[A,Intercept]` = rnorm(1000),
#'    `r_condition[B,Intercept]` = rnorm(1000),
#'    `r_condition[C,Intercept]` = rnorm(1000),
#'    `r_condition[A,Slope]` = rnorm(1000),
#'    `r_condition[B,Slope]` = rnorm(1000)
#'   ),
#'   class = c("tbl_df", "tbl", "data.frame"),
#'   row.names = c(NA, -1000L)
#'   )
#' str(posterior)
#'
#' # using one expression in braces
#' posterior %>%
#'   select(
#'     param_glue(
#'       "r_condition[{level},Intercept]",
#'       level = c("A", "B"))
#'   ) %>%
#'   mcmc_hist()
#'
#' # using multiple expressions in braces
#' posterior %>%
#'    select(
#'      param_glue(
#'        "r_condition[{level},{type}]",
#'         level = c("A", "B"),
#'         type = c("Intercept", "Slope"))
#'    ) %>%
#'    mcmc_hist()
#'}
#'
param_glue <- function(pattern, ..., vars = NULL) {
  if (!is.null(vars) && !is.character(vars)) {
    abort("'vars' must be NULL or a character vector.")
  }
  dots <- as.list(expand.grid(...))
  nms <- as.character(glue::glue_data(dots, pattern))
  param_matches <- match(nms, vars %||% tidyselect::peek_vars())
  param_matches[!is.na(param_matches)]
}


# internal ----------------------------------------------------------------

#' Internal function for tidy parameter selection
#'
#' This function is called internally by `prepare_mcmc_array()` if the user's
#' `pars` argument is a quosure.
#'
#' @noRd
#' @md
#' @param complete_pars A character vector of *all* parameter names.
#' @param pars_list A list of columns generated by `vars()`.
#' @return Character vector of selected parameter names.
#'
tidyselect_parameters <- function(complete_pars, pars_list) {
  helpers <- tidyselect::vars_select_helpers
  pars_list <- lapply(pars_list, rlang::env_bury, !!! helpers)
  selected <- tidyselect::vars_select(.vars = complete_pars, !!! pars_list)
  if (!length(selected)) {
    stop("No parameters were found matching those names.", call. = FALSE)
  }
  return(unname(selected))
}
