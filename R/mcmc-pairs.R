#' Pairs plot of MCMC draws
#'
#' @name MCMC-pairs
#' @family MCMC
#'
#' @template args-mcmc-x
#' @template args-pars
#' @template args-regex_pars
#' @template args-transformations
#' @param ... Currently ignored.
#'
#' @template return-ggplot
#'
NULL

#' @rdname MCMC-pairs
#' @export
mcmc_pairs <- function(x,
                       pars = character(),
                       regex_pars = character(),
                       transformations = list(),
                       ...) {
  suggested_package("GGally")

  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  user_pars <- parameter_names(x)
  dimnames(x)[[3]] <- rename_pairs_parameters(user_pars)
  if (!has_multiple_params(x))
    STOP_need_multiple_params()

  data <- merge_chains(x)

  message("Processing... this may take a little while")
  graph <- GGally::ggpairs(
    data,
    upper = list(),
    lower = list(
      continuous = GGally::wrap(
        "points",
        shape = 21,
        fill = get_color("dark"),
        color = get_color("dark_highlight"),
        alpha = 0.5
      )
    ),
    diag = list(
      continuous = GGally::wrap(
        "barDiag",
        fill = get_color("mid"),
        color = get_color("mid_highlight"),
        size = .25
      )
    )
  )

  graph <- reset_pairs_parameters(graph, user_pars)
  graph + theme_ppc()
}



# internal ----------------------------------------------------------------
# Remove special characters in parameter names so ggpairs doesn't complain
rename_pairs_parameters <- function(pars) {
  stopifnot(is.character(pars))
  pars <- gsub("\\[|\\:", "_", pars)
  pars <- gsub("\\(|\\)|\\]", "", pars)
  gsub(" ", "_", pars)
}

# Reset axis labels to original parameter names
reset_pairs_parameters <- function(ggmatrix, pars) {
  ggmatrix$xAxisLabels <- pars
  ggmatrix$yAxisLabels <- pars
  ggmatrix
}
