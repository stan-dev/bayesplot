#' @param method The method used to calculate the uniformity test:
#'   * `"independent"`: assumes independent PIT values (Säilynoja et al., 2022).
#'   * `"correlated"`: accounts for correlated PIT values (Tesso & Vehtari, 2026).
#' @param test When `method = "correlated"`, which dependence-aware test to use:
#'   `"POT"`, `"PRIT"`, or `"PIET"`. Defaults to `"POT"`.
#' @param gamma When `method = "correlated"`, tolerance threshold controlling
#'   how strongly suspicious points are flagged. Larger values (`gamma > 0`)
#'   emphasize points with larger deviations. If `NULL`, defaults to `0` and
#'   thus all suspicious points are flagged.
#' @param linewidth When `method = "correlated"`, the line width of the ECDF.
#'   Defaults to `0.3`.
#' @param color When `method = "correlated"`, a vector with base color and
#'   highlight color for the ECDF plot. Defaults to
#'   `c(ecdf = "grey60", highlight = "red")`. The first element is used for
#'   the main ECDF line, the second for highlighted suspicious regions.
#' @param help_text When `method = "correlated"`, a boolean defining whether
#'   to add information about p-value to the plot. Defaults to `TRUE`.
#' @param help_text_shrinkage When `method = "correlated"`, a numeric value
#'   between 0 and 1 defining the factor by which the help-text (p-value
#'   information) is scaled. The default is `0.8`.
#' @param pareto_pit A boolean defining whether to compute PIT values using
#'   Pareto-PIT method. Defaults to `TRUE` if `test` is either `"POT"` or
#'   `"PIET"` and no `pit` values are provided otherwise `FALSE`. This argument
#'   should normally not be modified by the user, except for development
#'   purposes. If `pit` is non-`NULL`, `pareto_pit` cannot be simultaneously
#'   `TRUE`.
