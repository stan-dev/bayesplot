#' @param y A vector of observations. See **Details**.
#' @param yrep An `S` by `N` matrix of draws from the posterior (or prior)
#'   predictive distribution. The number of rows, `S`, is the size of the
#'   posterior (or prior) sample used to generate `yrep`. The number of columns,
#'   `N` is the number of predicted observations (`length(y)`). The columns of
#'   `yrep` should be in the same order as the data points in `y` for the plots
#'   to make sense. See the **Details** and **Plot Descriptions** sections for
#'   additional advice specific to particular plots.
