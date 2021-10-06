#' @param x An object containing MCMC draws:
#' * A 3-D array, matrix, list of matrices, or data frame. The [MCMC-overview]
#' page provides details on how to specify each these.
#' * A `draws` object from the \pkg{\link{posterior}} package (e.g.,
#' `draws_array`, `draws_rvars`, etc.).
#' * An object with an `as.array()` method that returns the same kind of 3-D
#' array described on the [MCMC-overview] page.
#'
