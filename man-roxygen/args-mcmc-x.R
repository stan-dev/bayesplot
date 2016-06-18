#' @param x A 3-D array, matrix, or data frame of posterior draws. If \code{x}
#'   is a 3-D array then the dimensions must be \code{[Iteration, Chain,
#'   Parameter]} in that order. If there is only one Markov chain or if all
#'   chains have already been merged, then \code{x} can be a matrix or
#'   data.frame with a column for each parameter.
