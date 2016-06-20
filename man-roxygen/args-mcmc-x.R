#' @param x A 3-D array, matrix, or data frame of posterior draws:
#' \itemize{
#'  \item \strong{3-D array}: An \code{\link{array}} with dimensions
#'  \code{[Iteration, Chain, Parameter]} in that order.
#'  \item \strong{matrix}: A \code{\link{matrix}} with one column per
#'  parameter. If \code{x} is a matrix then there should only be a single
#'  Markov chain or all chains should already be merged (stacked).
#'  \item \strong{data frame}: There are two types of data frames allowed.
#'  Either a data frame with one column per parameter (if only a single chain or
#'  all chains have already been merged), or a data frame with one column per
#'  parameter plus an additional column \code{"Chain"} that contains the chain
#'  number (an integer) corresponding to each row in \code{x}.
#'  \item \strong{list}: A \code{list} of matrices, where each matrix
#'  corresponds to a Markov chain. All of the matrices should have the same
#'  number of iterations (rows) and parameters (columns), and parameters should
#'  have the same names and be in the same order.
#' }
#'
