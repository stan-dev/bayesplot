#' @param K An optional integer defining the number of equally spaced evaluation
#'  points for the PIT-ECDF. Reducing K when using `interpolate_adj = FALSE`
#'  makes computing the confidence bands faster. For `ppc_pit_ecdf` and
#'  `ppc_pit_ecdf_grouped`, if PIT values are supplied, defaults to
#'  `length(pit)`, otherwise yrep determines the maximum accuracy of the
#'  estimated PIT values and `K` is set to `min(nrow(yrep) + 1, 1000)`. For
#'  `mcmc_rank_ecdf`, defaults to the number of iterations per chain in `x`.
#' @param prob The desired simultaneous coverage level of the bands around the
#'   ECDF. A value in (0,1).
#' @param plot_diff A boolean defining whether to plot the difference between
#'   the observed PIT- ECDF and the theoretical expectation for uniform PIT
#'   values rather than plotting the regular ECDF. The default is `FALSE`, but
#'   for large samples we recommend setting `plot_diff=TRUE` as the difference
#'   plot will visually show a more dynamic range.
#' @param interpolate_adj A boolean defining if the simultaneous confidence
#'   bands should be interpolated based on precomputed values rather than
#'   computed exactly. Computing the bands may be computationally intensive and
#'   the approximation gives a fast method for assessing the ECDF trajectory.
#'   The default is to use interpolation if `K` is greater than 200.
