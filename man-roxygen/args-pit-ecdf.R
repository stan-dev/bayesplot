#' @param K An optional integer defining the number of equally spaced evaluation
#'   points for the ECDF. If the submitted PIT values are known to be discrete,
#'   this should be the number of the discrete cases. Defaults to the smaller of
#'   `length(y)` and `ncol(yrep)` when applicable.
#' @param prob The desired simultaneous coverage level of the bands around the
#'   ECDF. A value in (0,1).
#' @param plot_diff A boolean defining whether to plot the difference between
#'   the observed ECDF and the theoretical expectation for uniform PIT values
#'   rather than plotting the regular ECDF. The default is `FALSE`, but for
#'   large samples we recommend setting `plot_diff=TRUE` as the difference plot
#'   will visually show a more dynamic range.
#' @param interpolate_adj A boolean defining if the simultaneous confidence
#'   bands should be interpolated based on precomputed values rather than
#'   computed exactly. Computing the bands may be computationally intensive and
#'   the approximation gives a fast method for assessing the ECDF trajectory.
#'   The default is to use interpolation if `K` is greater than 200.
