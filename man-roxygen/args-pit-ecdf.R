#' @param K An optional integer defining the number of equally spaced evaluation
#' points for the ECDF. If the submitted PIT values are known to be discrete,
#' this should be the number of the discrete cases. Defaults to the smaller of
#' length(y) and ncol(yrep).
#' @param prob The desired simultaneous coverage level of the bands around the
#' ECDF. A value in (0,1).
#' @param plot_diff A boolean defining wether to plot the difference between the
#' observed ECDF and the theoretical expectation for uniform PIT values.
#' @param interpolate_adj A boolean defining if the simultaneous confidence
#' bands should be computed or interpolated from precomputed values. Computing
#' the bands may be computationally intensive and approximation gives a fast
#' method for assessing the ECDF trajectory.
