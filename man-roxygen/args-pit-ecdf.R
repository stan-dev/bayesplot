#' @param K An optional integer defining the number of equally spaced evaluation
#' points for the ECDF. If the
#' submitted PIT values are known to be discrete, this should be the number of
#' discrete cases. Defaults to the smaller of length(y) and ncol(yrep).
#' @param prob The desired simultaneous coverage level of the bands around the
#' ECDF. A value in (0,1).
#' @param plot_diff Wether to plot the difference between the observed ECDF and
#' the theoretical expectation when the PIT values are uniform.
#' @param adj_method Method to obtain the simultaneous confidence bands.
#' Possible values: "interpolate", "simulate", "optimize".
#' Defaults to "interpolate".
