#' @param bw,adjust,kernel,n_dens,bounds Optional arguments passed to
#'   [stats::density()] (and `bounds` to [ggplot2::stat_density()]) to override
#'   default kernel density estimation parameters or truncate the density
#'   support. `n_dens` defaults to `1024`.
