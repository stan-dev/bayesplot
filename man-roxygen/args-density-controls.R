#' @param bw,adjust,kernel,n_dens,bounds Optional arguments passed to
#'   [stats::density()] (and `bounds` to [ggplot2::stat_density()]) to override
#'   default kernel density estimation parameters or truncate the density
#'   support. If `NULL` (default), `bw` is set to `"nrd0"`, `adjust` to `1`,
#'   `kernel` to `"gaussian"`, and `n_dens` to `1024`.
