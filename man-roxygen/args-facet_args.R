#' @param facet_args A named list of arguments (other than `facets`) passed
#'   to [ggplot2::facet_wrap()] or [ggplot2::facet_grid()]
#'   to control faceting. Note: if `scales` is not included in `facet_args`
#'   then **bayesplot** may use `scales="free"` as the default (depending
#'   on the plot) instead of the **ggplot2** default of `scales="fixed"`.
