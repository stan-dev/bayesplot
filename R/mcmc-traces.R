#' Traceplot (time series plot) of MCMC draws
#'
#' @name MCMC-traces
#' @family MCMC
#'
#' @template args-mcmc-x
#' @template args-pars
#' @template args-regex_pars
#' @template args-transformations
#' @param size An optional value to override the default line size (if calling
#'   \code{mcmc_trace}) or the default point size (if calling
#'   \code{mcmc_trace_highlight}).
#' @param n_warmup An integer; the number of warmup iterations included in
#'   \code{x}. The default is \code{n_warmup = 0}, i.e. to assume no warmup
#'   iterations are included. If \code{n_warmup > 0} then the background for
#'   iterations \code{1:n_warmup} is shaded gray.
#' @param window An integer vector of length two specifying the limits of a
#'   range of iterations to display.
#' @param facet_args Arguments (other than \code{facets}) passed to
#'   \code{\link[ggplot2]{facet_wrap}} to control faceting.
#' @param ... Currently ignored.
#'
#' @template return-ggplot
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{\code{mcmc_trace}}{
#'    Standard traceplot of MCMC draws.
#'   }
#'   \item{\code{mcmc_trace_highlight}}{
#'    Traces are plotted using points rather than lines and the opacity of all
#'    chains but one is reduced.
#'   }
#' }
#'
NULL

#' @rdname MCMC-traces
#' @export
mcmc_trace <- function(x,
                       pars = character(),
                       regex_pars = character(),
                       transformations = list(),
                       facet_args = list(),
                       ...,
                       n_warmup = 0,
                       window = NULL,
                       size = NULL) {
  .mcmc_trace(
    x,
    pars = pars,
    regex_pars = regex_pars,
    transformations = transformations,
    facet_args = facet_args,
    n_warmup = n_warmup,
    window = window,
    size = size,
    style = "line",
    ...
  )
}

#' @rdname MCMC-traces
#' @export
#' @param highlight An integer specifying one of the chains that will be
#'   more visible than the others in the plot.
mcmc_trace_highlight <- function(x,
                                 pars = character(),
                                 regex_pars = character(),
                                 transformations = list(),
                                 facet_args = list(),
                                 ...,
                                 n_warmup = 0,
                                 window = NULL,
                                 size = NULL,
                                 highlight = 1) {
  if (length(dim(x)) != 3)
    stop("mcmc_trace_highlight requires a 3-D array (multiple chains).")

  .mcmc_trace(
    x,
    pars = pars,
    regex_pars = regex_pars,
    transformations = transformations,
    facet_args = facet_args,
    n_warmup = n_warmup,
    window = window,
    size = size,
    highlight = highlight,
    style = "point",
    ...
  )
}


# internal -----------------------------------------------------------------
.mcmc_trace <- function(x,
                       pars = character(),
                       regex_pars = character(),
                       transformations = list(),
                       n_warmup = 0,
                       window = NULL,
                       size = NULL,
                       facet_args = list(),
                       highlight = NULL,
                       style = c("line", "point"),
                       ...) {

  style <- match.arg(style)
  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)

  if (!is.null(highlight)) {
    if (!highlight %in% seq_len(ncol(x)))
      stop(
        "'highlight' is ", highlight,
        ", but 'x' contains ", ncol(x), " chains."
      )
  }

  data <- reshape2::melt(x, value.name = "Value")
  data$Chain <- factor(data$Chain)

  geom_args <- list()
  if (!is.null(size))
    geom_args$size <- size

  if (is.null(highlight)) {
    mapping <- aes_(x = ~ Iteration, y = ~ Value, color = ~ Chain)
  } else {
    stopifnot(length(highlight) == 1)
    mapping <- aes_(x = ~ Iteration, y = ~ Value,
                    alpha = ~ Chain == highlight,
                    color = ~ Chain == highlight)
  }
  graph <- ggplot(data, mapping)

  if (n_warmup > 0) {
    graph <- graph +
      annotate("rect",
               xmin = -Inf, xmax = n_warmup,
               ymin = -Inf, ymax = Inf,
               fill = "lightgray")
  }

  if (!is.null(window)) {
    stopifnot(length(window) == 2)
    graph <- graph + coord_cartesian(xlim = window)
  }

  if (!is.null(highlight)) {
    graph <- graph +
      scale_alpha_discrete(range = c(.2, 1), guide = "none") +
      scale_color_manual("", values = get_color(c("light", "dark")),
                         labels = c("Other chains", paste("Chain", highlight)))
  }

  graph <- graph +
    do.call(paste0("geom_", style), geom_args) +
    theme_ppc(legend_position =
                if (nlevels(data$Chain) > 1) "right" else "none")

  facet_args$facets <- ~ Parameter
  if (is.null(facet_args$scales))
    facet_args$scales <- "free_y"

  graph + do.call("facet_wrap", facet_args)
}
