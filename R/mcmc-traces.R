#' Traceplot (time series plot) of MCMC draws
#'
#' @name MCMC-traces
#' @family MCMC
#'
#' @template args-mcmc-x
#' @template args-pars
#' @template args-regex_pars
#' @param size An optional value to override the default line size (if calling
#'   \code{mcmc_trace}) or the default point size (if calling
#'   \code{mcmc_trace_highlight}).
#' @param n_warmup An integer; the number of warmup iterations included in
#'   \code{x}.
#' @param inc_warmup A logical value indicating if warmup iterations should be
#'   included in the plot if \code{n_warmup > 0}.
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
                       n_warmup = 0,
                       inc_warmup = n_warmup > 0,
                       window = NULL,
                       size = NULL,
                       facet_args = list(),
                       ...) {
  .mcmc_trace(
    x,
    pars = pars,
    regex_pars = regex_pars,
    n_warmup = n_warmup,
    inc_warmup = inc_warmup,
    window = window,
    size = size,
    facet_args = facet_args,
    style = "line",
    ...
  )
}

#' @rdname MCMC-traces
#' @export
#' @param highlight_chain An integer specifying one of the chains that will be
#'   more visible than the others in the plot.
mcmc_trace_highlight <- function(x,
                              pars = character(),
                              regex_pars = character(),
                              n_warmup = 0,
                              inc_warmup = n_warmup > 0,
                              window = NULL,
                              size = NULL,
                              facet_args = list(),
                              highlight_chain = 1,
                              ...) {
  if (length(dim(x)) != 3)
    stop("mcmc_trace_highlight requires 'x' contain multiple chains.")

  .mcmc_trace(
    x,
    pars = pars,
    regex_pars = regex_pars,
    n_warmup = n_warmup,
    inc_warmup = inc_warmup,
    window = window,
    size = size,
    facet_args = facet_args,
    highlight_chain = highlight_chain,
    style = "point",
    ...
  )
}


.mcmc_trace <- function(x,
                       pars = character(),
                       regex_pars = character(),
                       n_warmup = 0,
                       inc_warmup = n_warmup > 0,
                       window = NULL,
                       size = NULL,
                       facet_args = list(),
                       highlight_chain = NULL,
                       style = c("line", "point"),
                       ...) {

  x <- prepare_mcmc_array(x)
  pars <- select_parameters(explicit = pars,
                            patterns = regex_pars,
                            complete = dimnames(x)[[3]])
  x <- x[, , pars, drop = FALSE]

  if (!is.null(highlight_chain)) {
    if (!highlight_chain %in% seq_len(ncol(x)))
      stop(
        "'highlight_chain' is ", highlight_chain,
        ", but 'x' contains ", ncol(x), " chains."
      )
  }

  if (!inc_warmup && n_warmup > 0)
    x <- x[-seq_len(n_warmup), , , drop = FALSE]

  data <- reshape2::melt(x, value.name = "Value")
  data$Chain <- factor(data$Chain)

  geom_args <- list()
  if (!is.null(size))
    geom_args$size <- size

  if (is.null(highlight_chain)) {
    mapping <- aes_(x = ~ Iteration, y = ~ Value, color = ~ Chain)
  } else {
    stopifnot(length(highlight_chain) == 1)
    mapping <- aes_(x = ~ Iteration, y = ~ Value, color = ~ Chain,
                    alpha = ~ Chain == highlight_chain)
  }
  graph <- ggplot(data, mapping)

  if (inc_warmup && n_warmup > 0) {
    graph <- graph +
      annotate("rect", xmin = -Inf, xmax = n_warmup,
               ymin = -Inf, ymax = Inf, fill = "lightgray")
  }

  if (!is.null(window)) {
    stopifnot(length(window) == 2)
    graph <- graph + coord_cartesian(xlim = window)
  }

  if (!is.null(highlight_chain)) {
    graph <- graph +
      scale_alpha_discrete("", range = c(.2, 1), guide = "none")
  }

  graph <- graph +
    do.call(paste0("geom_", match.arg(style)), geom_args) +
    theme_ppc(legend_position =
                if (nlevels(data$Chain) > 1) "right" else "none")

  facet_args$facets <- ~ Parameter
  if (is.null(facet_args$scales))
    facet_args$scales <- "free_y"

  graph + do.call("facet_wrap", facet_args)
}
