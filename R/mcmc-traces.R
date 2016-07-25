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
#' @template seealso-color-scheme
#'
#' @examples
#' # some fake parameter draws to use for demonstration
#' x <- fake_draws(n = 500, chains = 4, params = 6)
#' dim(x)
#' dimnames(x)
#'
#' mcmc_trace(x)
#' mcmc_trace_highlight(x, chain = 2)
#'
#' # use traditional ggplot discrete color scale
#' mcmc_trace(x) + ggplot2::scale_color_hue()
#'
#' # zoom in on a window of iterations
#' mcmc_trace(x, window = c(100, 200))
#'
#' # parse facet label text
#' (p <- mcmc_trace(
#'   x,
#'   regex_pars = "beta\\[[1,3]\\]",
#'   facet_args = list(labeller = ggplot2::label_parsed)
#' ))
#' # plot with bigger facet fontsize and add tick marks
#' p +
#'  facet_text(size = 15) +
#'  xaxis_ticks(size = .25)
#'
#' # mark first 200 draws as warmup
#' mcmc_trace(x, n_warmup = 200)
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
#' @param highlight For \code{mcmc_trace_highlight}, an integer specifying one
#'   of the chains that will be more visible than the others in the plot.
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
    if (!has_multiple_chains(x))
      STOP_need_multiple_chains()

    if (!highlight %in% seq_len(ncol(x)))
      stop(
        "'highlight' is ", highlight,
        ", but 'x' contains ", ncol(x), " chains."
      )
  }

  data <- reshape2::melt(x, value.name = "Value")
  data$Chain <- factor(data$Chain)
  n_chain <- length(unique(data$Chain))
  n_iter <- length(unique(data$Iteration))

  geom_args <- list()
  geom_args$size <- size %||% ifelse(style == "line", 1/3, 1)

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
               # fill = NA,
               size = 2,
               color = "gray95",
               fill = "gray95",
               alpha = 0.5)
  }

  if (!is.null(window)) {
    stopifnot(length(window) == 2)
    graph <- graph + coord_cartesian(xlim = window)
  }

  graph <- graph +
    do.call(paste0("geom_", style), geom_args) +
    theme_default(
      x_lab = FALSE,
      y_lab = FALSE,
      legend_position = if (nlevels(data$Chain) > 1) "right" else "none"
    )

  if (!is.null(highlight)) {
    graph <- graph +
      scale_alpha_discrete(range = c(.2, 1), guide = "none") +
      scale_color_manual("",
                         values = get_color(c("l", "d")),
                         labels = c("Other chains", paste("Chain", highlight)))
  } else {
    graph <- graph +
      scale_color_manual("Chain", values = chain_colors(n_chain))
  }

  facet_args$facets <- ~ Parameter
  if (is.null(facet_args$scales))
    facet_args$scales <- "free"

  graph + do.call("facet_wrap", facet_args)
}

chain_colors <- function(n) {
  all_clrs <- unlist(get_color_scheme())
  clrs <- switch(
    as.character(n),
    "1" = get_color("m"),
    "2" = get_color(c("l", "d")),
    "3" = get_color(c("l", "m", "d")),
    "4" = all_clrs[-c(3, 5)],
    "5" = all_clrs[-3],
    "6" = all_clrs,
    rep_len(all_clrs, n)
  )
  unname(rev(clrs))
}