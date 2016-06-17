#' Traceplot (time series plot) of MCMC draws
#'
#' @family MCMC
#'
#' @export
#' @param x Posterior draws.
#' @template args-pars
#' @template args-regex_pars
#' @param size An optional value to override \code{\link[ggplot2]{geom_line}}'s
#'   default line size.
#' @param n_warmup The number of warmup iterations included in \code{x}.
#' @param inc_warmup Include warmup iterations in the plot?
#' @param window Integer vector of length two specifying the limits of a range
#'   of iterations to display.
#' @param facet_args Arguments (other than \code{facets}) passed to
#'   \code{\link[ggplot2]{facet_wrap}} to control faceting.
#' @param ... Currently unused.
#'
#' @template return-ggplot
#'
mcmc_trace <- function(x,
                      pars = NULL,
                      regex_pars = NULL,
                      size = NULL,
                      n_warmup = 0,
                      inc_warmup = n_warmup > 0,
                      window = NULL,
                      facet_args = list(),
                      ...) {
  UseMethod("mcmc_trace")
}

#' @rdname mcmc_trace
#' @method mcmc_trace array
#' @export
mcmc_trace.array <- function(x,
                            pars = NULL,
                            regex_pars = NULL,
                            size = NULL,
                            n_warmup = 0,
                            inc_warmup = n_warmup > 0,
                            window = NULL,
                            facet_args = list(),
                            ...) {
  if (length(dim(x)) == 2)
    return(mcmc_trace.matrix(as.matrix(x)))

  if (!is.null(dimnames(x)[[3]])) {
    parnames <- dimnames(x)[[3]]
  } else {
    stop("No parameter names found.")
  }

  dimnames(x) <- list(
    Iteration = seq_len(nrow(x)),
    Chain = seq_len(ncol(x)),
    Parameter = parnames
  )

  if (is.null(pars))
    pars <- parnames[1]
  if (!is.null(regex_pars))
    regex_pars <- grep(regex_pars, parnames, value = TRUE)

  sel <- unique(c(pars, regex_pars))
  x <- x[, , sel, drop = FALSE]

  if (!inc_warmup && n_warmup > 0)
    x <- x[-seq_len(n_warmup), , , drop = FALSE]

  data <- reshape2::melt(x, value.name = "Value")
  data$Chain <- factor(data$Chain)

  geom_args <- list()
  if (!is.null(size))
    geom_args$size <- size

  graph <-
    ggplot(data, aes_(x = ~ Iteration, y = ~ Value, color = ~ Chain))

  if (inc_warmup && n_warmup > 0) {
    graph <- graph +
      annotate("rect", xmin = -Inf, xmax = n_warmup,
               ymin = -Inf, ymax = Inf, fill = "lightgray")
  }

  if (!is.null(window))
    graph <- graph + coord_cartesian(xlim = window)

  graph <- graph +
    do.call("geom_line", geom_args) +
    theme_ppc(x_lab = FALSE, legend_position = "right")

  facet_args$facets <- ~ Parameter
  if (is.null(facet_args$scales))
    facet_args$scales <- "free_y"

  graph + do.call("facet_wrap", facet_args)
}

#' @rdname mcmc_trace
#' @method mcmc_trace matrix
#' @export
mcmc_trace.matrix <- function(x,
                             pars = NULL,
                             regex_pars = NULL,
                             size = NULL,
                             n_warmup = 0,
                             inc_warmup = n_warmup > 0,
                             window = NULL,
                             facet_args = list(),
                             ...) {
  A <- array(x, dim = c(nrow(x), 1, ncol(x)))
  dimnames(A) <- list(
    Iteration = NULL,
    Chain = NULL,
    Parameter = colnames(x)
  )

  mcmc_trace.array(A,
                  pars,
                  regex_pars,
                  size,
                  n_warmup,
                  inc_warmup,
                  window,
                  facet_args,
                  ...)
}


#' @rdname mcmc_trace
#' @method mcmc_trace data.frame
#' @export
mcmc_trace.data.frame <- function(x,
                                 pars = NULL,
                                 regex_pars = NULL,
                                 size = NULL,
                                 n_warmup = 0,
                                 inc_warmup = n_warmup > 0,
                                 window = NULL,
                                 facet_args = list(),
                                 ...) {
  mcmc_trace.matrix(as.matrix(x),
                   pars,
                   regex_pars,
                   size,
                   n_warmup,
                   inc_warmup,
                   window,
                   facet_args,
                   ...)
}
