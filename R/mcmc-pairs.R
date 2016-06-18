#' Pairs plot of MCMC draws
#'
#' @family MCMC
#' @aliases MCMC-pairs
#'
#' @param x Posterior draws.
#' @template args-pars
#' @template args-regex_pars
#' @param size An optional value to override \code{\link[ggplot2]{geom_line}}'s
#'   default line size.
#' @param facet_args Arguments (other than \code{facets}) passed to
#'   \code{\link[ggplot2]{facet_grid}} to control faceting.
#' @param ... For the generic, arguments passed to the various methods. For the
#'   methods themselves \code{...} is ignored.
#'
#' @template return-ggplot
#'
mcmc_pairs <- function(x,
                       pars = NULL,
                       regex_pars = NULL,
                       ...) {

  if (!requireNamespace("GGally", quietly = TRUE))
    stop("Please install the GGally package to use this function.")

  if (is.data.frame(x))
    x <- as.matrix(x)
  if (length(dim(x)) == 2) {
    x <- array(x, dim = c(nrow(x), 1, ncol(x)))
    dimnames(x) <- list(
      Iteration = NULL,
      Chain = NULL,
      Parameter = colnames(x)
    )
  }

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

  xdim <- dim(x)
  data <- as.data.frame(array(x, dim = c(prod(xdim[1:2]), xdim[3])))
  colnames(data) <- sel

  message("Processing... this may take a little while")
  graph <- GGally::ggpairs(
    data,
    upper = list(),
    lower = list(
      continuous = wrap("points",
                        shape = 21,
                        fill = ppc_color("dark"),
                        color = ppc_color("dark_highlight"),
                        alpha = 0.5
                        )
    ),
    diag = list(
      continuous = wrap("barDiag",
                        fill = ppc_color("mid"),
                        color = ppc_color("mid_highlight"),
                        size = .25)
      )
  )

  graph + theme_ppc()
}
