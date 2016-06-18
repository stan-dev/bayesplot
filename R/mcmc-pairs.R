#' Pairs plot of MCMC draws
#'
#' @family MCMC
#' @name MCMC-pairs
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
NULL

#' @rdname MCMC-pairs
#' @export
mcmc_pairs <- function(x, ...) {
  UseMethod("mcmc_pairs")
}

#' @rdname MCMC-pairs
#' @method mcmc_pairs array
#' @export
mcmc_pairs.array <- function(x,
                            pars = NULL,
                            regex_pars = NULL,
                            ...) {
  if (!requireNamespace("GGally", quietly = TRUE))
    stop("Please install the GGally package to use this function.")

  if (length(dim(x)) == 2)
    return(mcmc_pairs.matrix(as.matrix(x)))

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

#' @rdname MCMC-pairs
#' @method mcmc_pairs matrix
#' @export
mcmc_pairs.matrix <- function(x,
                             pars = NULL,
                             regex_pars = NULL,
                             ...) {
  A <- array(x, dim = c(nrow(x), 1, ncol(x)))
  dimnames(A) <- list(
    Iteration = NULL,
    Chain = NULL,
    Parameter = colnames(x)
  )

  mcmc_pairs.array(A,
                  pars,
                  regex_pars,
                  ...)
}


#' @rdname MCMC-pairs
#' @method mcmc_pairs data.frame
#' @export
mcmc_pairs.data.frame <- function(x,
                                 pars = NULL,
                                 regex_pars = NULL,
                                 ...) {
  mcmc_pairs.matrix(as.matrix(x),
                   pars,
                   regex_pars,
                   ...)
}
