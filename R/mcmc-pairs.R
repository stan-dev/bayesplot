#' Pairs plot of MCMC draws
#'
#' @family MCMC
#' @aliases MCMC-pairs
#'
#' @param x Posterior draws.
#' @template args-pars
#' @template args-regex_pars
#' @param ... Currently ignored.
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
      continuous = GGally::wrap("points",
                        shape = 21,
                        fill = get_color("dark"),
                        color = get_color("dark_highlight"),
                        alpha = 0.5
                        )
    ),
    diag = list(
      continuous = GGally::wrap("barDiag",
                        fill = get_color("mid"),
                        color = get_color("mid_highlight"),
                        size = .25)
      )
  )

  graph + theme_ppc()
}
