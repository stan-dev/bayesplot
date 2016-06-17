#' Kernel density plot of posterior draws
#'
#' @family MCMC
#'
#' @export
#' @param x Posterior draws.
#' @template args-pars
#' @template args-regex_pars
#' @param by_chain Separate by chain or merge chains?
#' @param facet_args Arguments (other than \code{facets}) passed to
#'   \code{\link[ggplot2]{facet_wrap}} (if \code{by_chain} is \code{FALSE}) or
#'   \code{\link[ggplot2]{facet_grid}} (if \code{by_chain} is \code{TRUE}) to
#'   control faceting.
#' @param ... Currently unused.
#'
#' @template return-ggplot
#'
mcmc_dens <- function(x,
                      pars = NULL,
                      regex_pars = NULL,
                      by_chain = FALSE,
                      transform = NULL,
                      facet_args = list(),
                      ...) {
  UseMethod("mcmc_dens")
}

#' @rdname mcmc_dens
#' @method mcmc_dens array
#' @export
mcmc_dens.array <- function(x,
                            pars = NULL,
                            regex_pars = NULL,
                            by_chain = FALSE,
                            transform = NULL,
                            facet_args = list(),
                            ...) {
  if (length(dim(x)) == 2)
    return(mcmc_dens.matrix(as.matrix(x)))

  if (!is.null(dimnames(x)[[3]])) {
    parnames <- dimnames(x)[[3]]
  } else {
    stop("No parameter names found.")
  }

  dimnames(x) <- list(
    Iteration = NULL,
    Chain = NULL,
    Parameter = parnames
  )

  if (is.null(pars))
    pars <- parnames[1]
  if (!is.null(regex_pars))
    regex_pars <- grep(regex_pars, parnames, value = TRUE)

  sel <- unique(c(pars, regex_pars))
  x <- x[, , sel, drop = FALSE]

  t_x <- if (is.null(transform))
    NULL else match.fun(transform)

  data <- reshape2::melt(x, value.name = "Value")
  if (!is.null(t_x))
    data$Value <- t_x(data$Value)

  graph <- ggplot(data, aes_(x = ~ Value)) +
    geom_histogram(
      fill = ppc_color("mid"),
      color = ppc_color("mid_highlight"),
      size = .25,
      na.rm = TRUE
    ) +
    dont_expand_y_axis() +
    theme_ppc(y_text = FALSE, x_lab = FALSE)

  if (is.null(facet_args$scales))
    facet_args$scales <- "free"

  if (!by_chain) {
    facet_args$facets <- ~ Parameter
    graph + do.call("facet_wrap", facet_args)
  } else {
    facet_args$facets <- Parameter ~ Chain
    graph + do.call("facet_grid", facet_args)
  }
}

#' @rdname mcmc_dens
#' @method mcmc_dens matrix
#' @export
mcmc_dens.matrix <- function(x,
                             pars = NULL,
                             regex_pars = NULL,
                             by_chain = FALSE,
                             transform = NULL,
                             facet_args = list(),
                             ...) {
  A <- array(x, dim = c(nrow(x), 1, ncol(x)))
  dimnames(A) <- list(
    Iteration = NULL,
    Chain = NULL,
    Parameter = colnames(x)
  )

  mcmc_dens.array(A,
                  pars,
                  regex_pars,
                  by_chain,
                  transform,
                  facet_args,
                  ...)
}


#' @rdname mcmc_dens
#' @method mcmc_dens data.frame
#' @export
mcmc_dens.data.frame <- function(x,
                                 pars = NULL,
                                 regex_pars = NULL,
                                 by_chain = FALSE,
                                 transform = NULL,
                                 facet_args = list(),
                                 ...) {
  mcmc_dens.matrix(as.matrix(x),
                   pars,
                   regex_pars,
                   by_chain,
                   transform,
                   facet_args,
                   ...)
}
