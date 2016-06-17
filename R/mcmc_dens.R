#' Kernel density plot of posterior draws
#'
#' @family MCMC
#'
#' @export
#' @param x Posterior draws.
#' @param pars Parameter names.
#' @param regex_pars Regular expression.
#' @param by_chain Separate by chain or merge chains?
#' @param ... Passed to \code{\link[ggplot2]{facet_wrap}} or
#'   \code{\link[ggplot2]{facet_grid}} (e.g. \code{scales}).
#'
#' @template return-ggplot
#'
mcmc_dens <- function(x,
                      pars = NULL,
                      regex_pars = NULL,
                      by_chain = FALSE,
                      transform = NULL,
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
    Parameter = dimnames(x)[[3]]
  )

  if (!is.null(regex_pars))
    regex_pars <- grep(regex_pars, dimnames(x)[[3]], value = TRUE)

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

  if (!by_chain)
    graph + facet_wrap( ~ Parameter, ...)
  else
    graph + facet_grid(Parameter ~ Chain, ...)
}

#' @rdname mcmc_dens
#' @method mcmc_dens matrix
#' @export
mcmc_dens.matrix <- function(x,
                             pars = NULL,
                             regex_pars = NULL,
                             by_chain = FALSE,
                             transform = NULL,
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
                                 ...) {
  mcmc_dens.matrix(as.matrix(x),
                   pars,
                   regex_pars,
                   by_chain,
                   transform,
                   ...)
}
