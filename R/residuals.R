#' Residuals
#'
#' \code{ppc_resid} plots the distributions of residuals computed from
#' \eqn{y} and simulated datasets \eqn{y^{rep}}{yrep}. For binomial data,
#' \code{ppc_resid_binned} generates binned residual plots (similar to
#' \code{\link[arm]{binnedplot}}) from \eqn{y} and the posterior draws of the
#' linear predictor transformed by the inverse-link function.
#'
#' @name residuals
#' @family PPCs
#'
#' @template args-ppc
#' @param ... Currently unused.
#'
#' @details
#' \code{ppc_resid} and \code{ppc_resid_binned} compute and plot
#' residuals for each row of the matrices \code{yrep} and \code{Ey},
#' respectively, so it is usually a good idea to \code{yrep} and \code{Ey} to
#' contain only a small number of draws (rows).
#'
#' For binomial and Bernoulli data the \code{ppc_resid_binned} function
#' should be used to generate binned residual plots. Bernoulli data can be input
#' as a vector of 0s and 1s, whereas for binomial data \code{y} should be a
#' vector of "success" proportions (not a matrix of "success" and "failure"
#' counts).
#'
#' @template return-ggplot
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#'
#' @examples
#' y <- rnorm(100)
#' yrep <- matrix(rnorm(2500), ncol = 100)
#' ppc_resid(y, yrep[1:3, ])
#' ppc_resid(y, yrep[10:15, ])
#'
NULL

#' @rdname residuals
#' @export
#'
ppc_resid <- function(y, yrep, ...) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  scheme <- get_color_scheme()

  n <- nrow(yrep)
  if (n == 1) {
    resids <- data.frame(x = y - as.vector(yrep))
    base <- ggplot(resids, aes_string(x = "x"))
    xylabs <- labs(y = NULL, x = "y - yrep")
  } else {
    resids <- melt_yrep(as.matrix(-1 * sweep(yrep, 2L, y)))
    resids$rep_id <- factor(resids$rep_id, labels = paste("y -", unique(resids$rep_id)))
    base <- ggplot(resids, aes_string(x = "value"))
    xylabs <- labs(y = NULL, x = NULL)
  }

  graph <- base +
    geom_histogram(
      mapping = aes_string(y = "..density.."),
      size = 0.25,
      fill = scheme[["dark"]],
      color = scheme[["dark_highlight"]]
    ) +
    xylabs +
    theme_ppc(y_text = FALSE)

  if (n > 1)
    graph <- graph + facet_wrap("rep_id", switch = "x")

  graph
}

#' @rdname residuals
#' @export
#' @param Ey A matrix of posterior draws of the linear predictor transformed by
#'   the inverse-link function.
#'
ppc_resid_binned <- function(y, Ey, ...) {
  if (!requireNamespace("arm", quietly = TRUE))
    stop("Please install the 'arm' package.")

  y <- validate_y(y)
  yrep <- validate_yrep(Ey, y)

  scheme <- get_color_scheme()
  line_color <- scheme[["light"]]
  line_size <- 1
  pt_fill <- scheme[["dark"]]
  pt_color <- scheme[["dark_highlight"]]

  resids <- sweep(-Ey, MARGIN = 2L, STATS = y, "+")
  ny <- length(y)
  if (ny >= 100) {
    nbins <- floor(sqrt(ny))
  } else if (ny > 10 && ny < 100) {
    nbins <- 10
  } else {
    # if (ny <= 10)
    nbins <- floor(ny / 2)
  }

  n <- nrow(Ey)
  binned <- binner(
    rep_id = 1,
    ey = Ey[1, ],
    r = resids[1, ],
    nbins = nbins
  )
  if (n > 1) {
    for (i in 2:nrow(resids))
      binned <- rbind(binned, binner(
        rep_id = i,
        ey = Ey[i,],
        r = resids[i,],
        nbins
      ))
  }

  base <- ggplot(binned, aes_string(x = "xbar"))
  graph <- base +
    geom_hline(
      yintercept = 0,
      linetype = 2,
      color = "black"
    ) +
    geom_path(
      mapping = aes_string(y = "se2"),
      color = line_color,
      size = line_size
    ) +
    geom_path(
      mapping = aes_string(y = "-se2"),
      color = line_color,
      size = line_size
    ) +
    geom_point(
      mapping = aes_string(y = "ybar"),
      shape = 21,
      fill = pt_fill,
      color = pt_color
    ) +
    labs(
      x = "Expected Values",
      y = "Average Residual \n (with 2SE bounds)"
    ) +
    theme_ppc()

  if (n > 1)
    graph <- graph + facet_wrap("rep", switch = "x")

  graph
}

binner <- function(rep_id, ey, r, nbins) {
  binned_resids <- arm::binned.resids(ey, r, nbins)$binned[, c("xbar", "ybar", "2se")]
  if (length(dim(binned_resids)) < 2L)
    binned_resids <- t(binned_resids)
  colnames(binned_resids) <- c("xbar", "ybar", "se2")
  data.frame(
    rep = paste0("yrep_", rep_id),
    binned_resids
  )
}
