#' Residuals
#'
#' \code{ppc_resid} plots the distributions of residuals computed from
#' \code{y} and simulated datasets \code{yrep}. For binomial data,
#' \code{ppc_resid_binned} generates binned residual plots (similar to
#' \code{\link[arm]{binnedplot}}) from \code{y} and the posterior draws of the
#' linear predictor transformed by the inverse-link function.
#'
#' @name residuals
#' @family PPCs
#'
#' @template args-y-yrep
#' @template args-hist
#' @param ... Currently unused.
#'
#' @details
#' \code{ppc_resid} and \code{ppc_resid_binned} compute and plot
#' residuals for each row of the matrices \code{yrep} and \code{Ey},
#' respectively, so it is usually a good idea for \code{yrep} and \code{Ey} to
#' contain only a small number of draws (rows).
#'
#' For binomial and Bernoulli data the \code{ppc_resid_binned} function
#' should be used to generate binned residual plots. Bernoulli data can be input
#' as a vector of 0s and 1s, whereas for binomial data \code{y} should be a
#' vector of "success" proportions (not a matrix of "success" and "failure"
#' counts).
#'
#' @section Plot descriptions:
#' \describe{
#'   \item{\code{ppc_resid}}{
#'    A separate histogram is plotted for the residuals computed from \code{y}
#'    and each dataset (row) in \code{yrep}. For this plot
#'    \code{yrep} should have only a small number of rows.
#'   }
#'   \item{\code{ppc_resid_binned}}{
#'    Intended for use with binomial data. A separate binned residual plot
#'    (similar to \code{\link[arm]{binnedplot}}) is generated for each dataset
#'    (row) in \code{Ey}, the posterior draws of the linear predictor
#'    transformed by the inverse-link function. For this plot \code{Ey} should
#'    have only a small number of rows.
#'   }
#' }
#'
#' @template return-ggplot
#'
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#' @template seealso-color-scheme
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
ppc_resid <- function(y, yrep, ..., binwidth = NULL) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  n <- nrow(yrep)

  if (n == 1) {
    resids <- data.frame(x = y - as.vector(yrep))
    graph <- ggplot(resids, aes_string(x = "x")) +
      labs(y = NULL, x = expression(italic(y) - italic(y)^rep))
  } else {
    resids <- melt_yrep(as.matrix(-1 * sweep(yrep, 2L, y)))
    resids$rep_id <- factor(resids$rep_id, labels = paste("y -", unique(resids$rep_id)))
    graph <- ggplot(resids, aes_string(x = "value")) +
      labs(y = NULL, x = NULL) +
      facet_wrap_parsed("rep_id", switch = "x")
  }

  graph +
    geom_histogram(
      mapping = aes_string(y = "..density.."),
      fill = ppc_color("dark"),
      color = ppc_color("dark_highlight"),
      size = 0.25,
      binwidth = binwidth
    ) +
    dont_expand_y_axis() +
    theme_ppc(y_text = FALSE)
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

  graph <-
    ggplot(binned, aes_string(x = "xbar")) +
    geom_hline(
      yintercept = 0,
      linetype = 2,
      color = "black"
    ) +
    geom_path(
      mapping = aes_string(y = "se2"),
      color = ppc_color("light"),
      size = 1
    ) +
    geom_path(
      mapping = aes_string(y = "-se2"),
      color = ppc_color("light"),
      size = 1
    ) +
    geom_point(
      mapping = aes_string(y = "ybar"),
      shape = 21,
      fill = ppc_color("dark"),
      color = ppc_color("dark_highlight")
    ) +
    labs(
      x = "Expected Values",
      y = "Average Residual \n (with 2SE bounds)"
    )

  if (n > 1)
    graph <- graph + facet_wrap_parsed("rep")

  graph + theme_ppc()
}

binner <- function(rep_id, ey, r, nbins) {
  binned_resids <- arm::binned.resids(ey, r, nbins)$binned
  binned_resids <- binned_resids[, c("xbar", "ybar", "2se")]
  if (length(dim(binned_resids)) < 2)
    binned_resids <- t(binned_resids)
  colnames(binned_resids) <- c("xbar", "ybar", "se2")
  data.frame(
    rep = create_yrep_ids(rep_id),
    binned_resids
  )
}
