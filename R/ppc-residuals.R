#' PPC residuals
#'
#' \code{ppc_resid_hist} plots histograms of residuals computed from \code{y}
#' and simulated datasets \code{yrep}. \code{ppc_resid_scatter} and
#' \code{ppc_resid_scatter_avg} plot scatterplots of residuals vs. \code{y} and
#' average residuals vs. \code{y}, respectively. For binomial data,
#' \code{ppc_resid_binned} generates binned residual plots (similar to
#' \code{\link[arm]{binnedplot}}) from \code{y} and the posterior draws of the
#' linear predictor transformed by the inverse-link function. See the
#' \strong{Details} and \strong{Plot Descriptions} sections, below.
#'
#' @name PPC-residuals
#' @family PPCs
#'
#' @template args-y-yrep
#' @param ... Currently unused.
#'
#' @details
#' All of these plots (aside from \code{ppc_resid_scatter_avg}) compute and plot
#' residuals for each row of the matrix \code{yrep} (or for
#' \code{ppc_resid_binned} the matrix \code{Ey}), so it is usually a good idea
#' for \code{yrep} and \code{Ey} to contain only a small number of draws (rows).
#' See \strong{Examples}, below.
#'
#' For binomial and Bernoulli data the \code{ppc_resid_binned} function
#' can be used to generate binned residual plots. Bernoulli data can be input
#' as a vector of 0s and 1s, whereas for binomial data \code{y} should be a
#' vector of "success" proportions (not a matrix of "success" and "failure"
#' counts).
#'
#' @section Plot descriptions:
#' \describe{
#'   \item{\code{ppc_resid_hist}}{
#'    A separate histogram is plotted for the residuals computed from \code{y}
#'    and each dataset (row) in \code{yrep}. For this plot
#'    \code{yrep} should have only a small number of rows.
#'   }
#'   \item{\code{ppc_resid_scatter}}{
#'    A separate scatterplot is displayed for \code{y} vs. the residuals
#'    computed from \code{y} and each dataset (row) in \code{yrep}. For this
#'    plot \code{yrep} should have only a small number of rows.
#'   }
#'   \item{\code{ppc_resid_scatter_avg}}{
#'    A single scatterplot of \code{y} vs. the average of the residuals computed
#'    from \code{y} and each dataset (row) in \code{yrep}. For each individual
#'    data point \code{y[n]} the average residual is the average of the
#'    residuals for \code{y[n]} computed over the the draws from the posterior
#'    predictive distribution.
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
#' y <- example_y_data()
#' yrep <- example_yrep_draws()
#' ppc_resid_hist(y, yrep[1:3, ])
#' ppc_resid_scatter(y, yrep[10:14, ])
#' ppc_resid_scatter_avg(y, yrep)
#'
NULL

#' @rdname PPC-residuals
#' @export
#' @template args-hist
#'
ppc_resid_hist <- function(y, yrep, ..., binwidth = NULL) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)

  if (nrow(yrep) == 1) {
    resids <- data.frame(x = y - as.vector(yrep))
    graph <- ggplot(resids, aes_(x = ~ x)) +
      labs(y = NULL, x = expression(italic(y) - italic(y)^rep))
  } else {
    resids <- melt_yrep(compute_resids(y, yrep))
    resids$rep_id <- factor(
      resids$rep_id,
      labels = paste("italic(y)", "-", unique(resids$rep_id))
    )
    graph <- ggplot(resids, aes_(x = ~ value)) +
      labs(y = NULL, x = NULL) +
      facet_wrap_parsed("rep_id", switch = "x")
  }

  graph +
    geom_histogram(
      mapping = aes_(y = ~ ..density..),
      fill = get_color("l"),
      color = get_color("lh"),
      size = 0.25,
      binwidth = binwidth
    ) +
    dont_expand_y_axis() +
    theme_default(y_text = FALSE)
}


#' @rdname PPC-residuals
#' @export
#' @param size,alpha Arguments passed to \code{\link[ggplot2]{geom_point}} to
#'   control the appearance of scatterplot points.
#'
ppc_resid_scatter <-
  function(y,
           yrep,
           ...,
           size = 2.5,
           alpha = 0.8) {
    y <- validate_y(y)
    yrep <- validate_yrep(yrep, y)

    if (nrow(yrep) == 1) {
      return(
        .ppc_scatter(
          data = data.frame(y = y, x = y - as.vector(yrep)),
          mapping = aes_(x = ~ x, y = ~ y),
          x_lab = expression(italic(y) - italic(y) ^ rep),
          y_lab = expression(italic(y)),
          size = size,
          alpha = alpha,
          abline = FALSE
        )
      )
    }

    resids <- melt_yrep(compute_resids(y, yrep))
    resid_labs <- paste("italic(y)", "-", unique(resids$rep_id))
    resids$rep_id <- factor(resids$rep_id, labels = resid_labs)

    .ppc_scatter(
      data = dplyr::left_join(
        resids,
        data.frame(y = y, y_id = seq_along(y)),
        by = "y_id"
      ),
      mapping = aes_(x = ~ value, y = ~ y),
      y_lab = expression(italic(y)),
      x_lab = NULL,
      size = size,
      alpha = alpha,
      abline = FALSE
    ) +
      facet_wrap_parsed("rep_id", switch = "x")
  }

#' @rdname PPC-residuals
#' @export
ppc_resid_scatter_avg <-
  function(y,
           yrep,
           ...,
           size = 2.5,
           alpha = 0.8) {
    y <- validate_y(y)
    yrep <- validate_yrep(yrep, y)

    if (nrow(yrep) == 1)
      return(
        ppc_resid_scatter(y, yrep,
                          size = size,
                          alpha = alpha, ...)
      )

    .ppc_scatter(
      data = data.frame(y, avg_resid = y - colMeans(yrep)),
      mapping = aes_(x = ~ avg_resid, y = ~ y),
      y_lab = y_label(),
      x_lab = "Average residual",
      alpha = alpha,
      size = size,
      abline = FALSE
    )
  }


#' @rdname PPC-residuals
#' @export
#' @param Ey A matrix of posterior draws of the linear predictor transformed by
#'   the inverse-link function.
#'
ppc_resid_binned <- function(y, Ey, ...) {
  suggested_package("arm")
  y <- validate_y(y)
  Ey <- validate_yrep(Ey, y)
  resids <- compute_resids(y, Ey)

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

  mixed_scheme <- is_mixed_scheme(get_color_scheme())
  point_fill <- get_color(ifelse(mixed_scheme, "m", "d"))
  point_color <- get_color(ifelse(mixed_scheme, "mh", "dh"))
  graph <-
    ggplot(binned, aes_(x = ~ xbar)) +
    geom_hline(
      yintercept = 0,
      linetype = 2,
      color = "black"
    ) +
    geom_path(
      mapping = aes_(y = ~ se2),
      color = get_color("l"),
      size = 1
    ) +
    geom_path(
      mapping = aes_(y = ~ -se2),
      color = get_color("l"),
      size = 1
    ) +
    geom_point(
      mapping = aes_(y = ~ ybar),
      shape = 21,
      fill = point_fill,
      color = point_color
    ) +
    labs(
      x = "Expected Values",
      y = "Average Residuals \n (with 2SE bounds)"
    )

  if (n > 1)
    graph <- graph + facet_wrap_parsed("rep")

  graph + theme_default()
}




# internal ----------------------------------------------------------------
compute_resids <- function(y, yrep) {
  r <- sweep(yrep, MARGIN = 2L, STATS = as.array(y), FUN = "-")
  as.matrix(-1 * r)
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
