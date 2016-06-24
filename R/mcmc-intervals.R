#' Plot interval estimates from MCMC draws
#'
#' @name MCMC-intervals
#' @family MCMC
#'
#' @template args-mcmc-x
#' @template args-pars
#' @template args-regex_pars
#' @template args-transformations
#' @param ... Currently unused.
#' @param prob The probability mass to include in inner interval (for
#'   \code{mcmc_intervals}) or in the shaded region (for \code{mcmc_areas}). The
#'   default is \code{0.5} (50\% interval).
#' @param prob_outer The probability mass to include in outer interval. The
#'   default is \code{0.9} for \code{mcmc_intervals} (90\% interval)
#'   and \code{1} for \code{mcmc_areas}.
#' @param point_est The point estimate to show. Either \code{"median"} (the
#'   default) or \code{"mean"}.
#' @param rhat An optional numeric vector of \eqn{\hat{R}}{Rhat} estimates, with
#'   one element per parameter included in \code{x}. If \code{rhat} is provided,
#'   the intervals/areas and point estimates in the resulting plot are colored
#'   based on \eqn{\hat{R}}{Rhat} value.
#'
#' @template return-ggplot
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{\code{mcmc_intervals}}{
#'    Plots of uncertainty intervals computed from posterior draws with all
#'    chains merged.
#'   }
#'   \item{\code{mcmc_areas}}{
#'    Density plots of computed from posterior draws with all chains merged,
#'    with uncertainty intervals shown as shaded areas under the curves.
#'   }
#' }
#'
#' @template seealso-color-scheme
#'
NULL

#' @rdname MCMC-intervals
#' @export
mcmc_intervals <- function(x,
                           pars = character(),
                           regex_pars = character(),
                           transformations = list(),
                           ...,
                           prob = 0.5,
                           prob_outer = 0.9,
                           point_est = c("median", "mean"),
                           rhat = numeric()) {
  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  .mcmc_intervals(
    x = merge_chains(x),
    prob_inner = prob,
    prob_outer = prob_outer,
    point_est = point_est,
    show_density = FALSE,
    rhat = rhat
  )
}

#' @rdname MCMC-intervals
#' @export
mcmc_areas <- function(x,
                       pars = character(),
                       regex_pars = character(),
                       transformations = list(),
                       ...,
                       prob = 0.5,
                       prob_outer = 1,
                       point_est = c("median", "mean"),
                       rhat = numeric()) {
  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  .mcmc_intervals(
    x = merge_chains(x),
    prob_inner = prob,
    prob_outer = prob_outer,
    point_est = point_est,
    show_density = TRUE,
    rhat = rhat
  )
}



# internal ----------------------------------------------------------------
.mcmc_intervals <- function(x,
                           prob_inner = 0.5,
                           prob_outer = 0.95,
                           point_est = c("median", "mean"),
                           rhat = numeric(),
                           show_density = FALSE) {
  n_param <- ncol(x)
  parnames <- colnames(x)

  probs <- c(0.5 - prob_outer / 2,
             0.5 - prob_inner / 2,
             0.5,
             0.5 + prob_inner / 2,
             0.5 + prob_outer / 2)

  quantiles <- t(apply(x, 2, quantile, probs = probs))
  y <- as.numeric(seq(n_param, 1, by = -1))
  x_lim <- c(min(quantiles[, 1]), max(quantiles[, 5]))
  x_range <- diff(x_lim)
  x_lim[1] <- x_lim[1] - 0.05 * x_range
  x_lim[2] <- x_lim[2] + 0.05 * x_range

  data <- data.frame(parnames, y, quantiles)
  colnames(data) <- c("parameter", "y", "ll", "l", "m", "h", "hh")
  if (match.arg(point_est) == "mean")
    data$m <- unname(colMeans(x))


  color_by_rhat <- isTRUE(length(rhat) > 0)
  if (color_by_rhat) {
    rhat <- factor_rhat(rhat)
    if (length(rhat) != nrow(data))
      stop(
        "'rhat' has length ", length(rhat),
        " but 'x' has ", nrow(data), " parameters.",
        call. = FALSE
      )

    data$rhat <- rhat
  }

  graph <- ggplot(data)

  if (findInterval(0, x_lim))
    graph <- graph + geom_vline(xintercept = 0, color = "gray90", size = 0.5)

  if (show_density) {

    # density outline
    nPoint.den <- 512
    y.den <- matrix(0, nrow = nPoint.den, ncol = n_param)
    x.den <- matrix(0, nrow = nPoint.den, ncol = n_param)
    for (i in 1:n_param) {
      d.temp <- density(x[, i],
                        from = quantiles[i, 1],
                        to = quantiles[i, 5],
                        n = nPoint.den)
      x.den[, i] <- d.temp$x
      y.max <- max(d.temp$y)
      y.den[, i] <- d.temp$y / y.max * 0.8 + y[i]
    }
    df_den <- data.frame(
      x = as.vector(x.den),
      y = as.vector(y.den),
      name = rep(parnames, each = nPoint.den)
    )
    if (color_by_rhat)
      df_den$rhat <- rep(rhat, each = nPoint.den)

    den_args <- list(
      data = df_den,
      mapping = aes_(
        x = ~ x,
        y = ~ y,
        group = ~ name,
        color = if (color_by_rhat) ~ rhat else NULL
      )
    )
    if (!color_by_rhat)
      den_args$color <- get_color("dark")
    p_den <- do.call("geom_line", den_args)

    #shaded interval
    y.poly <- matrix(0, nrow = nPoint.den + 2, ncol = n_param)
    x.poly <- matrix(0, nrow = nPoint.den + 2, ncol = n_param)
    for (i in 1:n_param) {
      d.temp <- density(x[, i],
                        from = quantiles[i, 2],
                        to = quantiles[i, 4],
                        n = nPoint.den)
      x.poly[, i] <-
        c(d.temp$x[1], as.vector(d.temp$x), d.temp$x[nPoint.den])
      y.max <- max(d.temp$y)
      y.poly[, i] <-
        as.vector(c(0, as.vector(d.temp$y) / y.max * 0.8, 0) + y[i])
    }
    df_poly <-
      data.frame(
        x = as.vector(x.poly),
        y = as.vector(y.poly),
        name = rep(parnames, each = nPoint.den + 2)
      )
    if (color_by_rhat)
      df_poly$rhat <- rep(rhat, each = nPoint.den + 2)
    p_poly <-
      geom_polygon(data = df_poly, aes_(
        x = ~ x,
        y = ~ y,
        group = ~ name,
        fill = if (color_by_rhat) ~ rhat else ~ y
      ))

    # point estimate
    segment_args <- list(
      mapping = aes_(
        x = ~ m,
        xend = ~ m,
        y = ~ y,
        yend = ~ y + 0.25,
        color = if (color_by_rhat) ~ rhat else NULL
      ),
      size = 1.5
    )
    if (!color_by_rhat)
      segment_args$color <- get_color("mid")
    p_point <- do.call("geom_segment", segment_args)

    # bottom line
    bottom_args <- list(
      mapping = aes_(
        x = ~ ll,
        xend = ~ hh,
        y = ~ y,
        yend = ~ y,
        color = if (color_by_rhat) ~ rhat else NULL
      )
    )
    if (!color_by_rhat)
      bottom_args$color <- get_color("dark")
    p_bottom <- do.call("geom_segment", bottom_args)

    graph <- graph +
      p_poly +
      p_point +
      p_bottom +
      p_den

    if (color_by_rhat) {
      graph <- graph + scale_fill_rhat() + scale_color_rhat()
    } else {
      graph <- graph + scale_fill_gradient(low = get_color("light"),
                                           high = get_color("light"),
                                           guide = "none")
    }

  } else { # No densities

    # outer interval
    graph <-
      graph + geom_segment(aes_(
        x = ~ ll,
        xend = ~ hh,
        y = ~ y,
        yend = ~ y
      ),
      colour = get_color("mid"))

    # inner interval
    segment_args <- list(
      mapping = aes_(
        x = ~ l,
        xend = ~ h,
        y = ~ y,
        yend = ~ y,
        color = if (color_by_rhat) ~ rhat else NULL
      ),
      size = 2,
      show.legend = FALSE
    )
    if (!color_by_rhat)
      segment_args$color <- get_color("dark")
    graph <- graph + do.call("geom_segment", segment_args)

    # point estimate
    point_args <- list(
      mapping = aes_(
        x = ~ m,
        y = ~ y,
        color = if (color_by_rhat) ~ rhat else NULL,
        fill = if (color_by_rhat) ~ rhat else NULL
      ),
      size = 4,
      shape = 21
    )
    if (!color_by_rhat) {
      point_args$color <- get_color("dark_highlight")
      point_args$fill <- get_color("light")
    }

    graph <- graph + do.call("geom_point", point_args)

    if (color_by_rhat)
      graph <- graph + scale_color_rhat() + scale_fill_rhat()
  }

  graph +
    scale_y_continuous(
      breaks = y,
      labels = parnames,
      limits = c(0.5, n_param + 1)
    ) +
    xlim(x_lim) +
    theme_default(y_lab = FALSE, x_lab = FALSE,
              legend_position = ifelse(color_by_rhat, "top", "none"))
}
