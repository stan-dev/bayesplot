#' Plot interval estimates from MCMC draws
#'
#' @name MCMC-intervals
#' @family MCMC
#'
#' @template args-mcmc-x
#' @template args-pars
#' @template args-regex_pars
#' @template args-transformations
#' @param prob_inner Probability mass to include in inner interval.
#' @param prob_outer Probability mass to include in outer interval.
#' @param point_est The point estimate to show. Either \code{"median"} (the
#'   default) or \code{"mean"}.
#'
NULL

#' @rdname MCMC-intervals
#' @export
mcmc_intervals <- function(x,
                           pars = character(),
                           regex_pars = character(),
                           transformations = list(),
                           ...,
                           show_density = FALSE,
                           prob_inner = 0.5,
                           prob_outer = 0.95,
                           point_est = c("median", "mean")) {
                           # rhat_values = c(),
                           # color_by_rhat = FALSE) {
  color_by_rhat <- FALSE

  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  samps_use <- merge_chains(x)
  n_param <- ncol(samps_use)
  parnames <- colnames(samps_use)


  # rhat_pal <- get_color(c("light_highlight", "mid_highlight", "dark_highlight"))
  # rhat_id <- ifelse(rhat_values < 1.05, "A",
  #                   ifelse(rhat_values < 1.1, "B", "C"))
  # rhat_id <- factor(
  #   rhat_id[parnames],
  #   levels = c("A", "B", "C"),
  #   labels = c("<1.05", "<1.1", ">1.1")
  # )
  # rhat_colors <- scale_color_manual(name = bquote(hat(R)),
  #                                   values = rhat_pal,
  #                                   drop = FALSE)
  # rhat_lgnd <- theme(
  #   legend.position = "top",
  #   legend.title =  element_text(size = 13, face = "bold"),
  #   legend.text =  element_text(size = 12)
  # )

  probs <- c(0.5 - prob_outer / 2,
             0.5 - prob_inner / 2,
             0.5,
             0.5 + prob_inner / 2,
             0.5 + prob_outer / 2)

  quantiles <- t(apply(samps_use, 2, quantile, probs = probs))
  y <- as.numeric(seq(n_param, 1, by = -1))
  x_lim <- c(min(quantiles[, 1]), max(quantiles[, 5]))
  x_range <- diff(x_lim)
  x_lim[1] <- x_lim[1] - 0.05 * x_range
  x_lim[2] <- x_lim[2] + 0.05 * x_range

  data <- data.frame(parnames, y, quantiles)
  colnames(data) <- c("parameter", "y", "ll", "l", "m", "h", "hh")
  if (match.arg(point_est) == "mean")
    data$m <- unname(colMeans(samps_use))



  graph <- ggplot(data)

  if (findInterval(0, x_lim))
    graph <- graph + geom_vline(xintercept = 0, color = "gray90", size = 0.5)

  if (!show_density) {
    graph <-
      graph + geom_segment(aes_(
        x = ~ ll,
        xend = ~ hh,
        y = ~ y,
        yend = ~ y
      ),
      colour = get_color("mid"))
  }


  if (show_density) {

    nPoint.den <- 512
    y.den <- matrix(0, nrow = nPoint.den, ncol = n_param)
    x.den <- matrix(0, nrow = nPoint.den, ncol = n_param)
    for (i in 1:n_param) {
      d.temp <- density(samps_use[, i],
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
    p_den <-
      geom_line(data = df_den,
                aes_(x = ~ x, y = ~ y, group = ~ name),
                color = get_color("dark"))

    #shaded interval
    y.poly <- matrix(0, nrow = nPoint.den + 2, ncol = n_param)
    x.poly <- matrix(0, nrow = nPoint.den + 2, ncol = n_param)
    for (i in 1:n_param) {
      d.temp <- density(samps_use[, i],
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
    p_poly <-
      geom_polygon(data = df_poly, aes_(
        x = ~ x,
        y = ~ y,
        group = ~ name,
        fill = ~ y
      ))
    p_fill <-
      scale_fill_gradient(low = get_color("light"),
                          high = get_color("light"),
                          guide = "none")

    #point estimator
    if (color_by_rhat) {
      p_point <-
        geom_segment(aes_(
          x = ~ m,
          xend = ~ m,
          y = ~ y,
          yend = ~ y + 0.25,
          color = ~ rhat_id
        ), size = 1.5)

      graph <- graph +
        p_poly + p_den + p_fill + p_point + rhat_colors + rhat_lgnd

    } else {
      p_point <-
        geom_segment(aes_(
          x = ~ m,
          xend = ~ m,
          y = ~ y,
          yend = ~ y + 0.25
        ),
        colour = get_color("mid"),
        size = 1.5)

      graph <- graph +
        p_poly +
        p_den +
        geom_segment(aes_(
          x = ~ ll,
          xend = ~ hh,
          y = ~ y,
          yend = ~ y
        ),
        colour = get_color("dark")) +
        p_fill +
        p_point
    }

  } else {

    graph <- graph + geom_segment(aes_(
      x = ~ l,
      xend = ~ h,
      y = ~ y,
      yend = ~ y
    ),
    colour = get_color("dark"),
    size = 2)

    if (color_by_rhat) {
      graph <- graph +
        geom_point(
          aes_(x = ~ m, y = ~ y, fill = ~ rhat_id),
          color = "black",
          shape = 21,
          size = 4
        ) +
        rhat_colors +
        rhat_lgnd

    } else {
      graph <- graph +
        geom_point(
          aes_(x = ~ m, y = ~ y),
          size = 4,
          color = get_color("dark_highlight"),
          fill = get_color("light"),
          shape = 21
        )
    }
  }

  graph +
    scale_y_continuous(
      breaks = y,
      labels = parnames,
      limits = c(0.5, n_param + 1)
    ) +
    xlim(x_lim) +
    theme_ppc(y_lab = FALSE, x_lab = FALSE)
}
