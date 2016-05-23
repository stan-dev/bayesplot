#' Distributions
#'
#' Compare the empirical distribution of the data \code{y} to the distributions
#' of simulated/replicated data \code{yrep} from the posterior predictive
#' distribution. \code{ppc_dens_overlay} plots the distributions as overlaid
#' densities. \code{ppc_hist} and \code{ppc_dens} plot separate histograms and
#' kernel density estimates, respectively, for \code{y} and each row of
#' \code{yrep} (so for \code{ppc_hist} and \code{ppc_dens} \code{yrep} should
#' contain only a small number of rows).
#'
#' @name distributions
#' @family PPCs
#'
#' @template args-y-yrep
#' @template args-hist
#' @param ... Currently unused.
#'
#' @template details-binomial
#' @template return-ggplot
#'
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#' @template seealso-color-scheme
#'
#' @examples
#' y <- rnorm(100)
#' yrep <- matrix(rnorm(2500), ncol = 100)
#' ppc_dens_overlay(y, yrep)
#' ppc_hist(y, yrep[1:8, ])
#'
NULL

#' @export
#' @rdname distributions
#'
ppc_hist <- function(y, yrep, ..., binwidth = NULL) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  plot_data <- ppc_dist_data(y, yrep)
  scheme <- get_color_scheme()
  fills <- c(scheme[["dark"]], scheme[["light"]])
  colors <- c(scheme[["dark_highlight"]], scheme[["light_highlight"]])
  ggplot(
    data = plot_data,
    mapping = aes_string(
      x = 'value',
      fill = 'is_y',
      color = "is_y"
    )
  ) +
    geom_histogram(
      mapping = aes_string(y = "..density.."),
      size = 0.25,
      binwidth = binwidth
    ) +
    facet_wrap("rep_id", switch = "x") +
    scale_fill_manual(values = fills) +
    scale_color_manual(values = colors) +
    coord_cartesian(expand = FALSE) +
    theme_ppc(y_text = FALSE, x_lab = FALSE)
}


#' @export
#' @rdname distributions
#'
ppc_dens <- function(y, yrep, ...) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  plot_data <- ppc_dist_data(y, yrep)
  scheme <- get_color_scheme()
  fills <- c(scheme[["dark"]], scheme[["light"]])
  colors <- c(scheme[["dark_highlight"]], scheme[["light_highlight"]])

  ggplot(
    data = plot_data,
    mapping = aes_string(
      x = 'value',
      fill = 'is_y',
      color = "is_y"
    )
  ) +
    geom_density(size = 1) +
    facet_wrap("rep_id", switch = "x") +
    scale_fill_manual(values = fills) +
    scale_color_manual(values = colors) +
    coord_cartesian(expand = FALSE) +
    theme_ppc(y_text = FALSE, x_lab = FALSE)
}

#' @export
#' @rdname distributions
ppc_dens_overlay <- function(y, yrep, ...) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  plot_data <- ppc_dist_data(y, yrep)
  scheme <- get_color_scheme()
  colors <- c(scheme[["light"]], scheme[["dark_highlight"]])
  fills <- c(NA, scheme[["dark"]])

  ggplot(
    data = plot_data,
    mapping = aes_string(
      x = "value",
      group = "rep_id",
      color = "is_y",
      fill = "is_y",
      size = "is_y"
    )
  ) +
    geom_density() +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = fills) +
    scale_size_manual(values = c(0.25, 1)) +
    xlab("y") +
    coord_cartesian(expand = FALSE) +
    theme_ppc(y_text = FALSE)
}

ppc_dist_data <- function(y, yrep) {
  yrep <- melt_yrep(yrep)
  yobs_lab <- "Observed y"
  levels(yrep$rep_id) <- c(levels(yrep$rep_id), yobs_lab)
  ydat <- data.frame(
    rep_id = yobs_lab,
    y_id = seq_along(y),
    value = y
  )
  within(data = rbind(yrep, ydat), {
    rep_id <- relevel(rep_id, ref = yobs_lab)
    is_y <- rep_id == yobs_lab
  })
}
