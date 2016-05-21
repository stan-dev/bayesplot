#' Distributions
#'
#' Compare the empirical distribution of the data \eqn{y} to the distributions
#' of simulated/replicated data \eqn{y^{rep}}{yrep} from the posterior
#' predictive distribution. \code{ppc_dens_overlay} plots the distributions as
#' overlaid densities. \code{ppc_hist} plots a separate histogram for each (so
#' \code{yrep} should contain only a small number of rows).
#'
#' @name distributions
#' @family PPCs
#'
#' @template args-ppc
#' @param ... Optional arguments to geoms to control features of the plots
#'   (e.g. \code{binwidth} if the plot is a histogram).
#'
#' @template details-ppc
#' @template return-ggplot
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
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
ppc_dens_overlay <- function(y, yrep, ...) {
  ppc_dist(y, yrep, overlay = TRUE, ...)
}

#' @export
#' @rdname distributions
#'
ppc_hist <- function(y, yrep, ...) {
  ppc_dist(y, yrep, overlay = FALSE, ...)
}


ppc_dist <- function(y, yrep, overlay = TRUE, ...) {
  validate_y_and_yrep(y, yrep)

  yrep <- melt_yrep(yrep)
  levels(yrep$rep_id) <- c(levels(yrep$rep_id), "Observed y")
  ydat <- data.frame(
    rep_id = "Observed y",
    y_id = seq_along(y),
    value = y
  )
  plot_data <- within(data = rbind(yrep, ydat), {
    rep_id <- relevel(rep_id, ref = "Observed y")
    is_y <- rep_id == "Observed y"
  })
  plot_function <- paste0("ppc_", ifelse(overlay, "density", "histogram"))
  graph <- do.call(plot_function, list(data = plot_data, ...))
  graph + theme_ppc()
}

ppc_histogram <- function(data, ...) {
  defaults <- list(size = 0.2)
  geom_args <- set_geom_args(defaults, ...)
  geom_args$mapping <- aes_string(y = "..density..")
  base <- ggplot(
    data = data,
    mapping = aes_string(
      x = 'value',
      fill = 'is_y',
      color = "is_y"
    )
  )
  base +
    call_geom("histogram", geom_args) +
    facet_wrap("rep_id", switch = "x") +
    scale_fill_manual(values = c(.PP_DARK, .PP_LIGHT)) +
    scale_color_manual(values = c(.PP_DARK_highlight, .PP_LIGHT_highlight)) +
    xlab(NULL)
}

ppc_density <- function(data, ...) {
  base <- ggplot(
    data = data,
    mapping = aes_string(
      x = "value",
      group = "rep_id",
      color = "is_y",
      fill = "is_y",
      size = "is_y"
    )
  )
  base +
    geom_density(...) +
    scale_color_manual(values = c(.PP_LIGHT, .PP_DARK_highlight)) +
    scale_fill_manual(values = c(NA, .PP_DARK)) +
    scale_size_manual(values = c(0.2, 1)) +
    xlab("y")
}
