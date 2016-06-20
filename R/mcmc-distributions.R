#' Histograms and kernel density plots of MCMC draws
#'
#' @name MCMC-distributions
#' @family MCMC
#'
#' @template args-mcmc-x
#' @template args-pars
#' @template args-regex_pars
#' @template args-transformations
#' @param facet_args Arguments (other than \code{facets}) passed to
#'   \code{\link[ggplot2]{facet_wrap}} (if \code{by_chain} is \code{FALSE}) or
#'   \code{\link[ggplot2]{facet_grid}} (if \code{by_chain} is \code{TRUE}) to
#'   control faceting.
#' @param ... Currently ignored.
#'
#' @template return-ggplot
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{\code{mcmc_hist}}{
#'    Histograms of posterior draws with all chains merged.
#'   }
#'   \item{\code{mcmc_dens}}{
#'    Kernel density plots of posterior draws with all chains merged.
#'   }
#'   \item{\code{mcmc_hist_by_chain}}{
#'    Histograms of posterior draws with chains separated via faceting.
#'   }
#'   \item{\code{mcmc_dens_overlay}}{
#'    Kernel density plots of posterior draws with chains separated but
#'    overlaid on a single plot.
#'   }
#'   \item{\code{mcmc_violin}}{
#'    The density estimate of each chain is plotted as a violin with
#'    horizontal lines at notable quantiles.
#'   }
#' }
#'
NULL

#' @rdname MCMC-distributions
#' @export
#' @template args-hist
#'
mcmc_hist <- function(x,
                      pars = character(),
                      regex_pars = character(),
                      transformations = list(),
                      facet_args = list(),
                      ...,
                      binwidth = NULL) {
  .mcmc_hist(
    x,
    pars = pars,
    regex_pars = regex_pars,
    transformations = transformations,
    facet_args = facet_args,
    binwidth = binwidth,
    by_chain = FALSE,
    ...
  )
}

#' @rdname MCMC-distributions
#' @export
mcmc_dens <- function(x,
                      pars = character(),
                      regex_pars = character(),
                      transformations = list(),
                      facet_args = list(),
                      ...) {
  .mcmc_dens(
    x,
    pars = pars,
    regex_pars = regex_pars,
    transformations = transformations,
    facet_args = facet_args,
    by_chain = FALSE,
    ...
  )
}

#' @rdname MCMC-distributions
#' @export
#'
mcmc_hist_by_chain <- function(x,
                               pars = character(),
                               regex_pars = character(),
                               transformations = list(),
                               facet_args = list(),
                               ...,
                               binwidth = NULL) {
  .mcmc_hist(
    x,
    pars = pars,
    regex_pars = regex_pars,
    transformations = transformations,
    facet_args = facet_args,
    binwidth = binwidth,
    by_chain = TRUE,
    ...
  )
}

#' @rdname MCMC-distributions
#' @export
mcmc_dens_overlay <- function(x,
                              pars = character(),
                              regex_pars = character(),
                              transformations = list(),
                              facet_args = list(),
                              ...) {
  .mcmc_dens(
    x,
    pars = pars,
    regex_pars = regex_pars,
    transformations = transformations,
    facet_args = facet_args,
    by_chain = TRUE,
    ...
  )
}

#' @rdname MCMC-distributions
#' @inheritParams ppc_violin_grouped
#' @export
mcmc_violin <- function(x,
                        pars = character(),
                        regex_pars = character(),
                        transformations = list(),
                        facet_args = list(),
                        ...,
                        probs = c(0.1, 0.5, 0.9)) {
  .mcmc_dens(
    x,
    pars = pars,
    regex_pars = regex_pars,
    transformations = transformations,
    facet_args = facet_args,
    geom = "violin",
    probs = probs,
    ...
  )
}



# internal -----------------------------------------------------------------
.mcmc_hist <- function(x,
                      pars = character(),
                      regex_pars = character(),
                      transformations = list(),
                      facet_args = list(),
                      binwidth = NULL,
                      by_chain = FALSE,
                      ...) {
  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  if (by_chain && !has_multiple_chains(x))
    STOP_need_multiple_chains()

  data <- reshape2::melt(x, value.name = "Value")
  graph <- ggplot(data, aes_(x = ~ Value)) +
    geom_histogram(
      fill = get_color("mid"),
      color = get_color("mid_highlight"),
      size = .25,
      na.rm = TRUE,
      binwidth = binwidth
    ) +
    dont_expand_y_axis(c(0.005, 0)) +
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

.mcmc_dens <- function(x,
                       pars = character(),
                       regex_pars = character(),
                       transformations = list(),
                       facet_args = list(),
                       by_chain = FALSE,
                       geom = c("density", "violin"),
                       probs = c(0.1, 0.5, 0.9),
                       ...) {
  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  data <- reshape2::melt(x, value.name = "Value")
  data$Chain <- factor(data$Chain)

  geom <- match.arg(geom)
  violin <- geom == "violin"

  if ((by_chain || violin) && !has_multiple_chains(x))
    STOP_need_multiple_chains()

  aes_mapping <- if (violin) {
    list(x = ~ Chain, y = ~ Value)
  } else {
    list(x = ~ Value)
  }
  geom_args <- list(size = 0.25, na.rm = TRUE)
  if (violin)
    geom_args$draw_quantiles <- probs

  if (by_chain) {
    aes_mapping[["fill"]] <- ~ Chain
    aes_mapping[["color"]] <- ~ Chain
    aes_mapping[["group"]] <- ~ Chain
    geom_args[["alpha"]] <- 0.5
  } else {
    geom_args[["fill"]] <- get_color("mid")
    geom_args[["color"]] <- get_color("mid_highlight")
  }

  graph <- ggplot(data, mapping = do.call("aes_", aes_mapping)) +
    do.call(paste0("geom_", geom), geom_args) +
    dont_expand_y_axis(c(0.005, 0)) +
    theme_ppc(y_text = FALSE,
              legend_position = ifelse(by_chain, "right", "none"))

  if (is.null(facet_args$scales))
    facet_args$scales <- "free"

  facet_args$facets <- ~ Parameter
  graph + do.call("facet_wrap", facet_args)
}
