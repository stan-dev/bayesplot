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
#' @template seealso-color-scheme
#'
#' @examples
#' # some fake parameter draws to use for demonstration
#' x <- fake_draws()
#' dim(x)
#' dimnames(x)
#'
#' ##################
#' ### Histograms ###
#' ##################
#'
#' # histograms of all parameters
#' mcmc_hist(x)
#'
#' # histograms of some parameters
#' mcmc_hist(x, pars = c("alpha", "beta[2]"))
#' mcmc_hist(x, pars = "sigma", regex_pars = "beta")
#'
#' # interpret facet labels as plotmath expressions
#' # (e.g. to get greek letters for parameters)
#' mcmc_hist(
#'  x,
#'  pars = c("alpha", "beta[2]"),
#'  facet_args = list(labeller = ggplot2::label_parsed)
#' )
#'
#' # with sigma on log scale
#' mcmc_hist(x, transformations = list(sigma = "log"))
#'
#' # can specify transformation as function(x) log(x) or
#' # as log (without quotes), but then the label is 't(sigma)'
#' # instead of 'log(sigma)'
#' mcmc_hist(x, transformations = list(sigma = log))
#'
#' # separate histograms by chain
#' set_color_scheme("blue")
#' mcmc_hist_by_chain(x, pars = "sigma", regex_pars = "beta")
#'
#' #################
#' ### Densities ###
#' #################
#'
#' mcmc_dens(
#'  x,
#'  pars = c("sigma", "beta[2]"),
#'  facet_args = list(nrow = 2)
#' )
#'
#' # separate and overlay chains
#' mcmc_dens_overlay(
#'  x,
#'  pars = c("sigma", "beta[2]"),
#'  facet_args = list(nrow = 2)
#' )
#'
#' # parse facet labels, put them on the y axis, and increase their font size
#' p <- mcmc_dens_overlay(
#'  x,
#'  pars = c("sigma", "beta[2]"),
#'  facet_args =
#'    list(nrow = 2, labeller = ggplot2::label_parsed, switch = "y")
#' )
#' p + facet_text(size = 15)
#'
#' # separate chains as violin plots
#' mcmc_violin(x)
#' mcmc_violin(
#'  x,
#'  probs = c(0.1, 0.9), # where to draw quantile lines
#'  transformations = list(sigma = "log")
#' )
#'
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
  graph <- ggplot(data, aes_(x = ~ Value, y = ~..density..)) +
    geom_histogram(
      fill = get_color("mid"),
      color = get_color("mid_highlight"),
      size = .25,
      na.rm = TRUE,
      binwidth = binwidth
    ) +
    dont_expand_y_axis(c(0.005, 0)) +
    theme_default(y_text = FALSE, x_lab = FALSE)

  if (is.null(facet_args$scales))
    facet_args$scales <- "free"

  if (!by_chain) {
    facet_args$facets <- ~ Parameter
    graph + do.call("facet_wrap", facet_args)
  } else {
    facet_args$facets <- Chain ~ Parameter
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

  if (by_chain || violin) {
    if (!has_multiple_chains(x))
      STOP_need_multiple_chains()
    else
      n_chain <- length(unique(data$Chain))
  }

  aes_mapping <- if (violin) {
    list(x = ~ Chain, y = ~ Value)
  } else {
    list(x = ~ Value)
  }
  geom_args <- list(size = 0.5, na.rm = TRUE)
  if (violin)
    geom_args$draw_quantiles <- probs

  if (by_chain) {
    aes_mapping[["color"]] <- ~ Chain
    aes_mapping[["group"]] <- ~ Chain
    geom_args[["alpha"]] <- 0.33
  } else {
    geom_args[["fill"]] <- get_color("mid")
    geom_args[["color"]] <- get_color("mid_highlight")
  }

  graph <- ggplot(data, mapping = do.call("aes_", aes_mapping)) +
    do.call(paste0("geom_", geom), geom_args) +
    dont_expand_y_axis(c(0.005, 0)) +
    theme_default(y_text = FALSE, x_lab = FALSE,
              legend_position = ifelse(by_chain, "right", "none"))

  if (!violin)
    graph <- graph + dont_expand_x_axis()
  if (by_chain)
    graph <- graph + scale_color_manual(values = chain_colors(n_chain))
  if (is.null(facet_args$scales))
    facet_args$scales <- "free"

  facet_args$facets <- ~ Parameter
  graph + do.call("facet_wrap", facet_args)
}