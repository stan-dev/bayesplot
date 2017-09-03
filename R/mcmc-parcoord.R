#' Parallel coordinates plot of MCMC draws
#'
#' Parallel coordinates plot of MCMC draws. See the \strong{Plot Descriptions}
#' section, below, for details.
#'
#' @name MCMC-parcoord
#' @family MCMC
#'
#' @template args-mcmc-x
#' @template args-pars
#' @template args-regex_pars
#' @template args-transformations
#' @template args-facet_args
#' @param ... Currently ignored.
#' @param size,alpha,div_size,div_alpha,div_color Arguments passed on to
#'   \code{\link[ggplot2]{geom_line}}. The last three are ignored unless
#'   \code{divergences} is specified.
#' @param divergences For models fit using \code{\link{NUTS}} (more generally,
#'   any \href{http://en.wikipedia.org/wiki/Symplectic_integrator}{symplectic
#'   integrator}), an optional data frame providing information about divergent
#'   transitions. The data frame should be the object returned by
#'   \code{\link{nuts_params}} or one with the same structure. If
#'   \code{divergences} is specified then \code{div_color}, \code{div_size}, and
#'   \code{div_alpha} can be specified to customize the appearance of the
#'   iterations that began with a divergence. See the the \strong{Examples}
#'   section, below.
#'
#' @examples
#' color_scheme_set("pink")
#' x <- example_mcmc_draws()
#' mcmc_parcoord(x)
#'
#' \dontrun{
#' library(rstan)
#' fit <- stan_demo("eight_schools")
#' draws <- as.array(fit, pars = c("tau", "theta", "lp__"))
#' np <- nuts_params(fit)
#'
#' color_scheme_set("blue")
#' mcmc_parcoord(draws)
#' mcmc_parcoord(draws, divergences = np)
#'
#' color_scheme_set("darkgray")
#' mcmc_parcoord(
#'   draws,
#'   divergences = np,
#'   size = 0.25,
#'   alpha = 0.1,
#'   div_color = "green",
#'   div_size = 0.5,
#'   div_alpha = 0.1
#' )
#' }
NULL

#' @rdname MCMC-parcoord
#' @export
mcmc_parcoord <-
  function(x,
           pars = character(),
           regex_pars = character(),
           transformations = list(),
           ...,
           size = 0.2,
           alpha = 0.2,
           divergences = NULL,
           div_color = "red",
           div_size = 0.2,
           div_alpha = 0.2) {
    check_ignored_arguments(...)
    x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
    param_labels <- parameter_names(x)
    draws <- reshape2::melt(merge_chains(x), varnames = c("Iteration", "Parameter"))
    n_iter <- num_iters(draws)
    n_param <- num_params(draws)
    has_divs <- !is.null(divergences)
    if (has_divs) {
      divergences <- validate_nuts_data_frame(divergences)
      draws$Divergent <-
        divergences %>%
        filter_(~ Parameter == "divergent__") %>%
        .$Value %>%
        rep(times = n_param)

      div_draws <- filter_(draws, ~ Divergent == 1)
      draws <- filter_(draws, ~ Divergent == 0)
    }

    graph <- ggplot(draws, aes_(
      x = ~ Parameter,
      y = ~ value,
      group = ~ factor(Iteration)
    )) +
      geom_line(
        size = size,
        alpha = alpha,
        color = get_color("dh")
      )

    if (has_divs) {
      graph <- graph +
        geom_line(
          data = div_draws,
          size = 1.1 * size,
          alpha = min(1, 1.1 * alpha),
          color = div_color
        )
    }

    graph +
      scale_x_discrete(expand = c(0,0), labels = param_labels) +
      labs(x = NULL, y = NULL)
  }
