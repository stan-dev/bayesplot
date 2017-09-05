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
#' @param ... Currently ignored.
#' @param size,alpha Arguments passed on to \code{\link[ggplot2]{geom_line}}.
#' @param np For models fit using \code{\link{NUTS}} (more generally,
#'   any \href{http://en.wikipedia.org/wiki/Symplectic_integrator}{symplectic
#'   integrator}), an optional data frame providing NUTS
#'   diagnostic information. The data frame should be the object returned by
#'   \code{\link{nuts_params}} or one with the same structure.
#' @param np_style A call to the \code{parcoord_style_np} helper function to
#'   specify arguments controlling the appearance of superimposed lines
#'   representing NUTS diagnostics (in this case divergences) if the \code{np}
#'   argument is specified.
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
#' mcmc_parcoord(draws, np = np)
#'
#' color_scheme_set("darkgray")
#' div_style <- parcoord_style_np(div_color = "green", div_size = 0.5, div_alpha = 0.1)
#' mcmc_parcoord(draws, size = 0.25, alpha = 0.1,
#'               np = np, np_style = div_style)
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
           np = NULL,
           np_style = parcoord_style_np()) {
    check_ignored_arguments(...)
    stopifnot(inherits(np_style, "nuts_style"))

    x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
    param_labels <- parameter_names(x)
    draws <- reshape2::melt(merge_chains(x), varnames = c("Iteration", "Parameter"))
    n_iter <- num_iters(draws)
    n_param <- num_params(draws)

    if (n_param < 2)
      stop("This plot requires at least two parameters in 'x'.")

    has_divs <- !is.null(np)
    if (has_divs) {
      np <- validate_nuts_data_frame(np)
      draws$Divergent <-
        np %>%
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
          size = np_style$size[["div"]],
          alpha = np_style$alpha[["div"]],
          color = np_style$color[["div"]]
        )
    }

    graph +
      scale_x_discrete(expand = c(0,0), labels = param_labels) +
      labs(x = NULL, y = NULL)
  }


#' @rdname MCMC-parcoord
#' @export
#' @param div_color,div_size,div_alpha Optional arguments to the
#'   \code{parcoord_style_np} helper function that are eventually passed to
#'   \code{\link[ggplot2]{geom_line}} if the \code{np} argument is also
#'   specified. They control the color, size, and transparency specifications
#'   for showing divergences in the plot. The default values are displayed in
#'   the \strong{Usage} section above.
parcoord_style_np <-
  function(div_color = "red",
           div_size = 0.2,
           div_alpha = 0.2) {
    stopifnot(
      is.character(div_color),
      is.numeric(div_size),
      is.numeric(div_alpha) && div_alpha >= 0 && div_alpha <= 1
    )
    style <- list(
      color = c(div = div_color),
      size = c(div = div_size),
      alpha = c(div = div_alpha)
    )
    structure(style, class = c(class(style), "nuts_style"))
  }
