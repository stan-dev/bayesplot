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
#' @template return-ggplot-or-data
#'
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{\code{mcmc_parcoord}}{
#'    Parallel coordinates plot of MCMC draws. A separate line is plotted for
#'    each draw. This plot is probably most useful if the optional NUTS
#'    diagnostic information is provided via the \code{np} argument. In that
#'    case divergences are highlighted in the plot. This was originally
#'    suggested by Ari Hartikainen on the Stan Forums and the discussion in
#'    \href{http://discourse.mc-stan.org/t/concentration-of-divergences/1590/21}{that
#'    thread} may be helpful for understanding the plot.
#'   }
#' }
#'
#' @examples
#' color_scheme_set("pink")
#' x <- example_mcmc_draws(params = 5)
#' mcmc_parcoord(x)
#' mcmc_parcoord(x, regex_pars = "beta")
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
#' div_style <- parcoord_style_np(div_color = "green", div_size = 0.3, div_alpha = 0.3)
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
           alpha = 0.3,
           np = NULL,
           np_style = parcoord_style_np()) {
    check_ignored_arguments(...)
    stopifnot(inherits(np_style, "nuts_style"))

    data <-
      mcmc_parcoord_data(
        x = x,
        pars = pars,
        regex_pars = regex_pars,
        transformations = transformations,
        np = np
      )

    # split into divergent and not divergent
    divg <- sym("Divergent")
    draws <- dplyr::filter(data, UQ(divg) == 0)
    div_draws <- dplyr::filter(data, UQ(divg) == 1)
    has_divs <- nrow(div_draws) > 0

    graph <- ggplot(draws, aes_(
      x = ~ Parameter,
      y = ~ Value,
      group = ~ factor(Draw)
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
      scale_x_discrete(expand = c(0,0), labels = levels(draws$Parameter)) +
      labs(x = NULL, y = NULL)
  }

#' @rdname MCMC-parcoord
#' @export
#' @importFrom dplyr n right_join mutate group_by ungroup select arrange rename
mcmc_parcoord_data <-
  function(x,
           pars = character(),
           regex_pars = character(),
           transformations = list(),
           np = NULL
           ) {
    x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
    long_d <- reshape2::melt(x, varnames = c("Iteration", "Chain", "Parameter"),
                             value.name = "Value")

    n_param <- num_params(long_d)
    if (n_param < 2)
      stop("'mcmc_parcoord' requires at least two parameters in 'x'.")

    param <- sym("Parameter")
    value <- sym("Value")
    if (is.null(np)) {
      # still include 'Divergent' so returned object always has same columns
      long_d$Divergent <- 0
    } else {
      # join with divergence info (both long_d and np have columns
      # 'Parameter' and 'Value' so need to be a little careful)
      np <- validate_nuts_data_frame(np)
      long_d <- np %>%
        dplyr::filter(UQ(param) == "divergent__") %>%
        select(- !!param) %>%
        rename(Divergent = !!value) %>%
        right_join(long_d, by = c("Iteration", "Chain"))
    }

    keep <- syms(c("Draw", "Parameter", "Value", "Divergent"))
    long_d %>%
      group_by(!! param) %>%
      mutate(Draw = 1:n()) %>%
      ungroup() %>%
      select(!!! keep)
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