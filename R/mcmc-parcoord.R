#' Parallel coordinates plot of MCMC draws
#'
#' Parallel coordinates plot of MCMC draws (one dimension per parameter).
#' See the **Plot Descriptions** section below for details,
#' and see [Gabry et al. (2019)](https://github.com/jgabry/bayes-vis-paper#readme)
#' for more background and a real example.
#'
#' @name MCMC-parcoord
#' @family MCMC
#'
#' @template args-mcmc-x
#' @template args-pars
#' @template args-regex_pars
#' @template args-transformations
#' @param ... Currently ignored.
#' @param size,alpha Arguments passed on to [ggplot2::geom_line()].
#' @param np For models fit using [NUTS] (more generally,
#'   any [symplectic integrator](https://en.wikipedia.org/wiki/Symplectic_integrator)),
#'   an optional data frame providing NUTS diagnostic information. The data
#'   frame should be the object returned by [nuts_params()] or one with the same
#'   structure.
#' @param np_style A call to the `parcoord_style_np()` helper function to
#'   specify arguments controlling the appearance of superimposed lines
#'   representing NUTS diagnostics (in this case divergences) if the `np`
#'   argument is specified.
#'
#' @template return-ggplot-or-data
#'
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{`mcmc_parcoord()`}{
#'    [Parallel coordinates plot](https://en.wikipedia.org/wiki/Parallel_coordinates)
#'    of MCMC draws. There is one dimension per parameter along the horizontal
#'    axis and each set of connected line segments represents a single MCMC draw
#'    (i.e., a vector of length equal to the number of parameters).
#'
#'    The parallel coordinates plot is most useful if the optional HMC/NUTS
#'    diagnostic information is provided via the `np` argument. In that
#'    case divergences are highlighted in the plot. The appearance of the
#'    divergences can be customized using the `np_style` argument and the
#'    `parcoord_style_np` helper function. This version of the plot is the
#'    same as the parallel coordinates plot described in Gabry et al. (2019).
#'
#'    When the plotted model parameters are on very different scales the
#'    `transformations` argument can be useful. For example, to standardize
#'    all variables before plotting you could use function `(x - mean(x))/sd(x)`
#'    when specifying the `transformations` argument to
#'    `mcmc_parcoord`. See the **Examples** section for how to do this.
#'   }
#' }
#'
#' @template reference-vis-paper
#' @references Hartikainen, A. (2017, Aug 23). Concentration of divergences
#' (Msg 21). Message posted to The Stan Forums:
#' <https://discourse.mc-stan.org/t/concentration-of-divergences/1590/21>.
#'
#' @examples
#' color_scheme_set("pink")
#' x <- example_mcmc_draws(params = 5)
#' mcmc_parcoord(x)
#' mcmc_parcoord(x, regex_pars = "beta")
#'
#' \dontrun{
#' # Example using a Stan demo model
#' library(rstan)
#' fit <- stan_demo("eight_schools")
#' draws <- as.array(fit, pars = c("mu", "tau", "theta", "lp__"))
#' np <- nuts_params(fit)
#' str(np)
#' levels(np$Parameter)
#'
#' color_scheme_set("brightblue")
#' mcmc_parcoord(draws, alpha = 0.05)
#' mcmc_parcoord(draws, np = np)
#'
#' # customize appearance of divergences
#' color_scheme_set("darkgray")
#' div_style <- parcoord_style_np(div_color = "green", div_size = 0.05, div_alpha = 0.4)
#' mcmc_parcoord(draws, size = 0.25, alpha = 0.1,
#'               np = np, np_style = div_style)
#'
#' # to use a transformation (e.g., standardizing all the variables can be helpful)
#' # specify the 'transformations' argument (though partial argument name
#' # matching means we can just use 'trans' or 'transform')
#' mcmc_parcoord(
#'   draws,
#'   transform = function(x) {(x - mean(x)) / sd(x)},
#'   size = 0.25,
#'   alpha = 0.1,
#'   np = np,
#'   np_style = div_style
#'  )
#'
#' # mcmc_parcoord_data returns just the data in a conventient form for plotting
#' d <- mcmc_parcoord_data(x, np = np)
#' head(d)
#' tail(d)
#'}
#'
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


    divg <- sym("Divergent")
    draws <- dplyr::filter(data, UQ(divg) == 0)
    div_draws <- dplyr::filter(data, UQ(divg) == 1)
    has_divs <- isTRUE(nrow(div_draws) > 0)

    graph <- ggplot(draws, aes(
      x = .data$Parameter,
      y = .data$Value,
      group = factor(.data$Draw)
    )) +
      geom_line(
        linewidth = size,
        alpha = alpha,
        color = get_color("dh")
      ) +
      bayesplot_theme_get()

    if (has_divs) {
      graph <- graph +
        geom_line(
          data = div_draws,
          linewidth = np_style$size[["div"]],
          alpha = np_style$alpha[["div"]],
          color = np_style$color[["div"]]
        )
    }

    graph +
      scale_x_discrete(expand = c(0,0), labels = levels(draws$Parameter)) +
      expand_limits(x = nlevels(draws$Parameter) + 0.25) +
      labs(x = NULL, y = NULL)
  }

#' @rdname MCMC-parcoord
#' @export
#' @importFrom dplyr n left_join mutate group_by ungroup select arrange rename
mcmc_parcoord_data <-
  function(x,
           pars = character(),
           regex_pars = character(),
           transformations = list(),
           np = NULL
           ) {
    x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
    long_d <- melt_mcmc(x)

    if (num_params(long_d) < 2) {
      abort("'mcmc_parcoord' requires at least two parameters in 'x'.")
    }

    param <- sym("Parameter")
    value <- sym("Value")
    if (is.null(np)) {
      # still include 'Divergent' so returned object always has same columns
      long_d$Divergent <- 0
    } else {
      # join with divergence info (both long_d and np have columns
      # 'Parameter' and 'Value' so need to be a little careful)
      divs <- np %>%
        validate_nuts_data_frame() %>%
        dplyr::filter(UQ(param) == "divergent__") %>%
        select(- !!param) %>%
        rename("Divergent" = !!value)

      long_d <- left_join(long_d, divs, by = c("Iteration", "Chain"))
    }

    keep_cols <- syms(c("Draw", "Parameter", "Value", "Divergent"))
    long_d %>%
      group_by(!! param) %>%
      mutate(Draw = 1:n()) %>%
      ungroup() %>%
      select(!!! keep_cols)
  }


#' @rdname MCMC-parcoord
#' @export
#' @param div_color,div_size,div_alpha Optional arguments to the
#'   `parcoord_style_np()` helper function that are eventually passed to
#'   [ggplot2::geom_line()] if the `np` argument is also specified. They control
#'   the color, size, and transparency specifications for showing divergences in
#'   the plot. The default values are displayed in the **Usage** section above.
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
