# Just some scratch paper as I develop the basic functionality

library(ggplot2)
library(ggjoy)

x <- example_mcmc_draws()
pars = character()
regex_pars = character()
transformations = list()
interval_width = 1


#' @template args-mcmc-x
#' @template args-pars
#' @template args-regex_pars
#' @template args-transformations
#' @template args-density-controls
#' @rdname MCMC-distributions
#' @export
mcmc_dens_ridge <- function(x,
                           pars = character(),
                           regex_pars = character(),
                           transformations = list(),
                           ...,
                           interval_width = 1,
                           bw = NULL,
                           adjust = NULL,
                           kernel = NULL) {
  check_ignored_arguments(...)

  data <- mcmc_densridge_data(
    x, by_chain = FALSE, pars = pars, regex_pars = regex_pars,
    transformations = transformations,
    interval_width = interval_width, bw = bw, adjust = adjust, kernel = kernel)

  ggplot(data) +
    aes_(x = ~ x, height = ~ density, y = ~ Parameter) +
    geom_joy(stat = "identity",
             fill = get_color("light"),
             color = get_color("mid"),
             size = 1) +
    xaxis_title(FALSE) +
    yaxis_title(FALSE) +
    yaxis_ticks(size = 1) +
    grid_lines_y(color = "grey75") +
    theme(
      axis.text.y = element_text(
        hjust = 1,
        vjust = 0,
        face = "bold"
      ))
}


mcmc_densridge_chains <- function(x,
                                  pars = character(),
                                  regex_pars = character(),
                                  transformations = list(),
                                  ...,
                                  interval_width = 1,
                                  bw = NULL,
                                  adjust = NULL,
                                  kernel = NULL) {
  check_ignored_arguments(...)

  data <- mcmc_densridge_data(
    x, by_chain = TRUE, pars = pars, regex_pars = regex_pars,
    transformations = transformations,
    interval_width = interval_width, bw = bw, adjust = adjust, kernel = kernel)

  n_chain <- num_chains(data)
  n_param <- num_params(data)

  # If parameters are more than ?, warn and use a manual color instead?
  if (n_param > 10) {
    NULL
  }

  # Joyplots are shapes. The line is a border and the area under has a fill.
  # Setting fill to NA makes the joyplot appear to be just a density curve. But
  # ggplot2 shows a legend for a shape (a box). We say show `show.legend =
  # FALSE` so that the joyplot is not used when constructing the legend. To
  # have a legend for joyplot lines, we draw invisible lines and use
  # `override.aes` to make sure the colors appear in the legend.
  ggplot(data) +
    aes_(x = ~ x, height = ~ density, y = ~ Parameter, color = ~ Chain,
        group = ~ interaction(Chain, Parameter)) +
    geom_joy(stat = "identity", fill = NA, show.legend = FALSE) +
    geom_line(alpha = 0) +
    scale_color_manual(values = chain_colors(n_chain),
                       guide = guide_legend(override.aes = list(alpha = 1))) +
    yaxis_title(FALSE) +
    yaxis_ticks(size = 1) +
    xaxis_title(FALSE) +
    grid_lines_y(color = "grey75") +
    theme(
      axis.text.y = element_text(
        hjust = 1,
        vjust = 0,
        face = "bold"
      ))
}



mcmc_dens_ridges_data <- function(x,
                                 pars = character(),
                                 regex_pars = character(),
                                 transformations = list(),
                                 ...,
                                 interval_width = 1,
                                 bw = NULL, adjust = NULL, kernel = NULL) {

  .mcmc_dens_ridges_data(x, by_chain = FALSE, pars, regex_pars,
                         transformations, interval_width, bw, adjust, kernel)
}

.mcmc_dens_ridges_data <- function(x,
                                  by_chain = FALSE
                                  pars = character(),
                                  regex_pars = character(),
                                  transformations = list(),
                                  interval_width = 1,
                                  bw = NULL, adjust = NULL, kernel = NULL) {
  x_tall <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  data <- reshape2::melt(x_tall, value.name = "Value")
  data$Chain <- factor(data$Chain)

  var_names <- if (by_chain) c("Parameter", "Chain") else "Parameter"
  vars <- syms(var_names)

  compute_column_density(
    df = data,
    group_vars = c(!!! vars),
    value_var = !! sym("Value"),
    ...)
}

f <- function() {




}
scheme_level_names()
names(get_color())


data <- mcmc_densridge_data(x, by_chain = FALSE, regex_pars = "b.*Subject", n = 2000)


m <- rstanarm::stan_glmer(Reaction ~ 1 + (1 | Subject), lme4::sleepstudy, family = gaussian)
x <- as.array(m)
pars <- names(m$coefficients)



names(x)
m <- rstanarm::stan_glm(Sepal.Length ~ . - 1, iris, family = gaussian)
pars <- names(coef(m))
x <- as.array(m)
lines <-
"model <- lm(
  # this is the formula
  y ~ x,
  data = data.frame(x = rnorm(10), y = rnorm(10))
)
"
cat(lines)
lme4::sleepstudy









