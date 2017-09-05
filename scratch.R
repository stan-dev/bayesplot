library(ggplot2)
library(ggjoy)

x <- example_mcmc_draws()
pars = character()
regex_pars = character()
transformations = list()
interval_width = 1


# Just some scratch paper as I develop the basic functionality

mcmc_joyplot <- function(x,
                         pars = character(),
                         regex_pars = character(),
                         transformations = list(),
                         ...,
                         prob = 1,
                         bw = NULL,
                         adjust = NULL,
                         kernel = NULL) {



  x <- prepare_mcmc_array(example_mcmc_draws(chains = 4, params = 5), pars, regex_pars = "beta", transformations)
  data <- reshape2::melt(x, value.name = "Value")
  data$Chain <- factor(data$Chain)
  n_param <- num_params(data)
  n_chain <- num_chains(data)

  grouped1 <- compute_column_density(data, c(Parameter, Chain),
                                     Value, interval_width = 1)

  # Joyplots are shapes. The line is a border and the area under has a fill.
  # Setting fill to NA makes the joyplot appear to be just a density curve. But
  # ggplot2 shows a legend for a shape (a box). We say show `show.legend =
  # FALSE` so that the joyplot is not used when constructing the legend. To
  # have a legend for joyplot lines, we draw invisible lines and use
  # `override.aes` to make sure the colors appear in the legend.
  ggplot(grouped1) +
    aes(x = x, height = density, y = Parameter, color = Chain) +
    geom_joy(stat = "identity", fill = NA, show.legend = FALSE) +
    geom_line(alpha = 0) +
    scale_color_manual(values = chain_colors(n_chain),
                       guide = guide_legend(override.aes = list(alpha = 1))) +
    yaxis_text(face = "bold") +
    yaxis_title(FALSE) +
    yaxis_ticks(size = 1) +
    xaxis_title(FALSE)
  ggsave("jp0.png", width = 6, height = 3)


  color_scheme_set("brightblue")
  grouped <- compute_column_density(data, c(Parameter), Value, interval_width = 1)

  ggplot(grouped) +
    aes(x = x, height = density, y = Parameter) +
    geom_joy(stat = "identity", show.legend = FALSE,
             fill = get_color("mid"),
             color = get_color("mid_highlight")) +
  yaxis_text(face = "bold") +
  yaxis_title(FALSE) +
  yaxis_ticks(size = 1) +
  xaxis_title(FALSE)
  ggsave("jp1.png", width = 6, height = 3)

}
