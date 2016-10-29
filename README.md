# bayesplot

[![Travis-CI Build Status](https://travis-ci.org/stan-dev/bayesplot.svg?branch=master)](https://travis-ci.org/stan-dev/bayesplot)
[![codecov](https://codecov.io/gh/stan-dev/bayesplot/branch/master/graph/badge.svg)](https://codecov.io/gh/stan-dev/bayesplot)

An R package providing a library of plotting functions for use after fitting 
Bayesian models (typically with MCMC). The idea is not only to provide
convenient functionality for users, but also a common set of functions that can
be easily used by developers working on a variety of packages for Bayesian
modeling, particularly (but not necessarily) those powered by 
[**RStan**](https://github.com/stan-dev/rstan).

The plots created by **bayesplot** are ggplot objects, which means that after 
a plot is created it can be further customized using the various functions for 
modifying ggplot objects provided by the **ggplot2** package.

### Installation

**bayesplot** is not yet on CRAN (coming soon) but can be installed from GitHub 
using the **devtools** package. You will also need the preview version of the 
upcoming **ggplot2** update.

```r
if (!require("devtools"))
  install.packages("devtools")

devtools::install_github("hadley/ggplot2")
devtools::install_github("stan-dev/bayesplot", build_vignettes = TRUE)
```

### Examples
Some quick examples using MCMC draws obtained from the __rstanarm__ and __rstan__ packages.

```r
library("bayesplot")
library("rstanarm")
library("ggplot2")

fit <- stan_glm(mpg ~ ., data = mtcars)
posterior <- as.matrix(fit)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(posterior, 
           pars = c("cyl", "drat", "am", "wt"), 
           prob = 0.8) + plot_title
```
<img src=https://github.com/jgabry/bayesplot/blob/master/images/mcmc_areas-rstanarm.png width=50% />
```r
color_scheme_set("red")
ppc_dens_overlay(y = fit$y, 
                 yrep = posterior_predict(fit, draws = 50))
```
<img src=https://github.com/jgabry/bayesplot/blob/master/images/ppc_dens_overlay-rstanarm.png width=50% />
```r
# also works nicely with piping
library("dplyr")
color_scheme_set("brightblue")
fit %>% 
  posterior_predict(draws = 500) %>%
  ppc_stat_grouped(y = mtcars$mpg, 
                   group = mtcars$carb, 
                   stat = "median")

```
<img src=https://github.com/jgabry/bayesplot/blob/master/images/ppc_stat_grouped-rstanarm.png width=50% />
```r
# with rstan demo model
library("rstan")
fit2 <- stan_demo("eight_schools", warmup = 300, iter = 700)
posterior2 <- extract(fit2, inc_warmup = TRUE, permuted = FALSE)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior2,  pars = c("mu", "tau"), n_warmup = 300,
                facet_args = list(nrow = 2, labeller = label_parsed))
p + facet_text(size = 15)
```
<img src=https://github.com/jgabry/bayesplot/blob/master/images/mcmc_trace-rstan.png width=50% />
```r
color_scheme_set("red")
np <- nuts_params(fit2)
mcmc_nuts_energy(np, merge_chains = FALSE) + ggtitle("NUTS Energy Diagnostic")
```
<img src=https://github.com/jgabry/bayesplot/blob/master/images/mcmc_nuts_energy-rstan.png width=50% />
```r
# more exampels with rstanarm
color_scheme_set("purple")

fit <- stan_glmer(mpg ~ wt + (1|cyl), data = mtcars)
ppc_intervals(
  y = mtcars$mpg,
  yrep = posterior_predict(fit),
  x = mtcars$wt,
  prob = 0.5
) +
  labs(
    x = "Weight (1000 lbs)",
    y = "MPG",
    title = "50% posterior predictive intervals \nvs observed miles per gallon",
    subtitle = "by vehicle weight"
  ) +
  panel_bg(fill = "gray95", color = NA) +
  grid_lines(color = "white")
```
<img src=https://github.com/jgabry/bayesplot/blob/master/images/ppc_intervals-rstanarm.png width=50% />
