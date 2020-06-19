# bayesplot <img src="man/figures/stanlogo.png" align="right" width="120" />

<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/bayesplot?color=blue)](https://cran.r-project.org/web/packages/bayesplot)
[![Downloads](https://cranlogs.r-pkg.org/badges/bayesplot?color=blue)](https://cran.rstudio.com/package=bayesplot)
[![R-CMD-check](https://github.com/stan-dev/bayesplot/workflows/R-CMD-check/badge.svg)](https://github.com/stan-dev/bayesplot/actions?workflow=R-CMD-check)
[![codecov](https://codecov.io/gh/stan-dev/bayesplot/branch/master/graph/badge.svg)](https://codecov.io/gh/stan-dev/bayesplot)
<!-- badges: end -->

### Overview

**bayesplot** is an R package providing an extensive library of plotting
functions for use after fitting Bayesian models (typically with MCMC). 
The plots created by **bayesplot** are ggplot objects, which means that after 
a plot is created it can be further customized using various functions from
the **ggplot2** package. 

Currently **bayesplot** offers a variety of plots of posterior draws, 
visual MCMC diagnostics, and graphical posterior (or prior) predictive checking. 
Additional functionality (e.g. for forecasting/out-of-sample prediction and other
inference-related tasks) will be added in future releases.

The idea behind **bayesplot** is not only to provide convenient functionality
for users, but also a common set of functions that can be easily used by
developers working on a variety of packages for Bayesian modeling, particularly
(but not necessarily) those powered by [**RStan**](https://mc-stan.org/rstan).

### Getting started 

If you are just getting started with **bayesplot** we recommend starting with
the tutorial [vignettes](https://mc-stan.org/bayesplot/articles/index.html), 
the examples throughout the package [documentation](https://mc-stan.org/bayesplot/reference/index.html), 
and the paper _Visualization in Bayesian workflow_:

* Gabry J, Simpson D, Vehtari A, Betancourt M, Gelman A (2019). Visualization in Bayesian workflow. 
_J. R. Stat. Soc. A_, 182: 389-402. doi:10.1111/rssa.12378. 
([journal version](https://rss.onlinelibrary.wiley.com/doi/full/10.1111/rssa.12378),
[arXiv preprint](https://arxiv.org/abs/1709.01449),
[code on GitHub](https://github.com/jgabry/bayes-vis-paper))

### Resources

* [mc-stan.org/bayesplot](https://mc-stan.org/bayesplot) (online documentation, vignettes)
* [Ask a question](https://discourse.mc-stan.org) (Stan Forums on Discourse)
* [Open an issue](https://github.com/stan-dev/bayesplot/issues) (GitHub issues for bug reports, feature requests)

### Installation

* Install from CRAN:

```r
install.packages("bayesplot")
```

* Install latest development version from GitHub (requires [devtools](https://github.com/hadley/devtools) package):

```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("stan-dev/bayesplot", dependencies = TRUE, build_vignettes = FALSE)
```

This installation won't include the vignettes (they take some time to build), but all of the vignettes are 
available online at [mc-stan.org/bayesplot/articles](https://mc-stan.org/bayesplot/articles/).


### Examples

Some quick examples using MCMC draws obtained from the [__rstanarm__](https://mc-stan.org/rstanarm) 
and [__rstan__](https://mc-stan.org/rstann) packages.

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

<img src=https://github.com/stan-dev/bayesplot/blob/master/images/mcmc_areas-rstanarm.png width=50%/>

```r
color_scheme_set("red")
ppc_dens_overlay(y = fit$y, 
                 yrep = posterior_predict(fit, draws = 50))
```

<img src=https://github.com/stan-dev/bayesplot/blob/master/images/ppc_dens_overlay-rstanarm.png width=50%/>

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

<img src=https://github.com/stan-dev/bayesplot/blob/master/images/ppc_stat_grouped-rstanarm.png width=50%/>

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

<img src=https://github.com/stan-dev/bayesplot/blob/master/images/mcmc_trace-rstan.png width=50% />

```r
# scatter plot also showing divergences
color_scheme_set("darkgray")
mcmc_scatter(
  as.matrix(fit2),
  pars = c("tau", "theta[1]"), 
  np = nuts_params(fit2), 
  np_style = scatter_style_np(div_color = "green", div_alpha = 0.8)
)
```

<img src=https://github.com/stan-dev/bayesplot/blob/master/images/mcmc_scatter-rstan.png width=50% />

```r
color_scheme_set("red")
np <- nuts_params(fit2)
mcmc_nuts_energy(np) + ggtitle("NUTS Energy Diagnostic")
```

<img src=https://github.com/stan-dev/bayesplot/blob/master/images/mcmc_nuts_energy-rstan.png width=50% />

```r
# another example with rstanarm
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

<img src=https://github.com/stan-dev/bayesplot/blob/master/images/ppc_intervals-rstanarm.png width=55% />
