[![Travis-CI Build Status](https://travis-ci.org/stan-dev/bayesplot.svg?branch=master)](https://travis-ci.org/stan-dev/bayesplot)
[![codecov](https://codecov.io/gh/stan-dev/bayesplot/branch/master/graph/badge.svg)](https://codecov.io/gh/stan-dev/bayesplot)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/bayesplot?color=blue)](http://cran.r-project.org/web/packages/bayesplot)
[![Downloads](http://cranlogs.r-pkg.org/badges/bayesplot?color=blue)](http://cran.rstudio.com/package=bayesplot)

<br>

<div style="text-align:center">
<a href="http://mc-stan.org">
<img src="https://raw.githubusercontent.com/stan-dev/logos/master/logo.png" width=200 alt="Stan Logo"/>
</a>
</div>

<br>

<a href="http://mc-stan.org/assets/img/bayesplot/mcmc_areas-rstanarm.png">
<img src="http://mc-stan.org/assets/img/bayesplot/mcmc_areas-rstanarm.png" style="width: 15%;"/>
</a>
<a href="http://mc-stan.org/assets/img/bayesplot/mcmc_nuts_energy-rstan.png">
<img src="http://mc-stan.org/assets/img/bayesplot/mcmc_nuts_energy-rstan.png" style="width: 15%;"/>
</a>
<a href="http://mc-stan.org/assets/img/bayesplot/mcmc_trace-rstan.png">
<img src="http://mc-stan.org/assets/img/bayesplot/mcmc_trace-rstan.png" style="width: 15%;"/>
</a>
<a href="http://mc-stan.org/assets/img/bayesplot/ppc_dens_overlay-rstanarm.png">
<img src="http://mc-stan.org/assets/img/bayesplot/ppc_dens_overlay-rstanarm.png" style="width: 15%;"/>
</a>
<a href="http://mc-stan.org/assets/img/bayesplot/ppc_intervals-rstanarm.png">
<img src="http://mc-stan.org/assets/img/bayesplot/ppc_intervals-rstanarm.png" style="width: 15%;"/>
</a>
<a href="http://mc-stan.org/assets/img/bayesplot/ppc_stat-rstanarm.png">
<img src="http://mc-stan.org/assets/img/bayesplot/ppc_stat-rstanarm.png" style="width: 15%;"/>
</a>


**bayesplot** is an R package providing an extensive library of plotting
functions for use after fitting Bayesian models (typically with MCMC). Currently
**bayesplot** offers a variety of plots of posterior draws, visual MCMC
diagnostics, as well as graphical posterior predictive checking. Additional
functionality (e.g. for forecasting/out-of-sample prediction and other
inference-related tasks) will be added in future releases.

The plots created by **bayesplot** are ggplot objects, which means that after 
a plot is created it can be further customized using the various functions for 
modifying ggplot objects provided by the **ggplot2** package.

The idea behind **bayesplot** is not only to provide convenient functionality
for users, but also a common set of functions that can be easily used by
developers working on a variety of packages for Bayesian modeling, particularly
(but not necessarily) those powered by [**RStan**](http://mc-stan.org/rstan).



## Installation

* Install from CRAN:

```r
install.packages("bayesplot")
```

* Install latest development version from GitHub (requires [devtools](https://github.com/hadley/devtools) package):

```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("stan-dev/bayesplot", dependencies = TRUE, build_vignettes = TRUE)
```

You can also set `build_vignettes=FALSE` for a faster installation from GitHub (the vignettes 
can always be accessed online anytime at [mc-stan.org/bayesplot/articles](http://mc-stan.org/bayesplot/articles/)).

If you are not using the [RStudio IDE](https://www.rstudio.com/) and you get an
error related to "pandoc" you will either need to remove the argument
`build_vignettes=TRUE` (to avoid building the vignettes) or install
[pandoc](http://pandoc.org/) (e.g., `brew install pandoc`) and probably also
pandoc-citeproc (e.g., `brew install pandoc-citeproc`). If you have the
`rmarkdown` R package installed then you can check if you have pandoc by running
the following in R:

```r
rmarkdown::pandoc_available()
```


## Examples

Some quick examples using MCMC draws obtained from our [__rstanarm__](http://mc-stan.org/rstanarm)
and [__rstan__](http://mc-stan.org/rstan) packages.

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

<div style="text-align:center">
<a href="https://raw.githubusercontent.com/stan-dev/bayesplot/master/images/mcmc_areas-rstanarm.png">
<img src=https://raw.githubusercontent.com/stan-dev/bayesplot/master/images/mcmc_areas-rstanarm.png width=50%/>
</a>
</div>

```r
color_scheme_set("red")
ppc_dens_overlay(y = fit$y,
                 yrep = posterior_predict(fit, draws = 50))
```

<div style="text-align:center">
<a href="https://raw.githubusercontent.com/stan-dev/bayesplot/master/images/ppc_dens_overlay-rstanarm.png">
<img src=https://raw.githubusercontent.com/stan-dev/bayesplot/master/images/ppc_dens_overlay-rstanarm.png width=50%/>
</a>
</div>

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

<div style="text-align:center">
<a href="https://raw.githubusercontent.com/stan-dev/bayesplot/master/images/ppc_stat_grouped-rstanarm.png">
<img src=https://raw.githubusercontent.com/stan-dev/bayesplot/master/images/ppc_stat_grouped-rstanarm.png width=50%/>
</a>
</div>

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

<div style="text-align:center">
<a href="https://raw.githubusercontent.com/stan-dev/bayesplot/master/images/mcmc_trace-rstan.png">
<img src=https://raw.githubusercontent.com/stan-dev/bayesplot/master/images/mcmc_trace-rstan.png width=50% />
</a>
</div>

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

<div style="text-align:center">
<a href="https://raw.githubusercontent.com/stan-dev/bayesplot/master/images/mcmc_scatter-rstan.png">
<img src=https://raw.githubusercontent.com/stan-dev/bayesplot/master/images/mcmc_scatter-rstan.png width=50% />
</a>
</div>

```r
color_scheme_set("red")
np <- nuts_params(fit2)
mcmc_nuts_energy(np) + ggtitle("NUTS Energy Diagnostic")
```

<div style="text-align:center">
<a href="https://raw.githubusercontent.com/stan-dev/bayesplot/master/images/mcmc_nuts_energy-rstan.png">
<img src=https://raw.githubusercontent.com/stan-dev/bayesplot/master/images/mcmc_nuts_energy-rstan.png width=50% />
</a>
</div>

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

<div style="text-align:center">
<a href="https://raw.githubusercontent.com/stan-dev/bayesplot/master/images/ppc_intervals-rstanarm.png">
<img src=https://raw.githubusercontent.com/stan-dev/bayesplot/master/images/ppc_intervals-rstanarm.png width=55% />
</a>
</div>
