# bayesplot

[![Travis-CI Build Status](https://travis-ci.org/jgabry/bayesplot.svg?branch=master)](https://travis-ci.org/jgabry/bayesplot)
[![codecov](https://codecov.io/gh/jgabry/bayesplot/branch/master/graph/badge.svg)](https://codecov.io/gh/jgabry/bayesplot)

An R package providing a library of plotting functions for use after fitting 
Bayesian models (typically with MCMC). The idea is not only to provide
convenient functionality for users, but also a common set of functions that can
be easily used by developers working on a variety of packages for Bayesian
modeling, particularly (but not necessarily) those powered by 
[**RStan**](https://github.com/stan-dev/rstan).


#### Installation

**bayesplot** is not yet on CRAN (coming soon) but can be installed from GitHub
using the **devtools** package.

```{r}
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("jgabry/bayesplot", build_vignettes = TRUE)
```
