# bayesplot

[![Travis-CI Build Status](https://travis-ci.org/jgabry/ppcheck.svg?branch=master)](https://travis-ci.org/jgabry/ppcheck)
[![codecov](https://codecov.io/gh/jgabry/ppcheck/branch/master/graph/badge.svg)](https://codecov.io/gh/jgabry/ppcheck)


Early draft of an R package providing a library a plotting functions for use
after fitting Bayesian models by MCMC. The idea is to provide a common set of
functions that can be easily used by developers working on a variety of packages
for Bayesian modeling.


#### Installation

```{r}
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("jgabry/ppcheck", build_vignettes = TRUE)
```
