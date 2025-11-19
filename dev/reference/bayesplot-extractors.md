# Extract quantities needed for plotting from model objects

Generics and methods for extracting quantities needed for plotting from
various types of model objects. Currently methods are provided for
stanfit (**rstan**), CmdStanMCMC (**cmdstanr**), and stanreg
(**rstanarm**) objects, but adding new methods should be relatively
straightforward.

## Usage

``` r
log_posterior(object, ...)

nuts_params(object, ...)

rhat(object, ...)

neff_ratio(object, ...)

# S3 method for class 'stanfit'
log_posterior(object, inc_warmup = FALSE, ...)

# S3 method for class 'stanreg'
log_posterior(object, inc_warmup = FALSE, ...)

# S3 method for class 'CmdStanMCMC'
log_posterior(object, inc_warmup = FALSE, ...)

# S3 method for class 'stanfit'
nuts_params(object, pars = NULL, inc_warmup = FALSE, ...)

# S3 method for class 'stanreg'
nuts_params(object, pars = NULL, inc_warmup = FALSE, ...)

# S3 method for class 'list'
nuts_params(object, pars = NULL, ...)

# S3 method for class 'CmdStanMCMC'
nuts_params(object, pars = NULL, ...)

# S3 method for class 'stanfit'
rhat(object, pars = NULL, ...)

# S3 method for class 'stanreg'
rhat(object, pars = NULL, regex_pars = NULL, ...)

# S3 method for class 'CmdStanMCMC'
rhat(object, pars = NULL, ...)

# S3 method for class 'stanfit'
neff_ratio(object, pars = NULL, ...)

# S3 method for class 'stanreg'
neff_ratio(object, pars = NULL, regex_pars = NULL, ...)

# S3 method for class 'CmdStanMCMC'
neff_ratio(object, pars = NULL, ...)
```

## Arguments

- object:

  The object to use.

- ...:

  Arguments passed to individual methods.

- inc_warmup:

  A logical scalar (defaulting to `FALSE`) indicating whether to include
  warmup draws, if applicable.

- pars:

  An optional character vector of parameter names. For `nuts_params()`
  these will be NUTS sampler parameter names rather than model
  parameters. If `pars` is omitted all parameters are included.

- regex_pars:

  An optional [regular expression](https://rdrr.io/r/base/grep.html) to
  use for parameter selection. Can be specified instead of `pars` or in
  addition to `pars`. When using `pars` for tidy parameter selection,
  the `regex_pars` argument is ignored since [select
  helpers](https://tidyselect.r-lib.org/reference/language.html) perform
  a similar function.

## Value

- `log_posterior()`:

  `log_posterior()` methods return a molten data frame (see
  [`reshape2::melt()`](https://rdrr.io/pkg/reshape2/man/melt.html)). The
  data frame should have columns `"Iteration"` (integer), `"Chain"`
  (integer), and `"Value"` (numeric). See **Examples**, below.

- `nuts_params()`:

  `nuts_params()` methods return a molten data frame (see
  [`reshape2::melt()`](https://rdrr.io/pkg/reshape2/man/melt.html)). The
  data frame should have columns `"Parameter"` (factor), `"Iteration"`
  (integer), `"Chain"` (integer), and `"Value"` (numeric). See
  **Examples**, below.

- `rhat()`, `neff_ratio()`:

  Methods return (named) vectors.

## See also

[MCMC-nuts](https://mc-stan.org/bayesplot/dev/reference/MCMC-nuts.md),
[MCMC-diagnostics](https://mc-stan.org/bayesplot/dev/reference/MCMC-diagnostics.md)

## Examples

``` r
# \dontrun{
library(rstanarm)
fit <- stan_glm(mpg ~ wt, data = mtcars, refresh = 0)

np <- nuts_params(fit)
head(np)
#>   Chain Iteration     Parameter     Value
#> 1     1         1 accept_stat__ 1.0000000
#> 2     1         2 accept_stat__ 0.9466923
#> 3     1         3 accept_stat__ 0.9904577
#> 4     1         4 accept_stat__ 0.9034513
#> 5     1         5 accept_stat__ 0.9068563
#> 6     1         6 accept_stat__ 0.9517777
tail(np)
#>       Chain Iteration Parameter    Value
#> 23995     4       995  energy__ 87.72738
#> 23996     4       996  energy__ 86.46415
#> 23997     4       997  energy__ 86.80967
#> 23998     4       998  energy__ 86.47497
#> 23999     4       999  energy__ 88.53321
#> 24000     4      1000  energy__ 88.01486

lp <- log_posterior(fit)
head(lp)
#>   Chain Iteration     Value
#> 1     1         1 -86.12467
#> 2     1         2 -86.80760
#> 3     1         3 -86.45860
#> 4     1         4 -86.53618
#> 5     1         5 -86.71946
#> 6     1         6 -88.15369
tail(lp)
#>      Chain Iteration     Value
#> 3995     4       995 -86.19753
#> 3996     4       996 -86.36249
#> 3997     4       997 -86.38378
#> 3998     4       998 -85.83448
#> 3999     4       999 -86.94738
#> 4000     4      1000 -86.36645
# }
```
