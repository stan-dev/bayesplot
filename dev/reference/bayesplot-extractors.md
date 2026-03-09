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
#> 2     1         2 accept_stat__ 0.9128983
#> 3     1         3 accept_stat__ 1.0000000
#> 4     1         4 accept_stat__ 0.9318817
#> 5     1         5 accept_stat__ 0.9799518
#> 6     1         6 accept_stat__ 0.9476718
tail(np)
#>       Chain Iteration Parameter    Value
#> 23995     4       995  energy__ 88.87303
#> 23996     4       996  energy__ 89.30687
#> 23997     4       997  energy__ 88.59940
#> 23998     4       998  energy__ 86.71444
#> 23999     4       999  energy__ 86.85860
#> 24000     4      1000  energy__ 87.95147

lp <- log_posterior(fit)
head(lp)
#>   Chain Iteration     Value
#> 1     1         1 -87.22566
#> 2     1         2 -87.77245
#> 3     1         3 -87.54427
#> 4     1         4 -88.95054
#> 5     1         5 -85.92995
#> 6     1         6 -86.47992
tail(lp)
#>      Chain Iteration     Value
#> 3995     4       995 -86.72768
#> 3996     4       996 -87.57157
#> 3997     4       997 -86.38115
#> 3998     4       998 -86.35389
#> 3999     4       999 -86.67260
#> 4000     4      1000 -87.41818
# }
```
