# Example draws to use in demonstrations and tests

These functions return various objects containing data used in the
examples throughout the **bayesplot** package documentation.

## Usage

``` r
example_mcmc_draws(chains = 4, params = 4)

example_yrep_draws()

example_y_data()

example_x_data()

example_group_data()
```

## Arguments

- chains:

  An integer between 1 and 4 indicating the desired number of chains.

- params:

  An integer between 1 and 6 indicating the desired number of
  parameters.

## Value

See **Details**.

## Details

Each of these functions returns an object containing data, parameter
draws, or predictions corresponding to a basic linear regression model
with data `y` (outcome vector) and `X` (predictor matrix), and
parameters `alpha` (intercept), `beta` (coefficient vector), and `sigma`
(error sd).

- `example_mcmc_draws()`:

  If `chains > 1`, a `250` (iterations) by `chains` by `params` array
  or, if `chains = 1`, a `250` by `params` matrix of MCMC draws from the
  posterior distribution of the parameters in the linear regression
  model described above. If `params = 1` then only the draws for `alpha`
  are included in the returned object. If `params >= 2` then draws for
  `sigma` are also included. And if `params` is between `3` and the
  maximum of `6` then draws for regression coefficients `beta[k]` (`k`
  in `1:(params-2)`) are also included.

- `example_y_data()`:

  A numeric vector with `434` observations of the outcome variable in
  the linear regression model.

- `example_x_data()`:

  A numeric vector with `434` observations of one of the predictor
  variables in the linear regression model.

- `example_group_data()`:

  A factor variable with `434` observations of a grouping variable with
  two levels.

- `example_yrep_draws()`:

  A `500` (draws) by `434` (data points) matrix of draws from the
  posterior predictive distribution. Each row represents a full dataset
  drawn from the posterior predictive distribution of the outcome `y`
  after fitting the linear regression model mentioned above.

## Examples

``` r
draws <- example_mcmc_draws()
dim(draws)
#> [1] 250   4   4
dimnames(draws)
#> $Iteration
#> NULL
#> 
#> $Chain
#> [1] "chain:1" "chain:2" "chain:3" "chain:4"
#> 
#> $Parameter
#> [1] "alpha"   "sigma"   "beta[1]" "beta[2]"
#> 

draws <- example_mcmc_draws(1, 2)
dim(draws)
#> [1] 250   2
colnames(draws)
#> [1] "alpha" "sigma"

draws <- example_mcmc_draws(params = 6)
dimnames(draws)[[3]]
#> [1] "alpha"   "sigma"   "beta[1]" "beta[2]" "beta[3]" "beta[4]"

y <- example_y_data()
x <- example_x_data()
group <- example_group_data()
length(y)
#> [1] 434
length(x)
#> [1] 434
length(group)
#> [1] 434
tail(data.frame(y, x, group), 5)
#>      y        x  group
#> 430 94 84.87741 GroupA
#> 431 76 92.99039 GroupB
#> 432 50 94.85971 GroupA
#> 433 88 96.85662 GroupB
#> 434 70 91.25334 GroupB

yrep <- example_yrep_draws()
dim(yrep) # ncol(yrep) = length(y) = length(x) = length(group)
#> [1] 500 434
```
