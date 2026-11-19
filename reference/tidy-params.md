# Tidy parameter selection

Parameter selection in the style of **dplyr** and other tidyverse
packages.

## Usage

``` r
param_range(prefix, range, vars = NULL)

param_glue(pattern, ..., vars = NULL)
```

## Arguments

- prefix, range:

  For `param_range()` only, `prefix` is a string naming a parameter and
  `range` is an integer vector providing the indices of a subset of
  elements to select. For example, using

        param_range("beta", c(1,2,8))

  would select parameters named `beta[1]`, `beta[2]`, and `beta[8]`.
  `param_range()` is only designed for the case that the indices are
  integers surrounded by brackets. If there are no brackets use
  [num_range()](https://tidyselect.r-lib.org/reference/language.html).

- vars:

  `NULL` or a character vector of parameter names to choose from. This
  is only needed for the atypical use case of calling the function as a
  standalone function outside of
  [`vars()`](https://dplyr.tidyverse.org/reference/vars.html),
  [`select()`](https://dplyr.tidyverse.org/reference/select.html), etc.
  Typically this is left as `NULL` and will be set automatically for the
  user.

- pattern, ...:

  For `param_glue()` only, `pattern` is a string containing expressions
  enclosed in braces and `...` should be named arguments providing one
  character vector per expression in braces in `pattern`. It is easiest
  to describe how to use these arguments with an example:

      param_glue("beta_{var}[{level}]",
                 var = c("age", "income"),
                 level = c(3,8))

  would select parameters with names `"beta_age[3]"`,
  `"beta_income[3]"`, `"beta_age[8]"`, `"beta_income[8]"`.

## Details

As of version `1.7.0`, **bayesplot** allows the `pars` argument for
[MCMC plots](https://mc-stan.org/bayesplot/reference/MCMC-overview.md)
to use "tidy" variable selection (in the style of the **dplyr**
package). The
[`vars()`](https://dplyr.tidyverse.org/reference/vars.html) function is
re-exported from **dplyr** for this purpose.

Features of tidy selection includes direct selection
(`vars(alpha, sigma)`), everything-but selection (`vars(-alpha)`),
ranged selection (`` vars(`beta[1]`:`beta[3]`) ``), support for
selection functions (`vars(starts_with("beta"))`), and combinations of
these features. See the **Examples** section, below.

When using `pars` for tidy parameter selection, the `regex_pars`
argument is ignored because **bayesplot** supports using [tidyselect
helper functions](https://tidyselect.r-lib.org/reference/language.html)
([`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
[`contains()`](https://tidyselect.r-lib.org/reference/starts_with.html),
[`num_range()`](https://tidyselect.r-lib.org/reference/starts_with.html),
etc.) for the same purpose. **bayesplot** also exports some additional
helper functions to help with parameter selection:

- `param_range()`: like
  [`num_range()`](https://tidyselect.r-lib.org/reference/starts_with.html)
  but used when parameter indexes are in brackets (e.g. `beta[2]`).

- `param_glue()`: for more complicated parameter names with multiple
  indexes (including variable names) inside the brackets (e.g.,
  `beta[(Intercept) age_group:3]`).

These functions can be used inside of
[`vars()`](https://dplyr.tidyverse.org/reference/vars.html),
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html),
and similar functions, just like the [tidyselect helper
functions](https://tidyselect.r-lib.org/reference/language.html).

## Extra Advice

Parameter names in
[`vars()`](https://dplyr.tidyverse.org/reference/vars.html) are not
quoted. When the names contain special characters like brackets, they
should be wrapped in backticks, as in `` vars(`beta[1]`) ``.

To exclude a range of variables, wrap the sequence in parentheses and
then negate it. For example, (`` vars(-(`beta[1]`:`beta[3]`)) ``) would
exclude `beta[1]`, `beta[2]`, and `beta[3]`.

[`vars()`](https://dplyr.tidyverse.org/reference/vars.html) is a helper
function. It holds onto the names and expressions used to select
columns. When selecting variables inside a **bayesplot** function, use
`vars(...)`: `mcmc_hist(data, pars = vars(alpha))`. When using
[`select()`](https://dplyr.tidyverse.org/reference/select.html) to
prepare a dataframe for a **bayesplot** function, do not use
[`vars()`](https://dplyr.tidyverse.org/reference/vars.html):
`data %>% select(alpha) %>% mcmc_hist()`.

Internally, tidy selection works by converting names and expressions
into position numbers. As a result, integers will select parameters;
`vars(1, 3)` selects the first and third ones. We do not endorse this
approach because positions might change as variables are added and
removed from models. To select a parameter that happens to be called
`1`, use backticks to escape it `` vars(`1`) ``.

## See also

[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)

## Examples

``` r
x <- example_mcmc_draws(params = 6)
dimnames(x)
#> $Iteration
#> NULL
#> 
#> $Chain
#> [1] "chain:1" "chain:2" "chain:3" "chain:4"
#> 
#> $Parameter
#> [1] "alpha"   "sigma"   "beta[1]" "beta[2]" "beta[3]" "beta[4]"
#> 
mcmc_hex(x, pars = vars(alpha, `beta[2]`))

mcmc_dens(x, pars = vars(sigma, contains("beta")))

mcmc_hist(x, pars = vars(-contains("beta")))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.


# using the param_range() helper
mcmc_hist(x, pars = vars(param_range("beta", c(1, 3, 4))))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.


# \donttest{
#############################
## Examples using rstanarm ##
#############################
if (requireNamespace("rstanarm", quietly = TRUE)) {
  # see ?rstanarm::example_model
  fit <- example("example_model", package = "rstanarm", local=TRUE)$value
  print(fit)
  posterior <- as.data.frame(fit)
  str(posterior)

  color_scheme_set("brightblue")
  mcmc_hist(posterior, pars = vars(size, contains("period")))

  # same as previous but using dplyr::select() and piping
  library("dplyr")
  posterior %>%
    select(size, contains("period")) %>%
    mcmc_hist()

  mcmc_intervals(posterior, pars = vars(contains("herd")))
  mcmc_intervals(posterior, pars = vars(contains("herd"), -contains("Sigma")))

  bayesplot_theme_set(ggplot2::theme_dark())
  color_scheme_set("viridisC")
  mcmc_areas_ridges(posterior, pars = vars(starts_with("b[")))

  bayesplot_theme_set()
  color_scheme_set("purple")
  not_789 <- vars(starts_with("b["), -matches("[7-9]"))
  mcmc_intervals(posterior, pars = not_789)

  # using the param_glue() helper
  just_149 <- vars(param_glue("b[(Intercept) herd:{level}]", level = c(1,4,9)))
  mcmc_intervals(posterior, pars = just_149)

  # same but using param_glue() with dplyr::select()
  # before passing to bayesplot
  posterior %>%
    select(param_glue("b[(Intercept) herd:{level}]",
                      level = c(1, 4, 9))) %>%
    mcmc_intervals()
}
#> 
#> exmpl_> if (.Platform$OS.type != "windows" || .Platform$r_arch != "i386") {
#> exmpl_+ example_model <- 
#> exmpl_+   stan_glmer(cbind(incidence, size - incidence) ~ size + period + (1|herd),
#> exmpl_+              data = lme4::cbpp, family = binomial, QR = TRUE,
#> exmpl_+              # this next line is only to keep the example small in size!
#> exmpl_+              chains = 2, cores = 1, seed = 12345, iter = 1000, refresh = 0)
#> exmpl_+ example_model
#> exmpl_+ }
#> stan_glmer
#>  family:       binomial [logit]
#>  formula:      cbind(incidence, size - incidence) ~ size + period + (1 | herd)
#>  observations: 56
#> ------
#>             Median MAD_SD
#> (Intercept) -1.5    0.6  
#> size         0.0    0.0  
#> period2     -1.0    0.3  
#> period3     -1.1    0.3  
#> period4     -1.6    0.4  
#> 
#> Error terms:
#>  Groups Name        Std.Dev.
#>  herd   (Intercept) 0.79    
#> Num. levels: herd 15 
#> 
#> ------
#> * For help interpreting the printed output see ?print.stanreg
#> * For info on the priors used see ?prior_summary.stanreg
#> stan_glmer
#>  family:       binomial [logit]
#>  formula:      cbind(incidence, size - incidence) ~ size + period + (1 | herd)
#>  observations: 56
#> ------
#>             Median MAD_SD
#> (Intercept) -1.5    0.6  
#> size         0.0    0.0  
#> period2     -1.0    0.3  
#> period3     -1.1    0.3  
#> period4     -1.6    0.4  
#> 
#> Error terms:
#>  Groups Name        Std.Dev.
#>  herd   (Intercept) 0.79    
#> Num. levels: herd 15 
#> 
#> ------
#> * For help interpreting the printed output see ?print.stanreg
#> * For info on the priors used see ?prior_summary.stanreg
#> 'data.frame':    1000 obs. of  21 variables:
#>  $ (Intercept)                        : num  -1.854 -1.748 -0.984 -0.771 -0.385 ...
#>  $ size                               : num  0.0189 0.0255 -0.0283 -0.03 -0.0609 ...
#>  $ period2                            : num  -1.243 -0.836 -1.028 -0.911 -0.506 ...
#>  $ period3                            : num  -1 -1.53 -1.82 -1.34 -1.17 ...
#>  $ period4                            : num  -1.23 -1.23 -1.58 -1.92 -1.69 ...
#>  $ b[(Intercept) herd:1]              : num  0.616 1.238 0.306 0.199 0.161 ...
#>  $ b[(Intercept) herd:2]              : num  -0.173 -0.269 -0.19 -0.158 -0.715 ...
#>  $ b[(Intercept) herd:3]              : num  0.542 0.041 1.007 0.731 0.281 ...
#>  $ b[(Intercept) herd:4]              : num  0.543 -0.228 -0.711 0.108 -0.709 ...
#>  $ b[(Intercept) herd:5]              : num  -0.59 -0.278 0.109 0.233 -0.215 ...
#>  $ b[(Intercept) herd:6]              : num  -0.5762 -0.2229 0.147 0.0758 -1.372 ...
#>  $ b[(Intercept) herd:7]              : num  1.397 0.878 0.688 0.826 0.403 ...
#>  $ b[(Intercept) herd:8]              : num  0.612 0.044 1.504 1.043 1.47 ...
#>  $ b[(Intercept) herd:9]              : num  -0.929 0.242 -0.44 -0.902 -0.699 ...
#>  $ b[(Intercept) herd:10]             : num  0.288 -1.226 -0.72 -0.985 0.254 ...
#>  $ b[(Intercept) herd:11]             : num  -0.8942 0.0629 -0.428 -0.577 0.5222 ...
#>  $ b[(Intercept) herd:12]             : num  -0.3775 0.2202 0.0753 -0.9419 -0.2842 ...
#>  $ b[(Intercept) herd:13]             : num  -1.669 -0.599 -1.063 -0.936 0.191 ...
#>  $ b[(Intercept) herd:14]             : num  1.9643 0.0846 1.1837 0.9874 1.1949 ...
#>  $ b[(Intercept) herd:15]             : num  -0.3047 -0.5399 -0.6052 -0.0674 -1.7796 ...
#>  $ Sigma[herd:(Intercept),(Intercept)]: num  0.873 0.258 0.787 0.641 0.626 ...
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

# }
# \dontrun{
###################################
## More examples of param_glue() ##
###################################
library(dplyr)
posterior <- tibble(
  b_Intercept = rnorm(1000),
  sd_condition__Intercept = rexp(1000),
  sigma = rexp(1000),
  `r_condition[A,Intercept]` = rnorm(1000),
  `r_condition[B,Intercept]` = rnorm(1000),
  `r_condition[C,Intercept]` = rnorm(1000),
  `r_condition[A,Slope]` = rnorm(1000),
  `r_condition[B,Slope]` = rnorm(1000)
)
posterior
#> # A tibble: 1,000 × 8
#>    b_Intercept sd_condition__Intercept  sigma `r_condition[A,Intercept]`
#>          <dbl>                   <dbl>  <dbl>                      <dbl>
#>  1      0.554                   0.306  0.402                       0.117
#>  2      0.220                   1.61   0.275                       0.991
#>  3      0.371                   0.0561 0.535                       1.04 
#>  4      0.680                   0.0179 1.45                       -0.103
#>  5      0.970                   3.09   0.879                       0.623
#>  6     -1.24                    0.523  0.0443                      1.57 
#>  7     -0.287                   0.837  0.325                       1.20 
#>  8      0.671                   0.0947 0.734                      -0.816
#>  9     -2.46                    0.691  0.629                       0.431
#> 10     -0.0841                  2.89   0.420                       0.645
#> # ℹ 990 more rows
#> # ℹ 4 more variables: `r_condition[B,Intercept]` <dbl>,
#> #   `r_condition[C,Intercept]` <dbl>, `r_condition[A,Slope]` <dbl>,
#> #   `r_condition[B,Slope]` <dbl>

# using one expression in braces
posterior %>%
  select(
    param_glue("r_condition[{level},Intercept]", level = c("A", "B"))
  ) %>%
  mcmc_hist()
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.


# using multiple expressions in braces
posterior %>%
   select(
     param_glue(
       "r_condition[{level},{type}]",
        level = c("A", "B"),
        type = c("Intercept", "Slope"))
   ) %>%
   mcmc_hist()
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

# }
```
