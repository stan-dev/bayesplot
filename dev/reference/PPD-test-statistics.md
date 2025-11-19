# PPD test statistics

The distribution of a (test) statistic `T(ypred)`, or a pair of (test)
statistics, over the simulations from the posterior or prior predictive
distribution. Each of these functions makes the same plot as the
corresponding
[`ppc_`](https://mc-stan.org/bayesplot/dev/reference/PPC-test-statistics.md)
function but without comparing to any observed data `y`. The **Plot
Descriptions** section at
[PPC-test-statistics](https://mc-stan.org/bayesplot/dev/reference/PPC-test-statistics.md)
has details on the individual plots.

## Usage

``` r
ppd_stat(
  ypred,
  stat = "mean",
  ...,
  discrete = FALSE,
  binwidth = NULL,
  bins = NULL,
  breaks = NULL,
  freq = TRUE
)

ppd_stat_grouped(
  ypred,
  group,
  stat = "mean",
  ...,
  discrete = FALSE,
  facet_args = list(),
  binwidth = NULL,
  bins = NULL,
  breaks = NULL,
  freq = TRUE
)

ppd_stat_freqpoly(
  ypred,
  stat = "mean",
  ...,
  facet_args = list(),
  binwidth = NULL,
  bins = NULL,
  freq = TRUE
)

ppd_stat_freqpoly_grouped(
  ypred,
  group,
  stat = "mean",
  ...,
  facet_args = list(),
  binwidth = NULL,
  bins = NULL,
  freq = TRUE
)

ppd_stat_2d(ypred, stat = c("mean", "sd"), ..., size = 2.5, alpha = 0.7)

ppd_stat_data(ypred, group = NULL, stat)
```

## Arguments

- ypred:

  An `S` by `N` matrix of draws from the posterior (or prior) predictive
  distribution. The number of rows, `S`, is the size of the posterior
  (or prior) sample used to generate `ypred`. The number of columns,
  `N`, is the number of predicted observations.

- stat:

  A single function or a string naming a function, except for the 2D
  plot which requires a vector of exactly two names or functions. In all
  cases the function(s) should take a vector input and return a scalar
  statistic. If specified as a string (or strings) then the legend will
  display the function name(s). If specified as a function (or
  functions) then generic naming is used in the legend.

- ...:

  Currently unused.

- discrete:

  For
  [`ppc_stat()`](https://mc-stan.org/bayesplot/dev/reference/PPC-test-statistics.md)
  and
  [`ppc_stat_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-test-statistics.md),
  if `TRUE` then a bar chart is used instead of a histogram.

- binwidth:

  Passed to
  [`ggplot2::geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html)
  to override the default binwidth.

- bins:

  Passed to
  [`ggplot2::geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html)
  to override the default binwidth.

- breaks:

  Passed to
  [`ggplot2::geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html)
  as an alternative to `binwidth`.

- freq:

  For histograms, `freq=TRUE` (the default) puts count on the y-axis.
  Setting `freq=FALSE` puts density on the y-axis. (For many plots the
  y-axis text is off by default. To view the count or density labels on
  the y-axis see the
  [`yaxis_text()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  convenience function.)

- group:

  A grouping variable of the same length as `y`. Will be coerced to
  [factor](https://rdrr.io/r/base/factor.html) if not already a factor.
  Each value in `group` is interpreted as the group level pertaining to
  the corresponding observation.

- facet_args:

  A named list of arguments (other than `facets`) passed to
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  or
  [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)
  to control faceting. Note: if `scales` is not included in `facet_args`
  then **bayesplot** may use `scales="free"` as the default (depending
  on the plot) instead of the **ggplot2** default of `scales="fixed"`.

- size, alpha:

  For the 2D plot only, arguments passed to
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
  to control the appearance of scatterplot points.

## Value

The plotting functions return a ggplot object that can be further
customized using the **ggplot2** package. The functions with suffix
`_data()` return the data that would have been drawn by the plotting
function.

## Details

For Binomial data, the plots may be more useful if the input contains
the "success" *proportions* (not discrete "success" or "failure"
counts).

## References

Gabry, J. , Simpson, D. , Vehtari, A. , Betancourt, M. and Gelman, A.
(2019), Visualization in Bayesian workflow. *J. R. Stat. Soc. A*, 182:
389-402. doi:10.1111/rssa.12378. ([journal
version](https://rss.onlinelibrary.wiley.com/doi/full/10.1111/rssa.12378),
[arXiv preprint](https://arxiv.org/abs/1709.01449), [code on
GitHub](https://github.com/jgabry/bayes-vis-paper))

## See also

Other PPDs:
[`PPD-distributions`](https://mc-stan.org/bayesplot/dev/reference/PPD-distributions.md),
[`PPD-intervals`](https://mc-stan.org/bayesplot/dev/reference/PPD-intervals.md),
[`PPD-overview`](https://mc-stan.org/bayesplot/dev/reference/PPD-overview.md)

## Examples

``` r
yrep <- example_yrep_draws()
ppd_stat(yrep)
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

ppd_stat(yrep, stat = "sd") + legend_none()
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.


# use your own function for the 'stat' argument
color_scheme_set("brightblue")
q25 <- function(y) quantile(y, 0.25)
ppd_stat(yrep, stat = "q25") # legend includes function name
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
```
