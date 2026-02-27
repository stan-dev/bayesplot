# PPD distributions

Plot posterior or prior predictive distributions. Each of these
functions makes the same plot as the corresponding
[`ppc_`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
function but without plotting any observed data `y`. The **Plot
Descriptions** section at
[PPC-distributions](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
has details on the individual plots.

## Usage

``` r
ppd_data(ypred, group = NULL)

ppd_dens_overlay(
  ypred,
  ...,
  size = 0.25,
  alpha = 0.7,
  trim = FALSE,
  bw = "nrd0",
  adjust = 1,
  kernel = "gaussian",
  bounds = NULL,
  n_dens = 1024
)

ppd_ecdf_overlay(
  ypred,
  ...,
  discrete = FALSE,
  pad = TRUE,
  size = 0.25,
  alpha = 0.7
)

ppd_dens(ypred, ..., trim = FALSE, size = 0.5, alpha = 1, bounds = NULL)

ppd_hist(ypred, ..., binwidth = NULL, bins = NULL, breaks = NULL, freq = TRUE)

ppd_dots(ypred, ..., binwidth = NA, quantiles = 100, freq = TRUE)

ppd_freqpoly(
  ypred,
  ...,
  binwidth = NULL,
  bins = NULL,
  freq = TRUE,
  size = 0.5,
  alpha = 1
)

ppd_freqpoly_grouped(
  ypred,
  group,
  ...,
  binwidth = NULL,
  bins = NULL,
  freq = TRUE,
  size = 0.5,
  alpha = 1
)

ppd_boxplot(ypred, ..., notch = TRUE, size = 0.5, alpha = 1)
```

## Arguments

- ypred:

  An `S` by `N` matrix of draws from the posterior (or prior) predictive
  distribution. The number of rows, `S`, is the size of the posterior
  (or prior) sample used to generate `ypred`. The number of columns,
  `N`, is the number of predicted observations.

- group:

  A grouping variable of the same length as `y`. Will be coerced to
  [factor](https://rdrr.io/r/base/factor.html) if not already a factor.
  Each value in `group` is interpreted as the group level pertaining to
  the corresponding observation.

- ...:

  For dot plots, optional additional arguments to pass to
  [`ggdist::stat_dots()`](https://mjskay.github.io/ggdist/reference/stat_dots.html).

- size, alpha:

  Passed to the appropriate geom to control the appearance of the
  predictive distributions.

- trim:

  A logical scalar passed to
  [`ggplot2::geom_density()`](https://ggplot2.tidyverse.org/reference/geom_density.html).

- bw, adjust, kernel, n_dens, bounds:

  Optional arguments passed to
  [`stats::density()`](https://rdrr.io/r/stats/density.html) (and
  `bounds` to
  [`ggplot2::stat_density()`](https://ggplot2.tidyverse.org/reference/geom_density.html))
  to override default kernel density estimation parameters or truncate
  the density support. `n_dens` defaults to `1024`.

- discrete:

  For
  [`ppc_ecdf_overlay()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md),
  should the data be treated as discrete? The default is `FALSE`, in
  which case `geom="line"` is passed to
  [`ggplot2::stat_ecdf()`](https://ggplot2.tidyverse.org/reference/stat_ecdf.html).
  If `discrete` is set to `TRUE` then `geom="step"` is used.

- pad:

  A logical scalar passed to
  [`ggplot2::stat_ecdf()`](https://ggplot2.tidyverse.org/reference/stat_ecdf.html).

- binwidth:

  Passed to
  [`ggplot2::geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html),
  [`ggplot2::geom_area()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html),
  and
  [`ggdist::stat_dots()`](https://mjskay.github.io/ggdist/reference/stat_dots.html)
  to override the default binwidth.

- bins:

  Passed to
  [`ggplot2::geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html)
  and
  [`ggplot2::geom_area()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html)
  to override the default binning.

- breaks:

  Passed to
  [`ggplot2::geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html)
  as an alternative to `binwidth`.

- freq:

  For histograms and frequency polygons, `freq=TRUE` (the default) puts
  count on the y-axis. Setting `freq=FALSE` puts density on the y-axis.
  (For many plots the y-axis text is off by default. To view the count
  or density labels on the y-axis see the
  [`yaxis_text()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  convenience function.)

- quantiles:

  For dot plots, an optional integer passed to
  [`ggdist::stat_dots()`](https://mjskay.github.io/ggdist/reference/stat_dots.html)
  specifying the number of quantiles to use for a quantile dot plot. If
  `quantiles` is `NA` then all data points are plotted. The default is
  `quantiles=100` so that each dot represent one percent of posterior
  mass.

- notch:

  For the box plot, a logical scalar passed to
  [`ggplot2::geom_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.html).
  Note: unlike
  [`geom_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.html),
  the default is `notch=TRUE`.

## Value

The plotting functions return a ggplot object that can be further
customized using the **ggplot2** package. The functions with suffix
`_data()` return the data that would have been drawn by the plotting
function.

## Details

For Binomial data, the plots may be more useful if the input contains
the "success" *proportions* (not discrete "success" or "failure"
counts).

## See also

Other PPDs:
[`PPD-intervals`](https://mc-stan.org/bayesplot/dev/reference/PPD-intervals.md),
[`PPD-overview`](https://mc-stan.org/bayesplot/dev/reference/PPD-overview.md),
[`PPD-test-statistics`](https://mc-stan.org/bayesplot/dev/reference/PPD-test-statistics.md)

## Examples

``` r
# difference between ppd_dens_overlay() and ppc_dens_overlay()
color_scheme_set("brightblue")
preds <- example_yrep_draws()
ppd_dens_overlay(ypred = preds[1:50, ])

ppc_dens_overlay(y = example_y_data(), yrep = preds[1:50, ])

```
