<!-- See http://style.tidyverse.org/news.html for advice on writing news -->

# bayesplot 1.10.0

* New function `mcmc_rank_ecdf()` for rank ecdf plots with confidence bands for
assessing if two or more chains sample the same distribution (#282,
@TeemuSailynoja)
* New functions `ppc_pit_ecdf()`, `ppc_pit_ecdf_grouped()`, PIT ecdf plots with
confidence bands to assess if `y` and `yrep` contain samples from the same
distribution. (#282, @TeemuSailynoja)
* Several `ppc` and `ppd` functions now accept the new `linewidth` argument
introduced in ggplot2 3.4.0: `ppc_bars()`, `ppc_bars_grouped()`,
`ppc_intervals()`, `ppc_intervals_grouped()`, `ppd_intervals()`,
`ppd_intervals_grouped()`.
* Fix bug in how `mcmc_pairs()` detected hitting `max_treedepth`, thanks to @dmphillippo. (#281)
* Fix failing tests due to changes in ggplot2 3.4.0 (#289)

# bayesplot 1.9.0

* New module PPD (posterior/prior predictive distribution) with a lot of new
plotting functions with `ppd_` prefix. These functions plot draws from the prior
or posterior predictive distributions (PPD) without comparing to observed data
(i.e., no `y` argument). Because these are not "checks" against the observed
data we use PPD instead of PPC. These plots are essentially the same as the
corresponding PPC plots but without showing any observed data (e.g.,
`ppd_intervals()` is like `ppc_intervals()` but without plotting `y`). See
`help("PPD-overview")` for details. (#151, #222)

* All PPC categories now have one or more `_data()` functions that return the
data frame used for plotting (#97, #222). Many of these have already been in
previous releases, but the new ones in this release are:
   - `ppc_bars_data()`
   - `ppc_error_data()`
   - `ppc_error_binnned_data()`
   - `ppc_scatter_data()`
   - `ppc_scatter_avg_data()`
   - `ppc_stat_data()`

* Many functions gain an argument `facet_args` for controlling ggplot2 faceting
  (many other functions have had this argument for a long time).
  The ones that just now got the argument are:
   - `ppc_scatter()`
   - `ppc_scatter_avg_grouped()`
   - `ppc_error_hist()`
   - `ppc_error_hist_grouped()`
   - `ppc_error_scatter()`
   - `ppc_error_binned()`
   
* New plotting function `ppc_km_overlay_grouped()`, the grouped variant of
  `ppc_km_overlay()`. (#260, @fweber144)

* `ppc_scatter()`, `ppc_scatter_avg()`, and `ppc_scatter_avg_grouped()` gain an
  argument `ref_line`, which can be set to `FALSE` to turn off the `x=y` line
  drawn behind the scatterplot.
  
* `ppc_ribbon()` and `ppc_ribbon_grouped()` gain argument `y_draw` that specifies whether the observed y should be plotted using a point, line, or both. (#257, @charlesm93)

* `mcmc_*()` functions now support all draws formats from the **posterior** package. (#277, @Ozan147)

* `mcmc_dens()` and `mcmc_dens_overlay()` gain arguments for controlling the
  the density calculation. (#258)

* `mcmc_hist()` and `mcmc_dens()` gain argument `alpha` for controlling transparency. (#244)

* `mcmc_areas()` and `mcmc_areas_ridges()` gain an argument `border_size` for
controlling the thickness of the ridgelines. (#224)

# bayesplot 1.8.1

* Fix R cmd check error on linux for CRAN

# bayesplot 1.8.0

### Bug fixes

* `mcmc_areas()` tries to use less vertical blank space. (#218, #230)

* Fix bug in `color_scheme_view()` minimal theme (#213).

* Fix error in `mcmc_acf()` for certain input types. (#244, #245, @hhau)

### New features

* New plotting functions `ppc_dens_overlay_grouped()` and `ppc_ecdf_overlay_grouped()`
  for plotting density and cumulative distributions of the posterior predictive
  distribution (versus observed data) by group. (#212)

* New plotting function `ppc_km_overlay()` for outcome variables that are   
  right-censored. Empirical CCDF estimates of `yrep` are compared with the
  Kaplan-Meier estimate of `y`. (#233, #234, @fweber144)

* `ppc_loo_pit_overlay()` now uses a boundary correction for an improved kernel
  density estimation. The new argument `boundary_correction` defaults to TRUE but
  can be set to FALSE to recover the old version of the plot. (#171, #235,
  @ecoronado92)

* CmdStanMCMC objects (from CmdStanR) can now be used with extractor
  functions `nuts_params()`, `log_posterior()`, `rhat()`, and
  `neff_ratio()`. (#227)

* On the y axis, `ppc_loo_pit_qq(..., compare = "normal")` now plots standard
  normal quantiles calculated from the PIT values (instead of the standardized
  PIT values). (#240, #243, @fweber144)

* `mcmc_rank_overlay()` gains argument `facet_args`. (#221, @hhau)

* For `mcmc_intervals()` the size` of the points and interval lines can be set with
  `mcmc_intervals(..., outer_size, inner_size, point_size)`. (#215, #228, #229)



# bayesplot 1.7.2

Compatibility with dplyr 1.0.0 (#219)

# bayesplot 1.7.1

Release requested by CRAN to fix errors at
https://cran.r-project.org/web/checks/check_results_bayesplot.html due to
matrices also inheriting from "array" in R 4.0.

# bayesplot 1.7.0

(GitHub issue/PR numbers in parentheses)

* The `pars` argument of all MCMC plotting functions now supports tidy variable
  selection. See `help("tidy-params", package="bayesplot")` for details and
  examples. (#161, #183, #188)

* Two new plots have been added for inspecting the distribution of ranks.
  Rank histograms were introduced by the Stan team's [new paper on
  MCMC diagnostics](https://arxiv.org/abs/1903.08008). (#178, #179)

  `mcmc_rank_hist()`: A traditional traceplot (`mcmc_trace()`) visualizes how
  sampled values the MCMC chains mix over the course of sampling. A rank
  histogram (`mcmc_rank_hist()`) visualizes how the *ranks* of values from the
  chains mix together. An ideal plot would show the ranks mixing or overlapping
  in a uniform distribution.

  `mcmc_rank_overlay()`: Instead of drawing each chain's histogram in a separate
  panel, this plot draws the top edge of the chains' histograms in a single
  panel.

* Added `mcmc_trace_data()`, which returns the data used for plotting the trace
  plots and rank histograms. (Advances #97)

* [ColorBrewer](https://colorbrewer2.org/) palettes are now available as color
  schemes via
  [`color_scheme_set()`](https://mc-stan.org/bayesplot/reference/bayesplot-colors.html).
  For example, `color_scheme_set("brewer-Spectral")` will use the Spectral
  palette. (#177, #190)

* MCMC plots now also accept objects with an `as.array` method as
  input (e.g., stanfit objects). (#175, #184)

* [`mcmc_trace()`](https://mc-stan.org/bayesplot/reference/MCMC-traces.html)
  gains an argument `iter1` which can be used to label the traceplot starting
  from the first iteration after warmup. (#14, #155, @mcol)

* [`mcmc_areas()`](https://mc-stan.org/bayesplot/reference/MCMC-intervals.html)
  gains an argument `area_method` which controls how to draw the density
  curves. The default `"equal area"` constrains the heights so that the curves
  have the same area. As a result, a narrow interval will appear as a spike
  of density, while a wide, uncertain interval is spread thin over the _x_ axis.
  Alternatively `"equal height"` will set the maximum height on each curve to
  the same value. This works well when the intervals are about the same width.
  Otherwise, that wide, uncertain interval will dominate the visual space
  compared to a narrow, less uncertain interval. A compromise between the two is
  `"scaled height"` which scales the curves from `"equal height"` using
  `height * sqrt(height)`. (#163, #169)

* `mcmc_areas()` correctly plots density curves where the point estimate
  does not include the highest point of the density curve.
  (#168, #169, @jtimonen)

* `mcmc_areas_ridges()` draws the vertical line at *x* = 0 over the curves so
  that it is always visible.

* `mcmc_intervals()` and `mcmc_areas()` raise a warning if `prob_outer` is ever
  less than `prob`. It sorts these two values into the correct order. (#138)

* MCMC parameter names are now *always* converted to factors prior to
  plotting. We use factors so that the order of parameters in a plot matches
  the order of the parameters in the original MCMC data. This change fixes a
  case where factor-conversion failed. (#162, #165, @wwiecek)

* The examples in
  [`?ppc_loo_pit_overlay()`](https://mc-stan.org/bayesplot/reference/PPC-loo.html)
  now work as expected. (#166, #167)

* Added `"viridisD"` as an alternative name for `"viridis"` to the supported
  colors.

* Added `"viridisE"` (the [cividis](https://github.com/marcosci/cividis)
  version of viridis) to the supported colors.

* `ppc_bars()` and `ppc_bars_grouped()` now allow negative integers as input.
  (#172, @jeffpollock9)


# bayesplot 1.6.0

(GitHub issue/PR numbers in parentheses)

* Loading **bayesplot** no longer overrides the ggplot theme! Rather, it sets
  a theme specific for **bayesplot**. Some packages using **bayesplot** may
  still override the default **ggplot** theme (e.g., **rstanarm** does but
  only until next release), but simply loading **bayesplot** itself will not.
  There are new functions for controlling the ggplot theme for **bayesplot**
  that work like their **ggplot2** counterparts but only affect plots made
  using **bayesplot**. Thanks to Malcolm Barrett. (#117, #149).
    - `bayesplot_theme_set()`
    - `bayesplot_theme_get()`
    - `bayesplot_theme_update()`
    - `bayesplot_theme_replace()`

* The [Visual MCMC Diagnostics
  vignette](https://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html)
  has been reorganized and has a lot of useful new content thanks to Martin
  Modrák. (#144, #153)

* The [LOO predictive
  checks](https://mc-stan.org/bayesplot/reference/PPC-loo.html) now require
  **loo** version `>= 2.0.0`. (#139)

* Histogram plots gain a `breaks` argument that can be used as an alternative
  to `binwidth`. (#148)

* [`mcmc_pairs()`](https://mc-stan.org/bayesplot/reference/MCMC-scatterplots.html)
  now has an argument `grid_args` to provide a way of passing optional
  arguments to `gridExtra::arrangeGrob()`. This can be used to add a title to
  the plot, for example. (#143)

* [`ppc_ecdf_overlay()`](https://mc-stan.org/bayesplot/reference/PPC-distributions.html)
  gains an argument `discrete`, which is `FALSE` by default, but can be used
  to make the Geom more appropriate for discrete data. (#145)

* [PPC intervals
  plots](https://mc-stan.org/bayesplot/reference/PPC-intervals.html) and [LOO
  predictive checks](https://mc-stan.org/bayesplot/reference/PPC-loo.html) now
  draw both an outer and an inner probability interval, which can be
  controlled through the new argument `prob_outer` and the already existing
  `prob`. This is consistent with what is produced by `mcmc_intervals()`.
  (#152, #154, @mcol)




# bayesplot 1.5.0

(GitHub issue/PR numbers in parentheses)

* New package documentation website: <https://mc-stan.org/bayesplot/>

* Two new plots that visualize posterior density using
  [ridgelines][ggridges]. These work well when parameters have similar
  values and similar densities, as in hierarchical models. (#104)
    - `mcmc_dens_chains()` draws the kernel density of each sampling chain.
    - `mcmc_areas_ridges()` draws the kernel density combined across chains.
    - Both functions have a `_data()` function to return the data plotted by
      each function.     

* `mcmc_intervals()` and `mcmc_areas()` have been rewritten. (#103)
    - They now use a discrete *y*-axis. Previously, they used a continuous
      scale with numeric breaks relabelled with parameter names; this design  
      caused some unexpected behavior when customizing these plots.
    - `mcmc_areas()` now uses geoms from the ggridges package to draw density
      curves.

* Added `mcmc_intervals_data()` and `mcmc_areas_data()` that return data
  plotted by `mcmc_intervals()` and `mcmc_areas()`. (Advances #97)

* New `ppc_data()` function returns the data plotted by many of the PPC plotting
  functions. (Advances #97)

* Added `ppc_loo_pit_overlay()` function for a better LOO PIT predictive check.
  (#123)

* Started using **vdiffr** to add visual unit tests to the existing PPC unit
  tests. (#137)




# bayesplot 1.4.0

(GitHub issue/PR numbers in parentheses)

* New plotting function `mcmc_parcoord()` for parallel coordinates plots of
  MCMC draws (optionally including HMC/NUTS diagnostic information). (#108)

* `mcmc_scatter` gains an `np` argument for specifying NUTS parameters, which
  allows highlighting divergences in the plot. (#112)

* New functions with names ending with suffix `_data` don't make the plots,
  they just return the data prepared for plotting (more of these to come in
  future releases):
    - `ppc_intervals_data()` (#101)
    - `ppc_ribbon_data()` (#101)
    - `mcmc_parcoord_data()` (#108)
    - `mcmc_rhat_data()` (#110)
    - `mcmc_neff_data()` (#110)

* `ppc_stat_grouped()`, `ppc_stat_freqpoly_grouped()` gain a `facet_args`
  argument for controlling **ggplot2** faceting (many of the `mcmc_` functions
  already have this).

* The `divergences` argument to `mcmc_trace()` has been deprecated in favor
  of `np` (NUTS parameters) to match the other functions that have an `np`
  argument.

* Fixed an issue where duplicated rhat values would break `mcmc_rhat()` (#105).




# bayesplot 1.3.0

(GitHub issue/PR numbers in parentheses)

* `bayesplot::theme_default()` is now set as the default ggplot2 plotting theme
  when **bayesplot** is loaded, which makes changing the default theme using
  `ggplot2::theme_set()` possible. Thanks to @gavinsimpson. (#87)

* `mcmc_hist()` and `mcmc_hist_by_chain()` now take a `freq` argument that
  defaults to `TRUE` (behavior is like `freq` argument to R's `hist`
  function).

* Using a `ts` object for `y` in PPC plots no longer results in an error.
  Thanks to @helske. (#94)

* `mcmc_intervals()` doesn't use round lineends anymore as they slightly
  exaggerate the width of the intervals. Thanks to @tjmahr. (#96)




# bayesplot 1.2.0

A lot of new stuff in this release. (GitHub issue/PR numbers in parentheses)

## Fixes

* Avoid error in some cases when `divergences` is specified in call to
  `mcmc_trace()` but there are not actually any divergent transitions.

* The `merge_chains` argument to `mcmc_nuts_energy()` now defaults to `FALSE`.

## New features in existing functions

* For `mcmc_*()` functions, transformations are recycled if `transformations`
  argument is specified as a single function rather than a named list. Thanks
  to @tklebel. (#64)

* For `ppc_violin_grouped()` there is now the option of showing `y` as a
  violin, points, or both. Thanks to @silberzwiebel. (#74)

* `color_scheme_get()` now has an optional argument `i` for selecting only a
  subset of the colors.

* New color schemes: darkgray, orange, viridis, viridisA, viridisB, viridisC.
  The viridis schemes are better than the other schemes for trace plots (the
  colors are very distinct from each other).

## New functions

* `mcmc_pairs()`, which is essentially a ggplot2+grid implementation of
  rstan's `pairs.stanfit()` method. (#67)

* `mcmc_hex()`, which is similar to `mcmc_scatter()` but using `geom_hex()`
  instead of `geom_point()`. This can be used to avoid overplotting. (#67)

* `overlay_function()` convenience function. Example usage: add a Gaussian (or
  any distribution) density curve to a plot made with `mcmc_hist()`.

* `mcmc_recover_scatter()` and `mcmc_recover_hist()`, which are similar to
  `mcmc_recover_intervals()` and compare estimates to "true" values used to
  simulate data. (#81, #83)

* New PPC category **Discrete** with functions:
    - `ppc_rootogram()` for use with models for count data. Thanks to
      @paul-buerkner. (#28)
    - `ppc_bars()`, `ppc_bars_grouped()` for use with models for ordinal,
      categorical and multinomial data. Thanks to @silberzwiebel. (#73)

* New PPC category **LOO** (thanks to suggestions from @avehtari) with
  functions:
    - `ppc_loo_pit()` for assessing the calibration of marginal predictions.
      (#72)
    - `ppc_loo_intervals()`, `ppc_loo_ribbon()` for plotting intervals of the
      LOO predictive distribution. (#72)




# bayesplot 1.1.0

(GitHub issue/PR numbers in parentheses)

## Fixes

* Images in vignettes should now render properly using `png` device. Thanks to
  TJ Mahr. (#51)

* `xaxis_title(FALSE)` and `yaxis_title(FALSE)` now set axis titles to `NULL`
  rather than changing theme elements to `element_blank()`. This makes it
  easier to add axis titles to plots that don’t have them by default. Thanks
  to Bill Harris. (#53)

## New features in existing functions

* Add argument `divergences` to `mcmc_trace()` function. For models fit using
  HMC/NUTS this can be used to display divergences as a rug at the bottom of
  the trace plot. (#42)

* The `stat` argument for all `ppc_stat_*()` functions now accepts a function
  instead of only the name of a function. (#31)

## New functions

* `ppc_error_hist_grouped()` for plotting predictive errors by level of a
  grouping variable. (#40)

* `mcmc_recover_intervals)(` for comparing MCMC estimates to "true" parameter
  values used to simulate the data. (#56)

* `bayesplot_grid()` for juxtaposing plots and enforcing shared axis limits.
  (#59)




# bayesplot 1.0.0

Initial CRAN release




[ggridges]: https://CRAN.R-project.org/package=ggridges
            "ggridges package"
