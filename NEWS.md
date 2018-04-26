# bayesplot 1.5.0.9000

(GitHub issue/PR numbers in parentheses)

* The [LOO predictive
checks](http://mc-stan.org/bayesplot/reference/PPC-loo.html) now require 
**loo** version `>= 2.0.0`. (#139)

* `mcmc_pairs()` now has an argument `grid_args` to provide a way of passing
optional arguments to `gridExtra::arrangeGrob()`. This can be used to add a
title to the plot, for example. (#143)


# bayesplot 1.5.0

(GitHub issue/PR numbers in parentheses)

* New package documentation website: <http://mc-stan.org/bayesplot/>

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

* Started using __vdiffr__ to add visual unit tests to the existing PPC unit tests. (#137)


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
