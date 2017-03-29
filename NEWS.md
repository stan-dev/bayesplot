# bayesplot 1.1.0.9000

(GitHub issue/PR numbers in parentheses)

A lot of new stuff in this release:

#### Fixes
* Avoid error in some cases when `divergences` is specified in call to
`mcmc_trace` but there are not actually any divergent transitions.
* The `merge_chains` argument to `mcmc_nuts_energy` now defaults to `FALSE`.

#### New features in existing functions
* For `mcmc_*` functions, transformations are recycled if `transformations` 
argument is specified as a single function rather than a named list. Thanks to @tklebel. (#64)
* For `ppc_violin_grouped` there is now the option of showing `y` as a violin, 
points, or both. Thanks to @silberzwiebel. (#74)
* `color_scheme_get` now has an optional argument `i` for selecting only a 
subset of the colors.
* New color schemes: darkgray, orange, viridis, viridisA, viridisB, viridisC. 
The viridis schemes are better than the other schemes for trace plots (the 
colors are very distinct from each other).

#### New functions
* `mcmc_pairs`, which is essentially a ggplot2+grid implementation of rstan's
`pairs.stanfit` method. (#67)
* `mcmc_hex`, which is similar to `mcmc_scatter` but using `geom_hex` instead of
`geom_point`. This can be used to avoid overplotting. (#67)
* `overlay_function` convenience function. Example usage: add a Gaussian (or any
distribution) density curve to a plot made with `mcmc_hist`.
* `mcmc_recover_scatter`, which is similar to `mcmc_recover_intervals` but plots
estimated vs "true" values as a scatterplot. (#81)
* New PPC category __Discrete__ with functions: 
    - `ppc_rootogram` for use with models for count data. Thanks to @paul-buerkner. (#28)
    - `ppc_bars`, `ppc_bars_grouped` for use with models for ordinal, categorical 
 and multinomial data. Thanks to @silberzwiebel. (#73)
* New PPC category __LOO__ (thanks to suggestions from @avehtari) with functions: 
    - `ppc_loo_pit` for assessing the calibration of marginal predictions. (#72)
    - `ppc_loo_intervals`, `ppc_loo_ribbon` for plotting intervals of the LOO predictive distribution. (#72)
<br>

# bayesplot 1.1.0

(GitHub issue/PR numbers in parentheses)

#### Fixes
* Images in vignettes should now render properly using `png` device. Thanks to
TJ Mahr. (#51)
* `xaxis_title(FALSE)` and `yaxis_title(FALSE)` now set axis titles to `NULL` 
rather than changing theme elements to `element_blank()`. This makes it easier
to add axis titles to plots that don’t have them by default. Thanks to Bill
Harris. (#53)

#### New features in existing functions
* Add argument `divergences` to `mcmc_trace` function. For models fit using 
HMC/NUTS this can be used to display divergences as a rug at the bottom of the 
trace plot. (#42)
* The `stat` argument for all `ppc_stat_*` functions now accepts a function
instead of only the name of a function. (#31)

#### New functions
* `ppc_error_hist_grouped` for plotting predictive errors
by level of a grouping variable. (#40)
* `mcmc_recover_intervals` for comparing MCMC estimates to "true"
parameter values used to simulate the data. (#56)
* `bayesplot_grid` for juxtaposing plots and enforcing shared
axis limits. (#59)


# bayesplot 1.0.0

* Initial CRAN release
