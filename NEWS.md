# bayesplot 1.1.0.9000

(GitHub issue/PR numbers in parentheses)

#### Fixes
* Avoid error in some cases when `divergences` specified in call to `mcmc_trace`
but there are not actually any divergent transitions.

#### New Features
* Introduce `ppc_rootogram` for use with models for count data. (#28)


# bayesplot 1.1.0

(GitHub issue/PR numbers in parentheses)

#### Fixes
* Images in vignettes should now render properly using `png` device. Thanks to
TJ Mahr. (#51)
* `xaxis_title(FALSE)` and `yaxis_title(FALSE)` now set axis titles to `NULL` 
rather than changing theme elements to `element_blank()`. This makes it easier
to add axis titles to plots that don’t have them by default. Thanks to Bill
Harris. (#53)

#### New Features
* Introduce `ppc_error_hist_grouped` for plotting predictive errors
by level of a grouping variable. (#40)
* Introduce `mcmc_recover_intervals` for comparing MCMC estimates to "true"
parameter values used to simulate the data. (#56)
* Add argument `divergences` to `mcmc_trace` function. For models fit using 
HMC/NUTS this can be used to display divergences as a rug at the bottom of the 
trace plot. (#42)
* Introduce `bayesplot_grid` function for juxtaposing plots and enforcing shared
axis limits. (#59)
* The `stat` argument for all `ppc_stat_*` functions now accepts a function
instead of only the name of a function. (#31)


# bayesplot 1.0.0

* Initial CRAN release
