# Get or view the names of available plotting or data functions

Get or view the names of available plotting or data functions

## Usage

``` r
available_ppc(pattern = NULL, fixed = FALSE, invert = FALSE, plots_only = TRUE)

available_ppd(pattern = NULL, fixed = FALSE, invert = FALSE, plots_only = TRUE)

available_mcmc(
  pattern = NULL,
  fixed = FALSE,
  invert = FALSE,
  plots_only = TRUE
)
```

## Arguments

- pattern, fixed, invert:

  Passed to [`base::grep()`](https://rdrr.io/r/base/grep.html).

- plots_only:

  If `TRUE` (the default) only plotting functions are searched for. If
  `FALSE` then functions that return data for plotting (functions ending
  in `_data()`) are also included.

## Value

A possibly empty character vector of function names with several
additional attributes (for use by a custom print method). If `pattern`
is missing then the returned object contains the names of all available
plotting functions in the
[MCMC](https://mc-stan.org/bayesplot/reference/MCMC-overview.md),
[PPC](https://mc-stan.org/bayesplot/reference/PPC-overview.md), or
[PPD](https://mc-stan.org/bayesplot/reference/PPD-overview.md) module,
depending on which function is called. If `pattern` is specified then a
subset of function names is returned.

## Examples

``` r
available_mcmc()
#> bayesplot MCMC module:
#>   mcmc_acf
#>   mcmc_acf_bar
#>   mcmc_areas
#>   mcmc_areas_ridges
#>   mcmc_combo
#>   mcmc_dens
#>   mcmc_dens_chains
#>   mcmc_dens_overlay
#>   mcmc_hex
#>   mcmc_hist
#>   mcmc_hist_by_chain
#>   mcmc_intervals
#>   mcmc_neff
#>   mcmc_neff_hist
#>   mcmc_nuts_acceptance
#>   mcmc_nuts_divergence
#>   mcmc_nuts_energy
#>   mcmc_nuts_stepsize
#>   mcmc_nuts_treedepth
#>   mcmc_pairs
#>   mcmc_parcoord
#>   mcmc_rank_ecdf
#>   mcmc_rank_hist
#>   mcmc_rank_overlay
#>   mcmc_recover_hist
#>   mcmc_recover_intervals
#>   mcmc_recover_scatter
#>   mcmc_rhat
#>   mcmc_rhat_hist
#>   mcmc_scatter
#>   mcmc_trace
#>   mcmc_trace_highlight
#>   mcmc_violin
available_mcmc("nuts")
#> bayesplot MCMC module:
#> (matching pattern 'nuts') 
#>   mcmc_nuts_acceptance
#>   mcmc_nuts_divergence
#>   mcmc_nuts_energy
#>   mcmc_nuts_stepsize
#>   mcmc_nuts_treedepth
available_mcmc("rhat|neff")
#> bayesplot MCMC module:
#> (matching pattern 'rhat|neff') 
#>   mcmc_neff
#>   mcmc_neff_hist
#>   mcmc_rhat
#>   mcmc_rhat_hist

available_ppc()
#> bayesplot PPC module:
#>   ppc_bars
#>   ppc_bars_grouped
#>   ppc_boxplot
#>   ppc_dens
#>   ppc_dens_overlay
#>   ppc_dens_overlay_grouped
#>   ppc_dots
#>   ppc_ecdf_overlay
#>   ppc_ecdf_overlay_grouped
#>   ppc_error_binned
#>   ppc_error_hist
#>   ppc_error_hist_grouped
#>   ppc_error_scatter
#>   ppc_error_scatter_avg
#>   ppc_error_scatter_avg_grouped
#>   ppc_error_scatter_avg_vs_x
#>   ppc_freqpoly
#>   ppc_freqpoly_grouped
#>   ppc_hist
#>   ppc_intervals
#>   ppc_intervals_grouped
#>   ppc_km_overlay
#>   ppc_km_overlay_grouped
#>   ppc_loo_intervals
#>   ppc_loo_pit_ecdf
#>   ppc_loo_pit_overlay
#>   ppc_loo_pit_qq
#>   ppc_loo_ribbon
#>   ppc_pit_ecdf
#>   ppc_pit_ecdf_grouped
#>   ppc_ribbon
#>   ppc_ribbon_grouped
#>   ppc_rootogram
#>   ppc_scatter
#>   ppc_scatter_avg
#>   ppc_scatter_avg_grouped
#>   ppc_stat
#>   ppc_stat_2d
#>   ppc_stat_freqpoly
#>   ppc_stat_freqpoly_grouped
#>   ppc_stat_grouped
#>   ppc_violin_grouped
available_ppc("grouped")
#> bayesplot PPC module:
#> (matching pattern 'grouped') 
#>   ppc_bars_grouped
#>   ppc_dens_overlay_grouped
#>   ppc_ecdf_overlay_grouped
#>   ppc_error_hist_grouped
#>   ppc_error_scatter_avg_grouped
#>   ppc_freqpoly_grouped
#>   ppc_intervals_grouped
#>   ppc_km_overlay_grouped
#>   ppc_pit_ecdf_grouped
#>   ppc_ribbon_grouped
#>   ppc_scatter_avg_grouped
#>   ppc_stat_freqpoly_grouped
#>   ppc_stat_grouped
#>   ppc_violin_grouped
available_ppc("grouped", invert = TRUE)
#> bayesplot PPC module:
#> (excluding pattern 'grouped') 
#>   ppc_bars
#>   ppc_boxplot
#>   ppc_dens
#>   ppc_dens_overlay
#>   ppc_dots
#>   ppc_ecdf_overlay
#>   ppc_error_binned
#>   ppc_error_hist
#>   ppc_error_scatter
#>   ppc_error_scatter_avg
#>   ppc_error_scatter_avg_vs_x
#>   ppc_freqpoly
#>   ppc_hist
#>   ppc_intervals
#>   ppc_km_overlay
#>   ppc_loo_intervals
#>   ppc_loo_pit_ecdf
#>   ppc_loo_pit_overlay
#>   ppc_loo_pit_qq
#>   ppc_loo_ribbon
#>   ppc_pit_ecdf
#>   ppc_ribbon
#>   ppc_rootogram
#>   ppc_scatter
#>   ppc_scatter_avg
#>   ppc_stat
#>   ppc_stat_2d
#>   ppc_stat_freqpoly

available_ppd()
#> bayesplot PPD module:
#>   ppd_boxplot
#>   ppd_dens
#>   ppd_dens_overlay
#>   ppd_dots
#>   ppd_ecdf_overlay
#>   ppd_freqpoly
#>   ppd_freqpoly_grouped
#>   ppd_hist
#>   ppd_intervals
#>   ppd_intervals_grouped
#>   ppd_ribbon
#>   ppd_ribbon_grouped
#>   ppd_stat
#>   ppd_stat_2d
#>   ppd_stat_freqpoly
#>   ppd_stat_freqpoly_grouped
#>   ppd_stat_grouped
available_ppd("grouped")
#> bayesplot PPD module:
#> (matching pattern 'grouped') 
#>   ppd_freqpoly_grouped
#>   ppd_intervals_grouped
#>   ppd_ribbon_grouped
#>   ppd_stat_freqpoly_grouped
#>   ppd_stat_grouped

# can also see which functions that return data are available
available_ppc(plots_only = FALSE)
#> bayesplot PPC module:
#>   ppc_bars
#>   ppc_bars_data
#>   ppc_bars_grouped
#>   ppc_boxplot
#>   ppc_data
#>   ppc_dens
#>   ppc_dens_overlay
#>   ppc_dens_overlay_grouped
#>   ppc_dots
#>   ppc_ecdf_overlay
#>   ppc_ecdf_overlay_grouped
#>   ppc_error_binned
#>   ppc_error_data
#>   ppc_error_hist
#>   ppc_error_hist_grouped
#>   ppc_error_scatter
#>   ppc_error_scatter_avg
#>   ppc_error_scatter_avg_grouped
#>   ppc_error_scatter_avg_vs_x
#>   ppc_freqpoly
#>   ppc_freqpoly_grouped
#>   ppc_hist
#>   ppc_intervals
#>   ppc_intervals_data
#>   ppc_intervals_grouped
#>   ppc_km_overlay
#>   ppc_km_overlay_grouped
#>   ppc_loo_intervals
#>   ppc_loo_pit_data
#>   ppc_loo_pit_ecdf
#>   ppc_loo_pit_overlay
#>   ppc_loo_pit_qq
#>   ppc_loo_ribbon
#>   ppc_pit_ecdf
#>   ppc_pit_ecdf_grouped
#>   ppc_ribbon
#>   ppc_ribbon_data
#>   ppc_ribbon_grouped
#>   ppc_rootogram
#>   ppc_scatter
#>   ppc_scatter_avg
#>   ppc_scatter_avg_data
#>   ppc_scatter_avg_grouped
#>   ppc_scatter_data
#>   ppc_stat
#>   ppc_stat_2d
#>   ppc_stat_data
#>   ppc_stat_freqpoly
#>   ppc_stat_freqpoly_grouped
#>   ppc_stat_grouped
#>   ppc_violin_grouped

# only show the _data functions
available_ppc("_data", plots_only = FALSE)
#> bayesplot PPC module:
#> (matching pattern '_data') 
#>   ppc_bars_data
#>   ppc_data
#>   ppc_error_data
#>   ppc_intervals_data
#>   ppc_loo_pit_data
#>   ppc_ribbon_data
#>   ppc_scatter_avg_data
#>   ppc_scatter_data
#>   ppc_stat_data
available_ppd("_data", plots_only = FALSE)
#> bayesplot PPD module:
#> (matching pattern '_data') 
#>   ppd_data
#>   ppd_intervals_data
#>   ppd_ribbon_data
#>   ppd_stat_data
available_mcmc("_data", plots_only = FALSE)
#> bayesplot MCMC module:
#> (matching pattern '_data') 
#>   mcmc_areas_data
#>   mcmc_areas_ridges_data
#>   mcmc_dens_chains_data
#>   mcmc_intervals_data
#>   mcmc_neff_data
#>   mcmc_parcoord_data
#>   mcmc_rhat_data
#>   mcmc_trace_data
```
