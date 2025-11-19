# Package index

## Overview

Package overview

- [`bayesplot-package`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-package.md)
  [`bayesplot`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-package.md)
  :

  **bayesplot**: Plotting for Bayesian Models

## Aesthetics

Functions for setting the color scheme and ggplot theme used by
**bayesplot**. (Also see the separate **ggplot helpers** section below.)

- [`color_scheme_set()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-colors.md)
  [`color_scheme_get()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-colors.md)
  [`color_scheme_view()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-colors.md)
  :

  Set, get, or view **bayesplot** color schemes

- [`bayesplot_theme_get()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot_theme_get.md)
  [`bayesplot_theme_set()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot_theme_get.md)
  [`bayesplot_theme_update()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot_theme_get.md)
  [`bayesplot_theme_replace()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot_theme_get.md)
  :

  Get, set, and modify the active **bayesplot** theme

- [`theme_default()`](https://mc-stan.org/bayesplot/dev/reference/theme_default.md)
  :

  Default **bayesplot** plotting theme

## PPC

Functions for carrying out a wide variety of graphical model checks
based on comparing observed data to draws from the posterior or prior
predictive distribution.

- [`PPC-overview`](https://mc-stan.org/bayesplot/dev/reference/PPC-overview.md)
  [`PPC`](https://mc-stan.org/bayesplot/dev/reference/PPC-overview.md) :
  Graphical posterior predictive checking
- [`ppc_km_overlay()`](https://mc-stan.org/bayesplot/dev/reference/PPC-censoring.md)
  [`ppc_km_overlay_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-censoring.md)
  : PPC censoring
- [`ppc_bars()`](https://mc-stan.org/bayesplot/dev/reference/PPC-discrete.md)
  [`ppc_bars_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-discrete.md)
  [`ppc_rootogram()`](https://mc-stan.org/bayesplot/dev/reference/PPC-discrete.md)
  [`ppc_bars_data()`](https://mc-stan.org/bayesplot/dev/reference/PPC-discrete.md)
  : PPCs for discrete outcomes
- [`ppc_data()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  [`ppc_dens_overlay()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  [`ppc_dens_overlay_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  [`ppc_ecdf_overlay()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  [`ppc_ecdf_overlay_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  [`ppc_dens()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  [`ppc_hist()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  [`ppc_freqpoly()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  [`ppc_freqpoly_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  [`ppc_boxplot()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  [`ppc_dots()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  [`ppc_violin_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  [`ppc_pit_ecdf()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  [`ppc_pit_ecdf_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  : PPC distributions
- [`ppc_error_hist()`](https://mc-stan.org/bayesplot/dev/reference/PPC-errors.md)
  [`ppc_error_hist_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-errors.md)
  [`ppc_error_scatter()`](https://mc-stan.org/bayesplot/dev/reference/PPC-errors.md)
  [`ppc_error_scatter_avg()`](https://mc-stan.org/bayesplot/dev/reference/PPC-errors.md)
  [`ppc_error_scatter_avg_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-errors.md)
  [`ppc_error_scatter_avg_vs_x()`](https://mc-stan.org/bayesplot/dev/reference/PPC-errors.md)
  [`ppc_error_binned()`](https://mc-stan.org/bayesplot/dev/reference/PPC-errors.md)
  [`ppc_error_data()`](https://mc-stan.org/bayesplot/dev/reference/PPC-errors.md)
  : PPC errors
- [`ppc_intervals()`](https://mc-stan.org/bayesplot/dev/reference/PPC-intervals.md)
  [`ppc_intervals_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-intervals.md)
  [`ppc_ribbon()`](https://mc-stan.org/bayesplot/dev/reference/PPC-intervals.md)
  [`ppc_ribbon_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-intervals.md)
  [`ppc_intervals_data()`](https://mc-stan.org/bayesplot/dev/reference/PPC-intervals.md)
  [`ppc_ribbon_data()`](https://mc-stan.org/bayesplot/dev/reference/PPC-intervals.md)
  : PPC intervals
- [`ppc_loo_pit_overlay()`](https://mc-stan.org/bayesplot/dev/reference/PPC-loo.md)
  [`ppc_loo_pit_data()`](https://mc-stan.org/bayesplot/dev/reference/PPC-loo.md)
  [`ppc_loo_pit_qq()`](https://mc-stan.org/bayesplot/dev/reference/PPC-loo.md)
  [`ppc_loo_pit_ecdf()`](https://mc-stan.org/bayesplot/dev/reference/PPC-loo.md)
  [`ppc_loo_pit()`](https://mc-stan.org/bayesplot/dev/reference/PPC-loo.md)
  [`ppc_loo_intervals()`](https://mc-stan.org/bayesplot/dev/reference/PPC-loo.md)
  [`ppc_loo_ribbon()`](https://mc-stan.org/bayesplot/dev/reference/PPC-loo.md)
  : LOO predictive checks
- [`ppc_scatter()`](https://mc-stan.org/bayesplot/dev/reference/PPC-scatterplots.md)
  [`ppc_scatter_avg()`](https://mc-stan.org/bayesplot/dev/reference/PPC-scatterplots.md)
  [`ppc_scatter_avg_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-scatterplots.md)
  [`ppc_scatter_data()`](https://mc-stan.org/bayesplot/dev/reference/PPC-scatterplots.md)
  [`ppc_scatter_avg_data()`](https://mc-stan.org/bayesplot/dev/reference/PPC-scatterplots.md)
  : PPC scatterplots
- [`ppc_stat()`](https://mc-stan.org/bayesplot/dev/reference/PPC-test-statistics.md)
  [`ppc_stat_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-test-statistics.md)
  [`ppc_stat_freqpoly()`](https://mc-stan.org/bayesplot/dev/reference/PPC-test-statistics.md)
  [`ppc_stat_freqpoly_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-test-statistics.md)
  [`ppc_stat_2d()`](https://mc-stan.org/bayesplot/dev/reference/PPC-test-statistics.md)
  [`ppc_stat_data()`](https://mc-stan.org/bayesplot/dev/reference/PPC-test-statistics.md)
  : PPC test statistics
- [`pp_check()`](https://mc-stan.org/bayesplot/dev/reference/pp_check.md)
  : Posterior (or prior) predictive checks (S3 generic and default
  method)

## PPD

Functions for creating graphical displays of simulated data from the
posterior or prior predictive distribution (PPD). These plots are
essentially the same as the corresponding PPC plots but without
comparing to any observed data.

- [`PPD-overview`](https://mc-stan.org/bayesplot/dev/reference/PPD-overview.md)
  [`PPD`](https://mc-stan.org/bayesplot/dev/reference/PPD-overview.md) :
  Plots of posterior or prior predictive distributions
- [`ppd_data()`](https://mc-stan.org/bayesplot/dev/reference/PPD-distributions.md)
  [`ppd_dens_overlay()`](https://mc-stan.org/bayesplot/dev/reference/PPD-distributions.md)
  [`ppd_ecdf_overlay()`](https://mc-stan.org/bayesplot/dev/reference/PPD-distributions.md)
  [`ppd_dens()`](https://mc-stan.org/bayesplot/dev/reference/PPD-distributions.md)
  [`ppd_hist()`](https://mc-stan.org/bayesplot/dev/reference/PPD-distributions.md)
  [`ppd_dots()`](https://mc-stan.org/bayesplot/dev/reference/PPD-distributions.md)
  [`ppd_freqpoly()`](https://mc-stan.org/bayesplot/dev/reference/PPD-distributions.md)
  [`ppd_freqpoly_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPD-distributions.md)
  [`ppd_boxplot()`](https://mc-stan.org/bayesplot/dev/reference/PPD-distributions.md)
  : PPD distributions
- [`ppd_intervals()`](https://mc-stan.org/bayesplot/dev/reference/PPD-intervals.md)
  [`ppd_intervals_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPD-intervals.md)
  [`ppd_ribbon()`](https://mc-stan.org/bayesplot/dev/reference/PPD-intervals.md)
  [`ppd_ribbon_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPD-intervals.md)
  [`ppd_intervals_data()`](https://mc-stan.org/bayesplot/dev/reference/PPD-intervals.md)
  [`ppd_ribbon_data()`](https://mc-stan.org/bayesplot/dev/reference/PPD-intervals.md)
  : PPD intervals
- [`ppd_stat()`](https://mc-stan.org/bayesplot/dev/reference/PPD-test-statistics.md)
  [`ppd_stat_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPD-test-statistics.md)
  [`ppd_stat_freqpoly()`](https://mc-stan.org/bayesplot/dev/reference/PPD-test-statistics.md)
  [`ppd_stat_freqpoly_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPD-test-statistics.md)
  [`ppd_stat_2d()`](https://mc-stan.org/bayesplot/dev/reference/PPD-test-statistics.md)
  [`ppd_stat_data()`](https://mc-stan.org/bayesplot/dev/reference/PPD-test-statistics.md)
  : PPD test statistics

## MCMC

Functions for creating plots of MCMC draws of model parameters and
general MCMC diagnostics.

- [`MCMC-overview`](https://mc-stan.org/bayesplot/dev/reference/MCMC-overview.md)
  [`MCMC`](https://mc-stan.org/bayesplot/dev/reference/MCMC-overview.md)
  : Plots for Markov chain Monte Carlo simulations
- [`mcmc_rhat()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-diagnostics.md)
  [`mcmc_rhat_hist()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-diagnostics.md)
  [`mcmc_rhat_data()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-diagnostics.md)
  [`mcmc_neff()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-diagnostics.md)
  [`mcmc_neff_hist()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-diagnostics.md)
  [`mcmc_neff_data()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-diagnostics.md)
  [`mcmc_acf()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-diagnostics.md)
  [`mcmc_acf_bar()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-diagnostics.md)
  : General MCMC diagnostics
- [`mcmc_hist()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-distributions.md)
  [`mcmc_dens()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-distributions.md)
  [`mcmc_hist_by_chain()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-distributions.md)
  [`mcmc_dens_overlay()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-distributions.md)
  [`mcmc_dens_chains()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-distributions.md)
  [`mcmc_dens_chains_data()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-distributions.md)
  [`mcmc_violin()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-distributions.md)
  : Histograms and kernel density plots of MCMC draws
- [`mcmc_intervals()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
  [`mcmc_areas()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
  [`mcmc_areas_ridges()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
  [`mcmc_intervals_data()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
  [`mcmc_areas_data()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
  [`mcmc_areas_ridges_data()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
  : Plot interval estimates from MCMC draws
- [`mcmc_recover_intervals()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-recover.md)
  [`mcmc_recover_scatter()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-recover.md)
  [`mcmc_recover_hist()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-recover.md)
  : Compare MCMC estimates to "true" parameter values
- [`mcmc_scatter()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-scatterplots.md)
  [`mcmc_hex()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-scatterplots.md)
  [`mcmc_pairs()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-scatterplots.md)
  [`scatter_style_np()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-scatterplots.md)
  [`pairs_style_np()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-scatterplots.md)
  [`pairs_condition()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-scatterplots.md)
  : Scatterplots of MCMC draws
- [`mcmc_parcoord()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-parcoord.md)
  [`mcmc_parcoord_data()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-parcoord.md)
  [`parcoord_style_np()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-parcoord.md)
  : Parallel coordinates plot of MCMC draws
- [`mcmc_trace()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-traces.md)
  [`mcmc_trace_highlight()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-traces.md)
  [`trace_style_np()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-traces.md)
  [`mcmc_rank_overlay()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-traces.md)
  [`mcmc_rank_hist()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-traces.md)
  [`mcmc_rank_ecdf()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-traces.md)
  [`mcmc_trace_data()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-traces.md)
  : Trace and rank plots of MCMC draws
- [`mcmc_combo()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-combos.md)
  : Combination plots

## HMC/NUTS diagnostics

Functions for plotting diagnostics specific to Hamiltonian Monte Carlo
(HMC) and the No-U-Turn Sampler (NUTS). Some of the general MCMC
plotting functions
([`mcmc_parcoord()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-parcoord.md),
[`mcmc_pairs()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-scatterplots.md),
[`mcmc_scatter()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-scatterplots.md),
[`mcmc_trace()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-traces.md))
can also show HMC/NUTS diagnostic information if optional arguments are
specified, but the special functions below are *only* intended for use
with HMC/NUTS.

- [`mcmc_nuts_acceptance()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-nuts.md)
  [`mcmc_nuts_divergence()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-nuts.md)
  [`mcmc_nuts_stepsize()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-nuts.md)
  [`mcmc_nuts_treedepth()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-nuts.md)
  [`mcmc_nuts_energy()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-nuts.md)
  : Diagnostic plots for the No-U-Turn-Sampler (NUTS)

## Tidy parameter selection for MCMC plots

Helper functions for tidy parameter selection and examples of using
**bayesplot** with [**dplyr**](https://dplyr.tidyverse.org/).

- [`param_range()`](https://mc-stan.org/bayesplot/dev/reference/tidy-params.md)
  [`param_glue()`](https://mc-stan.org/bayesplot/dev/reference/tidy-params.md)
  : Tidy parameter selection

## ggplot helpers

Convenience functions for arranging multiple plots, adding features to
plots, and shortcuts for modifying individual ggplot theme elements.

- [`bayesplot_grid()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot_grid.md)
  : Arrange plots in a grid
- [`vline_at()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  [`hline_at()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  [`vline_0()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  [`hline_0()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  [`abline_01()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  [`lbub()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  [`legend_move()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  [`legend_none()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  [`legend_text()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  [`xaxis_title()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  [`xaxis_text()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  [`xaxis_ticks()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  [`yaxis_title()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  [`yaxis_text()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  [`yaxis_ticks()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  [`facet_text()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  [`facet_bg()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  [`panel_bg()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  [`plot_bg()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  [`grid_lines()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  [`overlay_function()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  : Convenience functions for adding or changing plot details

## Extractors

Functions extracting various quantities needed for plotting from
different types of fitted model objects.

- [`log_posterior()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-extractors.md)
  [`nuts_params()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-extractors.md)
  [`rhat()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-extractors.md)
  [`neff_ratio()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-extractors.md)
  : Extract quantities needed for plotting from model objects

## Miscellaneous

Functions for generating data for examples and listing available
plotting functions.

- [`example_mcmc_draws()`](https://mc-stan.org/bayesplot/dev/reference/example-data.md)
  [`example_yrep_draws()`](https://mc-stan.org/bayesplot/dev/reference/example-data.md)
  [`example_y_data()`](https://mc-stan.org/bayesplot/dev/reference/example-data.md)
  [`example_x_data()`](https://mc-stan.org/bayesplot/dev/reference/example-data.md)
  [`example_group_data()`](https://mc-stan.org/bayesplot/dev/reference/example-data.md)
  : Example draws to use in demonstrations and tests
- [`available_ppc()`](https://mc-stan.org/bayesplot/dev/reference/available_ppc.md)
  [`available_ppd()`](https://mc-stan.org/bayesplot/dev/reference/available_ppc.md)
  [`available_mcmc()`](https://mc-stan.org/bayesplot/dev/reference/available_ppc.md)
  : Get or view the names of available plotting or data functions
