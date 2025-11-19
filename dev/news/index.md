# Changelog

## bayesplot (development version)

- Add `shape` argument to
  [`mcmc_scatter()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-scatterplots.md)
  by [@behramulukir](https://github.com/behramulukir)
  ([\#375](https://github.com/stan-dev/bayesplot/issues/375))
- Restore pre ggplot2 v4.0 behavior of bayesplot theme setting
  ([\#385](https://github.com/stan-dev/bayesplot/issues/385))

## bayesplot 1.14.0

CRAN release: 2025-08-31

- PPC “avg” functions
  ([`ppc_scatter_avg()`](https://mc-stan.org/bayesplot/dev/reference/PPC-scatterplots.md),
  [`ppc_error_scatter_avg()`](https://mc-stan.org/bayesplot/dev/reference/PPC-errors.md),
  etc.) gain a `stat` argument to set the averaging function.
  (Suggestion of
  [\#348](https://github.com/stan-dev/bayesplot/issues/348),
  [@kruschke](https://github.com/kruschke)).
- `ppc_error_scatter_avg_vs_x(x = some_expression)` labels the x axis
  with `some_expression`.
- New quantile dot plot functions
  [`ppc_dots()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  and
  [`ppd_dots()`](https://mc-stan.org/bayesplot/dev/reference/PPD-distributions.md)
  by [@behramulukir](https://github.com/behramulukir)
  ([\#357](https://github.com/stan-dev/bayesplot/issues/357))
- Add `x` argument to
  [`ppc_error_binned()`](https://mc-stan.org/bayesplot/dev/reference/PPC-errors.md)
  to control x axis by [@behramulukir](https://github.com/behramulukir)
  ([\#359](https://github.com/stan-dev/bayesplot/issues/359))
- Add `x` argument to
  [`ppc_error_scatter_avg()`](https://mc-stan.org/bayesplot/dev/reference/PPC-errors.md)
  to control x axis by [@behramulukir](https://github.com/behramulukir)
  ([\#367](https://github.com/stan-dev/bayesplot/issues/367))
- Add `discrete` style to
  [`ppc_rootogram()`](https://mc-stan.org/bayesplot/dev/reference/PPC-discrete.md)
  by [@behramulukir](https://github.com/behramulukir)
  ([\#362](https://github.com/stan-dev/bayesplot/issues/362))
- Add `discrete` argument to
  [`ppc_stat()`](https://mc-stan.org/bayesplot/dev/reference/PPC-test-statistics.md)
  and
  [`ppd_stat()`](https://mc-stan.org/bayesplot/dev/reference/PPD-test-statistics.md)
  to support discrete stats by
  [@behramulukir](https://github.com/behramulukir)
  ([\#369](https://github.com/stan-dev/bayesplot/issues/369))

## bayesplot 1.13.0

CRAN release: 2025-06-18

- Add
  [`ppc_loo_pit_ecdf()`](https://mc-stan.org/bayesplot/dev/reference/PPC-loo.md)
  by [@TeemuSailynoja](https://github.com/TeemuSailynoja)
  ([\#345](https://github.com/stan-dev/bayesplot/issues/345))
- Add possibility for left-truncation to
  [`ppc_km_overlay()`](https://mc-stan.org/bayesplot/dev/reference/PPC-censoring.md)
  and
  [`ppc_km_overlay_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-censoring.md)
  by [@Sakuski](https://github.com/Sakuski)
  ([\#347](https://github.com/stan-dev/bayesplot/issues/347))
- Give user control over extrapolation in
  [`ppc_km_overlay()`](https://mc-stan.org/bayesplot/dev/reference/PPC-censoring.md)
  and
  [`ppc_km_overlay_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-censoring.md)
  by [@Sakuski](https://github.com/Sakuski)
  ([\#353](https://github.com/stan-dev/bayesplot/issues/353))
- Allow passing `moment_match` via `...` for loo functions by
  [@n-kall](https://github.com/n-kall)
  ([\#351](https://github.com/stan-dev/bayesplot/issues/351))
- Skip some tests when missing Suggests by
  [@MichaelChirico](https://github.com/MichaelChirico)
  ([\#344](https://github.com/stan-dev/bayesplot/issues/344))
- Remove a test that will fail with next ggplot2 release
  ([\#356](https://github.com/stan-dev/bayesplot/issues/356))

## bayesplot 1.12.0

CRAN release: 2025-04-10

- Expand checking workflows to more platforms by
  [@andrjohns](https://github.com/andrjohns)
  ([\#324](https://github.com/stan-dev/bayesplot/issues/324))
- Skip tests depending on Suggested dependency rstantools if not
  installed by [@MichaelChirico](https://github.com/MichaelChirico)
  ([\#325](https://github.com/stan-dev/bayesplot/issues/325))
- Skip tests depending on Suggested dependency gridExtra if not
  installed by [@MichaelChirico](https://github.com/MichaelChirico)
  ([\#326](https://github.com/stan-dev/bayesplot/issues/326))
- Fix missing legends for unobserved levels in rhat and neff plots
  ([\#328](https://github.com/stan-dev/bayesplot/issues/328))
- Document problems with `ppc_stat` with `stat="mean"`
  ([\#329](https://github.com/stan-dev/bayesplot/issues/329))
- Ensure rank overlay plot starts at 0 even if not all bins present,
  thanks [@sims1253](https://github.com/sims1253)
  ([\#332](https://github.com/stan-dev/bayesplot/issues/332))
- Compatibility with ggplot2 3.6.0 by
  [@teunbrand](https://github.com/teunbrand)
  ([\#337](https://github.com/stan-dev/bayesplot/issues/337))
- Update GitHub actions workflows
  ([\#338](https://github.com/stan-dev/bayesplot/issues/338))
- Remove deprecated functions from
  [`available_ppc()`](https://mc-stan.org/bayesplot/dev/reference/available_ppc.md)
  ([\#340](https://github.com/stan-dev/bayesplot/issues/340))
- Fix missing counts in
  [`ppc_bars_data()`](https://mc-stan.org/bayesplot/dev/reference/PPC-discrete.md)
  by [@TeemuSailynoja](https://github.com/TeemuSailynoja), thanks
  [@famuvie](https://github.com/famuvie)
  ([\#342](https://github.com/stan-dev/bayesplot/issues/342))

## bayesplot 1.11.1

CRAN release: 2024-02-15

- Limit maximum number of evaluation points in `ppc_pit_ecdf` functions
  by default to 1000. by
  [@TeemuSailynoja](https://github.com/TeemuSailynoja) in
  [\#318](https://github.com/stan-dev/bayesplot/issues/318)

## bayesplot 1.11.0

CRAN release: 2024-01-30

- Update for new ggplot2 release by
  [@teunbrand](https://github.com/teunbrand) in
  [\#309](https://github.com/stan-dev/bayesplot/issues/309)
- Add `bins` argument to many histogram plots by 2 in
  [\#300](https://github.com/stan-dev/bayesplot/issues/300)
- Follow ggplot2 updates on
  [`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)
  and
  [`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  by [@heavywatal](https://github.com/heavywatal) in
  [\#305](https://github.com/stan-dev/bayesplot/issues/305)
- Better `ppc_loo_pit_qq` plots by
  [@avehtari](https://github.com/avehtari) in
  [\#307](https://github.com/stan-dev/bayesplot/issues/307)
- Check `prob` is numeric for intervals plots by
  [@tony-stone](https://github.com/tony-stone) in
  [\#299](https://github.com/stan-dev/bayesplot/issues/299)
- Add `bins` and `breaks` arguments to more histogram and hex plots by
  [@heavywatal](https://github.com/heavywatal) in
  [\#313](https://github.com/stan-dev/bayesplot/issues/313)
- Replace `size` argument with `linewidth` for `geom_line` and
  `geom_ridgeline` by [@heavywatal](https://github.com/heavywatal) in
  [\#314](https://github.com/stan-dev/bayesplot/issues/314)
- All LOO plots now accept `psis_object` argument by
  [@jgabry](https://github.com/jgabry) in
  [\#311](https://github.com/stan-dev/bayesplot/issues/311)
- [`ppc_pit_ecdf()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  and
  [`ppc_pit_ecdf_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  now support discrete variables, and their default method for selecting
  the number of ECDF evaluation points has been updated. by
  [@TeemuSailynoja](https://github.com/TeemuSailynoja) in
  [\#316](https://github.com/stan-dev/bayesplot/issues/316)

## bayesplot 1.10.0

CRAN release: 2022-11-16

- New function
  [`mcmc_rank_ecdf()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-traces.md)
  for rank ecdf plots with confidence bands for assessing if two or more
  chains sample the same distribution
  ([\#282](https://github.com/stan-dev/bayesplot/issues/282),
  [@TeemuSailynoja](https://github.com/TeemuSailynoja))
- New functions
  [`ppc_pit_ecdf()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md),
  [`ppc_pit_ecdf_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md),
  PIT ecdf plots with confidence bands to assess if `y` and `yrep`
  contain samples from the same distribution.
  ([\#282](https://github.com/stan-dev/bayesplot/issues/282),
  [@TeemuSailynoja](https://github.com/TeemuSailynoja))
- Several `ppc` and `ppd` functions now accept the new `linewidth`
  argument introduced in ggplot2 3.4.0:
  [`ppc_bars()`](https://mc-stan.org/bayesplot/dev/reference/PPC-discrete.md),
  [`ppc_bars_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-discrete.md),
  [`ppc_intervals()`](https://mc-stan.org/bayesplot/dev/reference/PPC-intervals.md),
  [`ppc_intervals_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-intervals.md),
  [`ppd_intervals()`](https://mc-stan.org/bayesplot/dev/reference/PPD-intervals.md),
  [`ppd_intervals_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPD-intervals.md).
- Fix bug in how
  [`mcmc_pairs()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-scatterplots.md)
  detected hitting `max_treedepth`, thanks to
  [@dmphillippo](https://github.com/dmphillippo).
  ([\#281](https://github.com/stan-dev/bayesplot/issues/281))
- Fix failing tests due to changes in ggplot2 3.4.0
  ([\#289](https://github.com/stan-dev/bayesplot/issues/289))

## bayesplot 1.9.0

CRAN release: 2022-03-10

- New module PPD (posterior/prior predictive distribution) with a lot of
  new plotting functions with `ppd_` prefix. These functions plot draws
  from the prior or posterior predictive distributions (PPD) without
  comparing to observed data (i.e., no `y` argument). Because these are
  not “checks” against the observed data we use PPD instead of PPC.
  These plots are essentially the same as the corresponding PPC plots
  but without showing any observed data (e.g.,
  [`ppd_intervals()`](https://mc-stan.org/bayesplot/dev/reference/PPD-intervals.md)
  is like
  [`ppc_intervals()`](https://mc-stan.org/bayesplot/dev/reference/PPC-intervals.md)
  but without plotting `y`). See
  [`help("PPD-overview")`](https://mc-stan.org/bayesplot/dev/reference/PPD-overview.md)
  for details.
  ([\#151](https://github.com/stan-dev/bayesplot/issues/151),
  [\#222](https://github.com/stan-dev/bayesplot/issues/222))

- All PPC categories now have one or more `_data()` functions that
  return the data frame used for plotting
  ([\#97](https://github.com/stan-dev/bayesplot/issues/97),
  [\#222](https://github.com/stan-dev/bayesplot/issues/222)). Many of
  these have already been in previous releases, but the new ones in this
  release are:

  - [`ppc_bars_data()`](https://mc-stan.org/bayesplot/dev/reference/PPC-discrete.md)
  - [`ppc_error_data()`](https://mc-stan.org/bayesplot/dev/reference/PPC-errors.md)
  - `ppc_error_binnned_data()`
  - [`ppc_scatter_data()`](https://mc-stan.org/bayesplot/dev/reference/PPC-scatterplots.md)
  - [`ppc_scatter_avg_data()`](https://mc-stan.org/bayesplot/dev/reference/PPC-scatterplots.md)
  - [`ppc_stat_data()`](https://mc-stan.org/bayesplot/dev/reference/PPC-test-statistics.md)

- Many functions gain an argument `facet_args` for controlling ggplot2
  faceting (many other functions have had this argument for a long
  time). The ones that just now got the argument are:

  - [`ppc_scatter()`](https://mc-stan.org/bayesplot/dev/reference/PPC-scatterplots.md)
  - [`ppc_scatter_avg_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-scatterplots.md)
  - [`ppc_error_hist()`](https://mc-stan.org/bayesplot/dev/reference/PPC-errors.md)
  - [`ppc_error_hist_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-errors.md)
  - [`ppc_error_scatter()`](https://mc-stan.org/bayesplot/dev/reference/PPC-errors.md)
  - [`ppc_error_binned()`](https://mc-stan.org/bayesplot/dev/reference/PPC-errors.md)

- New plotting function
  [`ppc_km_overlay_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-censoring.md),
  the grouped variant of
  [`ppc_km_overlay()`](https://mc-stan.org/bayesplot/dev/reference/PPC-censoring.md).
  ([\#260](https://github.com/stan-dev/bayesplot/issues/260),
  [@fweber144](https://github.com/fweber144))

- [`ppc_scatter()`](https://mc-stan.org/bayesplot/dev/reference/PPC-scatterplots.md),
  [`ppc_scatter_avg()`](https://mc-stan.org/bayesplot/dev/reference/PPC-scatterplots.md),
  and
  [`ppc_scatter_avg_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-scatterplots.md)
  gain an argument `ref_line`, which can be set to `FALSE` to turn off
  the `x=y` line drawn behind the scatterplot.

- [`ppc_ribbon()`](https://mc-stan.org/bayesplot/dev/reference/PPC-intervals.md)
  and
  [`ppc_ribbon_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-intervals.md)
  gain argument `y_draw` that specifies whether the observed y should be
  plotted using a point, line, or both.
  ([\#257](https://github.com/stan-dev/bayesplot/issues/257),
  [@charlesm93](https://github.com/charlesm93))

- `mcmc_*()` functions now support all draws formats from the
  **posterior** package.
  ([\#277](https://github.com/stan-dev/bayesplot/issues/277),
  [@Ozan147](https://github.com/Ozan147))

- [`mcmc_dens()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-distributions.md)
  and
  [`mcmc_dens_overlay()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-distributions.md)
  gain arguments for controlling the the density calculation.
  ([\#258](https://github.com/stan-dev/bayesplot/issues/258))

- [`mcmc_hist()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-distributions.md)
  and
  [`mcmc_dens()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-distributions.md)
  gain argument `alpha` for controlling transparency.
  ([\#244](https://github.com/stan-dev/bayesplot/issues/244))

- [`mcmc_areas()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
  and
  [`mcmc_areas_ridges()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
  gain an argument `border_size` for controlling the thickness of the
  ridgelines.
  ([\#224](https://github.com/stan-dev/bayesplot/issues/224))

## bayesplot 1.8.1

CRAN release: 2021-06-14

- Fix R cmd check error on linux for CRAN

## bayesplot 1.8.0

CRAN release: 2021-01-10

#### Bug fixes

- [`mcmc_areas()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
  tries to use less vertical blank space.
  ([\#218](https://github.com/stan-dev/bayesplot/issues/218),
  [\#230](https://github.com/stan-dev/bayesplot/issues/230))

- Fix bug in
  [`color_scheme_view()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-colors.md)
  minimal theme
  ([\#213](https://github.com/stan-dev/bayesplot/issues/213)).

- Fix error in
  [`mcmc_acf()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-diagnostics.md)
  for certain input types.
  ([\#244](https://github.com/stan-dev/bayesplot/issues/244),
  [\#245](https://github.com/stan-dev/bayesplot/issues/245),
  [@hhau](https://github.com/hhau))

#### New features

- New plotting functions
  [`ppc_dens_overlay_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  and
  [`ppc_ecdf_overlay_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  for plotting density and cumulative distributions of the posterior
  predictive distribution (versus observed data) by group.
  ([\#212](https://github.com/stan-dev/bayesplot/issues/212))

- New plotting function
  [`ppc_km_overlay()`](https://mc-stan.org/bayesplot/dev/reference/PPC-censoring.md)
  for outcome variables that are  
  right-censored. Empirical CCDF estimates of `yrep` are compared with
  the Kaplan-Meier estimate of `y`.
  ([\#233](https://github.com/stan-dev/bayesplot/issues/233),
  [\#234](https://github.com/stan-dev/bayesplot/issues/234),
  [@fweber144](https://github.com/fweber144))

- [`ppc_loo_pit_overlay()`](https://mc-stan.org/bayesplot/dev/reference/PPC-loo.md)
  now uses a boundary correction for an improved kernel density
  estimation. The new argument `boundary_correction` defaults to TRUE
  but can be set to FALSE to recover the old version of the plot.
  ([\#171](https://github.com/stan-dev/bayesplot/issues/171),
  [\#235](https://github.com/stan-dev/bayesplot/issues/235),

  1.  

- CmdStanMCMC objects (from CmdStanR) can now be used with extractor
  functions
  [`nuts_params()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-extractors.md),
  [`log_posterior()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-extractors.md),
  [`rhat()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-extractors.md),
  and
  [`neff_ratio()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-extractors.md).
  ([\#227](https://github.com/stan-dev/bayesplot/issues/227))

- On the y axis, `ppc_loo_pit_qq(..., compare = "normal")` now plots
  standard normal quantiles calculated from the PIT values (instead of
  the standardized PIT values).
  ([\#240](https://github.com/stan-dev/bayesplot/issues/240),
  [\#243](https://github.com/stan-dev/bayesplot/issues/243),
  [@fweber144](https://github.com/fweber144))

- [`mcmc_rank_overlay()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-traces.md)
  gains argument `facet_args`.
  ([\#221](https://github.com/stan-dev/bayesplot/issues/221),
  [@hhau](https://github.com/hhau))

- For
  [`mcmc_intervals()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
  the
  size`of the points and interval lines can be set with`mcmc_intervals(…,
  outer_size, inner_size, point_size)\`.
  ([\#215](https://github.com/stan-dev/bayesplot/issues/215),
  [\#228](https://github.com/stan-dev/bayesplot/issues/228),
  [\#229](https://github.com/stan-dev/bayesplot/issues/229))

## bayesplot 1.7.2

CRAN release: 2020-05-28

Compatibility with dplyr 1.0.0
([\#219](https://github.com/stan-dev/bayesplot/issues/219))

## bayesplot 1.7.1

CRAN release: 2019-12-01

Release requested by CRAN to fix errors at
<https://cran.r-project.org/web/checks/check_results_bayesplot.html> due
to matrices also inheriting from “array” in R 4.0.

## bayesplot 1.7.0

CRAN release: 2019-05-23

(GitHub issue/PR numbers in parentheses)

- The `pars` argument of all MCMC plotting functions now supports tidy
  variable selection. See
  [`help("tidy-params", package="bayesplot")`](https://mc-stan.org/bayesplot/dev/reference/tidy-params.md)
  for details and examples.
  ([\#161](https://github.com/stan-dev/bayesplot/issues/161),
  [\#183](https://github.com/stan-dev/bayesplot/issues/183),
  [\#188](https://github.com/stan-dev/bayesplot/issues/188))

- Two new plots have been added for inspecting the distribution of
  ranks. Rank histograms were introduced by the Stan team’s [new paper
  on MCMC diagnostics](https://arxiv.org/abs/1903.08008).
  ([\#178](https://github.com/stan-dev/bayesplot/issues/178),
  [\#179](https://github.com/stan-dev/bayesplot/issues/179))

  [`mcmc_rank_hist()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-traces.md):
  A traditional traceplot
  ([`mcmc_trace()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-traces.md))
  visualizes how sampled values the MCMC chains mix over the course of
  sampling. A rank histogram
  ([`mcmc_rank_hist()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-traces.md))
  visualizes how the *ranks* of values from the chains mix together. An
  ideal plot would show the ranks mixing or overlapping in a uniform
  distribution.

  [`mcmc_rank_overlay()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-traces.md):
  Instead of drawing each chain’s histogram in a separate panel, this
  plot draws the top edge of the chains’ histograms in a single panel.

- Added
  [`mcmc_trace_data()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-traces.md),
  which returns the data used for plotting the trace plots and rank
  histograms. (Advances
  [\#97](https://github.com/stan-dev/bayesplot/issues/97))

- [ColorBrewer](https://colorbrewer2.org/) palettes are now available as
  color schemes via
  [`color_scheme_set()`](https://mc-stan.org/bayesplot/reference/bayesplot-colors.html).
  For example, `color_scheme_set("brewer-Spectral")` will use the
  Spectral palette.
  ([\#177](https://github.com/stan-dev/bayesplot/issues/177),
  [\#190](https://github.com/stan-dev/bayesplot/issues/190))

- MCMC plots now also accept objects with an `as.array` method as input
  (e.g., stanfit objects).
  ([\#175](https://github.com/stan-dev/bayesplot/issues/175),
  [\#184](https://github.com/stan-dev/bayesplot/issues/184))

- [`mcmc_trace()`](https://mc-stan.org/bayesplot/reference/MCMC-traces.html)
  gains an argument `iter1` which can be used to label the traceplot
  starting from the first iteration after warmup.
  ([\#14](https://github.com/stan-dev/bayesplot/issues/14),
  [\#155](https://github.com/stan-dev/bayesplot/issues/155),
  [@mcol](https://github.com/mcol))

- [`mcmc_areas()`](https://mc-stan.org/bayesplot/reference/MCMC-intervals.html)
  gains an argument `area_method` which controls how to draw the density
  curves. The default `"equal area"` constrains the heights so that the
  curves have the same area. As a result, a narrow interval will appear
  as a spike of density, while a wide, uncertain interval is spread thin
  over the *x* axis. Alternatively `"equal height"` will set the maximum
  height on each curve to the same value. This works well when the
  intervals are about the same width. Otherwise, that wide, uncertain
  interval will dominate the visual space compared to a narrow, less
  uncertain interval. A compromise between the two is `"scaled height"`
  which scales the curves from `"equal height"` using
  `height * sqrt(height)`.
  ([\#163](https://github.com/stan-dev/bayesplot/issues/163),
  [\#169](https://github.com/stan-dev/bayesplot/issues/169))

- [`mcmc_areas()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
  correctly plots density curves where the point estimate does not
  include the highest point of the density curve.
  ([\#168](https://github.com/stan-dev/bayesplot/issues/168),
  [\#169](https://github.com/stan-dev/bayesplot/issues/169),
  [@jtimonen](https://github.com/jtimonen))

- [`mcmc_areas_ridges()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
  draws the vertical line at *x* = 0 over the curves so that it is
  always visible.

- [`mcmc_intervals()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
  and
  [`mcmc_areas()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
  raise a warning if `prob_outer` is ever less than `prob`. It sorts
  these two values into the correct order.
  ([\#138](https://github.com/stan-dev/bayesplot/issues/138))

- MCMC parameter names are now *always* converted to factors prior to
  plotting. We use factors so that the order of parameters in a plot
  matches the order of the parameters in the original MCMC data. This
  change fixes a case where factor-conversion failed.
  ([\#162](https://github.com/stan-dev/bayesplot/issues/162),
  [\#165](https://github.com/stan-dev/bayesplot/issues/165),
  [@wwiecek](https://github.com/wwiecek))

- The examples in
  [`?ppc_loo_pit_overlay()`](https://mc-stan.org/bayesplot/reference/PPC-loo.html)
  now work as expected.
  ([\#166](https://github.com/stan-dev/bayesplot/issues/166),
  [\#167](https://github.com/stan-dev/bayesplot/issues/167))

- Added `"viridisD"` as an alternative name for `"viridis"` to the
  supported colors.

- Added `"viridisE"` (the [cividis](https://github.com/marcosci/cividis)
  version of viridis) to the supported colors.

- [`ppc_bars()`](https://mc-stan.org/bayesplot/dev/reference/PPC-discrete.md)
  and
  [`ppc_bars_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-discrete.md)
  now allow negative integers as input.
  ([\#172](https://github.com/stan-dev/bayesplot/issues/172),
  [@jeffpollock9](https://github.com/jeffpollock9))

## bayesplot 1.6.0

CRAN release: 2018-08-02

(GitHub issue/PR numbers in parentheses)

- Loading **bayesplot** no longer overrides the ggplot theme! Rather, it
  sets a theme specific for **bayesplot**. Some packages using
  **bayesplot** may still override the default **ggplot** theme (e.g.,
  **rstanarm** does but only until next release), but simply loading
  **bayesplot** itself will not. There are new functions for controlling
  the ggplot theme for **bayesplot** that work like their **ggplot2**
  counterparts but only affect plots made using **bayesplot**. Thanks to
  Malcolm Barrett.
  ([\#117](https://github.com/stan-dev/bayesplot/issues/117),
  [\#149](https://github.com/stan-dev/bayesplot/issues/149)).

  - [`bayesplot_theme_set()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot_theme_get.md)
  - [`bayesplot_theme_get()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot_theme_get.md)
  - [`bayesplot_theme_update()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot_theme_get.md)
  - [`bayesplot_theme_replace()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot_theme_get.md)

- The [Visual MCMC Diagnostics
  vignette](https://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html)
  has been reorganized and has a lot of useful new content thanks to
  Martin Modrák.
  ([\#144](https://github.com/stan-dev/bayesplot/issues/144),
  [\#153](https://github.com/stan-dev/bayesplot/issues/153))

- The [LOO predictive
  checks](https://mc-stan.org/bayesplot/reference/PPC-loo.html) now
  require **loo** version `>= 2.0.0`.
  ([\#139](https://github.com/stan-dev/bayesplot/issues/139))

- Histogram plots gain a `breaks` argument that can be used as an
  alternative to `binwidth`.
  ([\#148](https://github.com/stan-dev/bayesplot/issues/148))

- [`mcmc_pairs()`](https://mc-stan.org/bayesplot/reference/MCMC-scatterplots.html)
  now has an argument `grid_args` to provide a way of passing optional
  arguments to
  [`gridExtra::arrangeGrob()`](https://rdrr.io/pkg/gridExtra/man/arrangeGrob.html).
  This can be used to add a title to the plot, for example.
  ([\#143](https://github.com/stan-dev/bayesplot/issues/143))

- [`ppc_ecdf_overlay()`](https://mc-stan.org/bayesplot/reference/PPC-distributions.html)
  gains an argument `discrete`, which is `FALSE` by default, but can be
  used to make the Geom more appropriate for discrete data.
  ([\#145](https://github.com/stan-dev/bayesplot/issues/145))

- [PPC intervals
  plots](https://mc-stan.org/bayesplot/reference/PPC-intervals.html) and
  [LOO predictive
  checks](https://mc-stan.org/bayesplot/reference/PPC-loo.html) now draw
  both an outer and an inner probability interval, which can be
  controlled through the new argument `prob_outer` and the already
  existing `prob`. This is consistent with what is produced by
  [`mcmc_intervals()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md).
  ([\#152](https://github.com/stan-dev/bayesplot/issues/152),
  [\#154](https://github.com/stan-dev/bayesplot/issues/154),
  [@mcol](https://github.com/mcol))

## bayesplot 1.5.0

CRAN release: 2018-03-30

(GitHub issue/PR numbers in parentheses)

- New package documentation website: <https://mc-stan.org/bayesplot/>

- Two new plots that visualize posterior density using
  [ridgelines](https://CRAN.R-project.org/package=ggridges "ggridges package").
  These work well when parameters have similar values and similar
  densities, as in hierarchical models.
  ([\#104](https://github.com/stan-dev/bayesplot/issues/104))

  - [`mcmc_dens_chains()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-distributions.md)
    draws the kernel density of each sampling chain.
  - [`mcmc_areas_ridges()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
    draws the kernel density combined across chains.
  - Both functions have a `_data()` function to return the data plotted
    by each function.

- [`mcmc_intervals()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
  and
  [`mcmc_areas()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
  have been rewritten.
  ([\#103](https://github.com/stan-dev/bayesplot/issues/103))

  - They now use a discrete *y*-axis. Previously, they used a continuous
    scale with numeric breaks relabelled with parameter names; this
    design  
    caused some unexpected behavior when customizing these plots.
  - [`mcmc_areas()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
    now uses geoms from the ggridges package to draw density curves.

- Added
  [`mcmc_intervals_data()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
  and
  [`mcmc_areas_data()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
  that return data plotted by
  [`mcmc_intervals()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
  and
  [`mcmc_areas()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md).
  (Advances [\#97](https://github.com/stan-dev/bayesplot/issues/97))

- New
  [`ppc_data()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  function returns the data plotted by many of the PPC plotting
  functions. (Advances
  [\#97](https://github.com/stan-dev/bayesplot/issues/97))

- Added
  [`ppc_loo_pit_overlay()`](https://mc-stan.org/bayesplot/dev/reference/PPC-loo.md)
  function for a better LOO PIT predictive check.
  ([\#123](https://github.com/stan-dev/bayesplot/issues/123))

- Started using **vdiffr** to add visual unit tests to the existing PPC
  unit tests.
  ([\#137](https://github.com/stan-dev/bayesplot/issues/137))

## bayesplot 1.4.0

CRAN release: 2017-09-12

(GitHub issue/PR numbers in parentheses)

- New plotting function
  [`mcmc_parcoord()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-parcoord.md)
  for parallel coordinates plots of MCMC draws (optionally including
  HMC/NUTS diagnostic information).
  ([\#108](https://github.com/stan-dev/bayesplot/issues/108))

- `mcmc_scatter` gains an `np` argument for specifying NUTS parameters,
  which allows highlighting divergences in the plot.
  ([\#112](https://github.com/stan-dev/bayesplot/issues/112))

- New functions with names ending with suffix `_data` don’t make the
  plots, they just return the data prepared for plotting (more of these
  to come in future releases):

  - [`ppc_intervals_data()`](https://mc-stan.org/bayesplot/dev/reference/PPC-intervals.md)
    ([\#101](https://github.com/stan-dev/bayesplot/issues/101))
  - [`ppc_ribbon_data()`](https://mc-stan.org/bayesplot/dev/reference/PPC-intervals.md)
    ([\#101](https://github.com/stan-dev/bayesplot/issues/101))
  - [`mcmc_parcoord_data()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-parcoord.md)
    ([\#108](https://github.com/stan-dev/bayesplot/issues/108))
  - [`mcmc_rhat_data()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-diagnostics.md)
    ([\#110](https://github.com/stan-dev/bayesplot/issues/110))
  - [`mcmc_neff_data()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-diagnostics.md)
    ([\#110](https://github.com/stan-dev/bayesplot/issues/110))

- [`ppc_stat_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-test-statistics.md),
  [`ppc_stat_freqpoly_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-test-statistics.md)
  gain a `facet_args` argument for controlling **ggplot2** faceting
  (many of the `mcmc_` functions already have this).

- The `divergences` argument to
  [`mcmc_trace()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-traces.md)
  has been deprecated in favor of `np` (NUTS parameters) to match the
  other functions that have an `np` argument.

- Fixed an issue where duplicated rhat values would break
  [`mcmc_rhat()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-diagnostics.md)
  ([\#105](https://github.com/stan-dev/bayesplot/issues/105)).

## bayesplot 1.3.0

CRAN release: 2017-08-07

(GitHub issue/PR numbers in parentheses)

- [`bayesplot::theme_default()`](https://mc-stan.org/bayesplot/dev/reference/theme_default.md)
  is now set as the default ggplot2 plotting theme when **bayesplot** is
  loaded, which makes changing the default theme using
  [`ggplot2::theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html)
  possible. Thanks to [@gavinsimpson](https://github.com/gavinsimpson).
  ([\#87](https://github.com/stan-dev/bayesplot/issues/87))

- [`mcmc_hist()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-distributions.md)
  and
  [`mcmc_hist_by_chain()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-distributions.md)
  now take a `freq` argument that defaults to `TRUE` (behavior is like
  `freq` argument to R’s `hist` function).

- Using a `ts` object for `y` in PPC plots no longer results in an
  error. Thanks to [@helske](https://github.com/helske).
  ([\#94](https://github.com/stan-dev/bayesplot/issues/94))

- [`mcmc_intervals()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-intervals.md)
  doesn’t use round lineends anymore as they slightly exaggerate the
  width of the intervals. Thanks to
  [@tjmahr](https://github.com/tjmahr).
  ([\#96](https://github.com/stan-dev/bayesplot/issues/96))

## bayesplot 1.2.0

CRAN release: 2017-04-12

A lot of new stuff in this release. (GitHub issue/PR numbers in
parentheses)

### Fixes

- Avoid error in some cases when `divergences` is specified in call to
  [`mcmc_trace()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-traces.md)
  but there are not actually any divergent transitions.

- The `merge_chains` argument to
  [`mcmc_nuts_energy()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-nuts.md)
  now defaults to `FALSE`.

### New features in existing functions

- For `mcmc_*()` functions, transformations are recycled if
  `transformations` argument is specified as a single function rather
  than a named list. Thanks to [@tklebel](https://github.com/tklebel).
  ([\#64](https://github.com/stan-dev/bayesplot/issues/64))

- For
  [`ppc_violin_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  there is now the option of showing `y` as a violin, points, or both.
  Thanks to [@silberzwiebel](https://github.com/silberzwiebel).
  ([\#74](https://github.com/stan-dev/bayesplot/issues/74))

- [`color_scheme_get()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-colors.md)
  now has an optional argument `i` for selecting only a subset of the
  colors.

- New color schemes: darkgray, orange, viridis, viridisA, viridisB,
  viridisC. The viridis schemes are better than the other schemes for
  trace plots (the colors are very distinct from each other).

### New functions

- [`mcmc_pairs()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-scatterplots.md),
  which is essentially a ggplot2+grid implementation of rstan’s
  `pairs.stanfit()` method.
  ([\#67](https://github.com/stan-dev/bayesplot/issues/67))

- [`mcmc_hex()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-scatterplots.md),
  which is similar to
  [`mcmc_scatter()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-scatterplots.md)
  but using
  [`geom_hex()`](https://ggplot2.tidyverse.org/reference/geom_hex.html)
  instead of
  [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).
  This can be used to avoid overplotting.
  ([\#67](https://github.com/stan-dev/bayesplot/issues/67))

- [`overlay_function()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot-helpers.md)
  convenience function. Example usage: add a Gaussian (or any
  distribution) density curve to a plot made with
  [`mcmc_hist()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-distributions.md).

- [`mcmc_recover_scatter()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-recover.md)
  and
  [`mcmc_recover_hist()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-recover.md),
  which are similar to
  [`mcmc_recover_intervals()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-recover.md)
  and compare estimates to “true” values used to simulate data.
  ([\#81](https://github.com/stan-dev/bayesplot/issues/81),
  [\#83](https://github.com/stan-dev/bayesplot/issues/83))

- New PPC category **Discrete** with functions:

  - [`ppc_rootogram()`](https://mc-stan.org/bayesplot/dev/reference/PPC-discrete.md)
    for use with models for count data. Thanks to
    2.  ([\#28](https://github.com/stan-dev/bayesplot/issues/28))
  - [`ppc_bars()`](https://mc-stan.org/bayesplot/dev/reference/PPC-discrete.md),
    [`ppc_bars_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-discrete.md)
    for use with models for ordinal, categorical and multinomial data.
    Thanks to [@silberzwiebel](https://github.com/silberzwiebel).
    ([\#73](https://github.com/stan-dev/bayesplot/issues/73))

- New PPC category **LOO** (thanks to suggestions from
  [@avehtari](https://github.com/avehtari)) with functions:

  - [`ppc_loo_pit()`](https://mc-stan.org/bayesplot/dev/reference/PPC-loo.md)
    for assessing the calibration of marginal predictions.
    ([\#72](https://github.com/stan-dev/bayesplot/issues/72))
  - [`ppc_loo_intervals()`](https://mc-stan.org/bayesplot/dev/reference/PPC-loo.md),
    [`ppc_loo_ribbon()`](https://mc-stan.org/bayesplot/dev/reference/PPC-loo.md)
    for plotting intervals of the LOO predictive distribution.
    ([\#72](https://github.com/stan-dev/bayesplot/issues/72))

## bayesplot 1.1.0

CRAN release: 2016-12-20

(GitHub issue/PR numbers in parentheses)

### Fixes

- Images in vignettes should now render properly using `png` device.
  Thanks to TJ Mahr.
  ([\#51](https://github.com/stan-dev/bayesplot/issues/51))

- `xaxis_title(FALSE)` and `yaxis_title(FALSE)` now set axis titles to
  `NULL` rather than changing theme elements to
  [`element_blank()`](https://ggplot2.tidyverse.org/reference/element.html).
  This makes it easier to add axis titles to plots that don’t have them
  by default. Thanks to Bill Harris.
  ([\#53](https://github.com/stan-dev/bayesplot/issues/53))

### New features in existing functions

- Add argument `divergences` to
  [`mcmc_trace()`](https://mc-stan.org/bayesplot/dev/reference/MCMC-traces.md)
  function. For models fit using HMC/NUTS this can be used to display
  divergences as a rug at the bottom of the trace plot.
  ([\#42](https://github.com/stan-dev/bayesplot/issues/42))

- The `stat` argument for all `ppc_stat_*()` functions now accepts a
  function instead of only the name of a function.
  ([\#31](https://github.com/stan-dev/bayesplot/issues/31))

### New functions

- [`ppc_error_hist_grouped()`](https://mc-stan.org/bayesplot/dev/reference/PPC-errors.md)
  for plotting predictive errors by level of a grouping variable.
  ([\#40](https://github.com/stan-dev/bayesplot/issues/40))

- `mcmc_recover_intervals)(` for comparing MCMC estimates to “true”
  parameter values used to simulate the data.
  ([\#56](https://github.com/stan-dev/bayesplot/issues/56))

- [`bayesplot_grid()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot_grid.md)
  for juxtaposing plots and enforcing shared axis limits.
  ([\#59](https://github.com/stan-dev/bayesplot/issues/59))

## bayesplot 1.0.0

CRAN release: 2016-11-18

Initial CRAN release
