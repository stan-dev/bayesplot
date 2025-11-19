# Plots of posterior or prior predictive distributions

The **bayesplot** PPD module provides various plotting functions for
creating graphical displays of simulated data from the posterior or
prior predictive distribution. These plots are essentially the same as
the corresponding
[PPC](https://mc-stan.org/bayesplot/dev/reference/PPC-overview.md) plots
but without showing any observed data. Because these are not "checks"
compared to data we use PPD (for prior/posterior predictive
distribution) instead of PPC (for prior/posterior predictive check).

## PPD plotting functions

The functions for plotting prior and posterior predictive distributions
without observed data each have the prefix `ppd_` and all have a
required argument `ypred` (a matrix of predictions). The plots are
organized into several categories, each with its own documentation:

- [PPD-distributions](https://mc-stan.org/bayesplot/dev/reference/PPD-distributions.md):
  Histograms, kernel density estimates, boxplots, and other plots of
  multiple simulated datasets (rows) in `ypred`. These are the same as
  the plots in
  [PPC-distributions](https://mc-stan.org/bayesplot/dev/reference/PPC-distributions.md)
  but without including any comparison to `y`.

- [PPD-intervals](https://mc-stan.org/bayesplot/dev/reference/PPD-intervals.md):
  Interval estimates for each predicted observations (columns) in
  `ypred`. The x-axis variable can be optionally specified by the user
  (e.g. to plot against against a predictor variable or over time).These
  are the same as the plots in
  [PPC-intervals](https://mc-stan.org/bayesplot/dev/reference/PPC-intervals.md)
  but without including any comparison to `y`.

- [PPD-test-statistics](https://mc-stan.org/bayesplot/dev/reference/PPD-test-statistics.md):
  The distribution of a statistic, or a pair of statistics, over the
  simulated datasets (rows) in `ypred`. These are the same as the plots
  in
  [PPC-test-statistics](https://mc-stan.org/bayesplot/dev/reference/PPC-test-statistics.md)
  but without including any comparison to `y`.

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
[`PPD-test-statistics`](https://mc-stan.org/bayesplot/dev/reference/PPD-test-statistics.md)
