# Plots for Markov chain Monte Carlo simulations

The **bayesplot** MCMC module provides various plotting functions for
creating graphical displays of Markov chain Monte Carlo (MCMC)
simulations. The **MCMC plotting functions** section, below, provides
links to the documentation for various categories of MCMC plots.
Currently the MCMC plotting functions accept posterior draws provided in
one of the following formats:

- **3-D array**: An array with dimensions `Iteration, Chain, Parameter`
  in that order.

- **list**: A list of matrices, where each matrix corresponds to a
  Markov chain. All of the matrices should have the same number of
  iterations (rows) and parameters (columns), and parameters should have
  the same names and be in the same order.

- **matrix (2-D array)**: A matrix with one column per parameter. If
  using matrix there should only be a single Markov chain or all chains
  should already be merged (stacked).

- **data frame**: There are two types of data frames allowed. Either a
  data frame with one column per parameter (if only a single chain or
  all chains have already been merged), or a data frame with one column
  per parameter plus an additional column `"Chain"` that contains the
  chain number (an integer) corresponding to each row in the data frame.

- **draws**: Any of the `draws` formats supported by the posterior
  package.

**Note**: typically the user should *not* include warmup iterations in
the object passed to **bayesplot** plotting functions, although for
certain plots (e.g. trace plots) it can occasionally be useful to
include the warmup iterations for diagnostic purposes.

## MCMC plotting functions

- [Posterior
  distributions](https://mc-stan.org/bayesplot/reference/MCMC-distributions.md):
  Histograms and kernel density plots of parameter draws, optionally
  showing each Markov chain separately.

- [Uncertainty
  intervals](https://mc-stan.org/bayesplot/reference/MCMC-intervals.md):
  Uncertainty intervals computed from parameter draws.

- [Trace plots](https://mc-stan.org/bayesplot/reference/MCMC-traces.md):
  Times series of parameter draws, optionally including HMC/NUTS
  diagnostic information.

- [Scatterplots](https://mc-stan.org/bayesplot/reference/MCMC-scatterplots.md):
  Scatterplots, heatmaps, and pairs plots of parameter draws, optionally
  including HMC/NUTS diagnostic information.

- [Parallel coordinates
  plots](https://mc-stan.org/bayesplot/reference/MCMC-parcoord.md):
  Parallel coordinates plot of MCMC draws (one dimension per parameter),
  optionally including HMC/NUTS diagnostic information.

- [Combos](https://mc-stan.org/bayesplot/reference/MCMC-combos.md):
  Combination plots (e.g. trace plot + histogram).

- [General MCMC
  diagnostics](https://mc-stan.org/bayesplot/reference/MCMC-diagnostics.md):
  MCMC diagnostic plots including R-hat, effective sample size,
  autocorrelation. [NUTS
  diagnostics](https://mc-stan.org/bayesplot/reference/MCMC-nuts.md):
  Special diagnostic plots for the No-U-Turn Sampler.

- [Comparisons to "true"
  values](https://mc-stan.org/bayesplot/reference/MCMC-recover.md):
  Plots comparing MCMC estimates to "true" parameter values (e.g.,
  values used to simulate data).

## References

Gabry, J. , Simpson, D. , Vehtari, A. , Betancourt, M. and Gelman, A.
(2019), Visualization in Bayesian workflow. *J. R. Stat. Soc. A*, 182:
389-402. doi:10.1111/rssa.12378. ([journal
version](https://rss.onlinelibrary.wiley.com/doi/full/10.1111/rssa.12378),
[arXiv preprint](https://arxiv.org/abs/1709.01449), [code on
GitHub](https://github.com/jgabry/bayes-vis-paper))

## See also

Other MCMC:
[`MCMC-combos`](https://mc-stan.org/bayesplot/reference/MCMC-combos.md),
[`MCMC-diagnostics`](https://mc-stan.org/bayesplot/reference/MCMC-diagnostics.md),
[`MCMC-distributions`](https://mc-stan.org/bayesplot/reference/MCMC-distributions.md),
[`MCMC-intervals`](https://mc-stan.org/bayesplot/reference/MCMC-intervals.md),
[`MCMC-nuts`](https://mc-stan.org/bayesplot/reference/MCMC-nuts.md),
[`MCMC-parcoord`](https://mc-stan.org/bayesplot/reference/MCMC-parcoord.md),
[`MCMC-recover`](https://mc-stan.org/bayesplot/reference/MCMC-recover.md),
[`MCMC-scatterplots`](https://mc-stan.org/bayesplot/reference/MCMC-scatterplots.md),
[`MCMC-traces`](https://mc-stan.org/bayesplot/reference/MCMC-traces.md)
