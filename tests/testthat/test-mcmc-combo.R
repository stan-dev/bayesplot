library(bayesplot)
context("MCMC: combo")

source("data-for-mcmc-tests.R")

test_that("mcmc_combo returns a gtable object", {
  expect_gtable(mcmc_combo(arr, regex_pars = "beta", plot = FALSE))
  expect_gtable(mcmc_combo(mat, regex_pars = "beta", plot = FALSE,
                           binwidth = 1/20, combo = c("dens", "hist")))

  expect_gtable(mcmc_combo(dframe, regex_pars = "Intercept", plot = FALSE))
  expect_gtable(mcmc_combo(dframe_multiple_chains, regex_pars = "Intercept",
                           combo = c("trace_highlight", "dens_overlay"),
                           plot = FALSE))
  expect_gtable(mcmc_combo(arr1chain, regex_pars = "Intercept", plot = FALSE,
                           combo = c("trace", "hist")))

  expect_gtable(mcmc_combo(arr1, pars = "(Intercept)", plot = FALSE))
  expect_gtable(mcmc_combo(mat1, plot = FALSE))
  expect_gtable(mcmc_combo(dframe1, plot = FALSE))
})

# functions that require multiple chains ----------------------------------
test_that("mcmc_combo throws error if 1 chain but multiple chains required", {
  expect_error(mcmc_combo(arr1chain, regex_pars = "beta", plot = FALSE,
               combo = c("trace_highlight", "dens")),
               "requires multiple chains")
  expect_error(mcmc_combo(mat, regex_pars = "beta", plot = FALSE,
               combo = c("trace_highlight", "hist")),
               "requires multiple chains")
  expect_error(mcmc_combo(dframe, regex_pars = "beta", plot = FALSE,
               combo = c("dens_overlay", "trace")),
               "requires multiple chains")
})

