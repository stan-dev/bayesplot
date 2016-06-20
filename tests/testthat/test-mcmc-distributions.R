library(bayesplot)
context("MCMC: distributions")

source("data-for-mcmc-tests.R")

test_that("mcmc_hist returns a ggplot object", {
  expect_gg(mcmc_hist(arr, pars = "beta[1]", regex_pars = "x\\:"))
  expect_gg(mcmc_hist(arr1chain, regex_pars = "beta"))
  expect_gg(mcmc_hist(mat))
  expect_gg(mcmc_hist(dframe))
  expect_gg(mcmc_hist(dframe_multiple_chains))

  expect_gg(mcmc_hist(arr1))
  expect_gg(mcmc_hist(mat1))
  expect_gg(mcmc_hist(dframe1))
})

test_that("mcmc_dens returns a ggplot object", {
  expect_gg(mcmc_dens(arr, pars = "beta[2]", regex_pars = "x\\:"))
  expect_gg(mcmc_dens(arr1chain, regex_pars = "beta"))
  expect_gg(mcmc_dens(mat))
  expect_gg(mcmc_dens(dframe))
  expect_gg(mcmc_dens(dframe_multiple_chains))

  expect_gg(mcmc_dens(arr1))
  expect_gg(mcmc_dens(mat1))
  expect_gg(mcmc_dens(dframe1))
})


# functions that require multiple chains ----------------------------------
test_that("mcmc_hist_by_chain returns a ggplot object", {
  expect_gg(mcmc_hist_by_chain(arr, pars = "beta[1]", regex_pars = "x\\:"))
  expect_gg(mcmc_hist_by_chain(dframe_multiple_chains, regex_pars = c("(Intercept)", "beta")))
})

test_that("mcmc_dens_overlay returns a ggplot object", {
  expect_gg(mcmc_dens_overlay(arr, pars = "beta[1]", regex_pars = "x\\:"))
  expect_gg(mcmc_dens_overlay(dframe_multiple_chains, pars = c("(Intercept)", "beta[2]")))
})

test_that("mcmc_violin returns a ggplot object", {
  expect_gg(mcmc_violin(arr, pars = "beta[2]", regex_pars = "x\\:"))
  expect_gg(mcmc_violin(dframe_multiple_chains, regex_pars = c("\\(Intercept\\)$", "beta")))
})

test_that("mcmc_* throws error if 1 chain but multiple chains required", {
  expect_error(mcmc_hist_by_chain(mat), "requires multiple chains")
  expect_error(mcmc_hist_by_chain(dframe), "requires multiple chains")
  expect_error(mcmc_hist_by_chain(arr1chain), "requires multiple chains")

  expect_error(mcmc_dens_overlay(mat), "requires multiple chains")
  expect_error(mcmc_dens_overlay(dframe), "requires multiple chains")
  expect_error(mcmc_dens_overlay(arr1chain), "requires multiple chains")

  expect_error(mcmc_violin(mat), "requires multiple chains")
  expect_error(mcmc_violin(dframe), "requires multiple chains")
  expect_error(mcmc_violin(arr1chain), "requires multiple chains")
})

