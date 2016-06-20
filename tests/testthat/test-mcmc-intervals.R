library(bayesplot)
context("MCMC: intervals")

source("data-for-mcmc-tests.R")

test_that("mcmc_intervals returns a ggplot object", {
  expect_gg(mcmc_intervals(arr, pars = "beta[1]", regex_pars = "x\\:"))
  expect_gg(mcmc_intervals(arr1chain, pars = "beta[1]", regex_pars = "Intercept"))
  expect_gg(mcmc_intervals(mat, regex_pars = "beta"))
  expect_gg(mcmc_intervals(dframe))
  expect_gg(mcmc_intervals(dframe_multiple_chains))

  expect_gg(mcmc_intervals(arr1))
  expect_gg(mcmc_intervals(mat1))
  expect_gg(mcmc_intervals(dframe1))
})

test_that("mcmc_areas returns a ggplot object", {
  expect_gg(mcmc_areas(arr, pars = "beta[2]", regex_pars = "x\\:"))
  expect_gg(mcmc_areas(arr1chain, regex_pars = c("beta", "x\\:")))
  expect_gg(mcmc_areas(mat))
  expect_gg(mcmc_areas(dframe))
  expect_gg(mcmc_areas(dframe_multiple_chains))

  expect_gg(mcmc_areas(arr1))
  expect_gg(mcmc_areas(mat1))
  expect_gg(mcmc_areas(dframe1))
})


