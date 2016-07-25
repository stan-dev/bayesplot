library(bayesplot)
context("MCMC: scatterplots")

source("data-for-mcmc-tests.R")


test_that("mcmc_scatter returns a ggplot object", {
  expect_gg(mcmc_scatter(arr, pars = c("beta[1]", "beta[2]")))
  expect_gg(mcmc_scatter(arr1chain, regex_pars = "beta"))
  expect_gg(mcmc_scatter(mat, pars = c("sigma", "(Intercept)")))
  expect_gg(mcmc_scatter(dframe, regex_pars = "x:[2,4]"))
  expect_gg(mcmc_scatter(dframe_multiple_chains,
                         pars = c("sigma", "(Intercept)")))
})

test_that("mcmc_scatter throws error if only 1 parameter", {
  expect_error(mcmc_scatter(arr, pars = "sigma"), "exactly 2 parameters")
  expect_error(mcmc_scatter(arr1), "exactly 2 parameters")
  expect_error(mcmc_scatter(mat1), "exactly 2 parameters")
  expect_error(mcmc_scatter(dframe1), "exactly 2 parameters")
  expect_error(mcmc_scatter(chainlist1), "exactly 2 parameters")
})

test_that("mcmc_pairs returns a gg object", {
  expect_message(mcmc_pairs(arr, pars = c("(Intercept)", "sigma")),
                 "Processing...")
  expect_ggmatrix(mcmc_pairs(arr, pars = c("(Intercept)", "sigma")))
  expect_ggmatrix(mcmc_pairs(arr, regex_pars = "beta"))
  expect_ggmatrix(mcmc_pairs(arr, regex_pars = "x:[1-3]"))

  expect_ggmatrix(mcmc_pairs(arr1chain, regex_pars = "beta"))
  expect_ggmatrix(mcmc_pairs(mat, pars = c("(Intercept)", "sigma")))
  expect_ggmatrix(mcmc_pairs(dframe, pars = c("(Intercept)", "sigma")))
  expect_ggmatrix(mcmc_pairs(dframe_multiple_chains, regex_pars = "beta"))
})

test_that("mcmc_pairs throws error if only 1 parameter", {
  expect_error(mcmc_pairs(arr, pars = "sigma"), "requires multiple parameters")
  expect_error(mcmc_pairs(arr1), "requires multiple parameters")
  expect_error(mcmc_pairs(mat1), "requires multiple parameters")
  expect_error(mcmc_pairs(dframe1), "requires multiple parameters")
  expect_error(mcmc_pairs(chainlist1), "requires multiple parameters")
})
