library(bayesplot)
context("MCMC: traces")

source("data-for-mcmc-tests.R")

test_that("mcmc_trace returns a ggplot object", {
  expect_gg(mcmc_trace(arr, pars = "beta[1]", regex_pars = "x\\:"))
  expect_gg(mcmc_trace(arr1chain, pars = "beta[2]", regex_pars = "x\\:"))
  expect_gg(mcmc_trace(mat))
  expect_gg(mcmc_trace(dframe))
  expect_gg(mcmc_trace(dframe_multiple_chains))
  expect_gg(mcmc_trace(chainlist))

  expect_gg(mcmc_trace(arr1))
  expect_gg(mcmc_trace(mat1))
  expect_gg(mcmc_trace(dframe1))
  expect_gg(mcmc_trace(chainlist1))
})

# functions that require multiple chains ----------------------------------
test_that("mcmc_trace_highlight returns a ggplot object", {
  expect_gg(mcmc_trace_highlight(arr, regex_pars = c("beta", "x\\:")))
  expect_gg(mcmc_trace_highlight(dframe_multiple_chains, highlight = 2))
})

test_that("mcmc_trace_highlight throws error if 1 chain but multiple chains required", {
  expect_error(mcmc_trace_highlight(mat), "requires multiple")
  expect_error(mcmc_trace_highlight(dframe, highlight = 1), "requires multiple chains")
  expect_error(mcmc_trace_highlight(arr1chain, highlight = 1), "requires multiple chains")
})

# options -----------------------------------------------------------------
test_that("mcmc_trace options work", {
  expect_gg(g1 <- mcmc_trace(arr, regex_pars = "beta", window = c(5, 10)))
  coord <- g1$coordinates
  expect_equal(g1$coordinates$limits$x, c(5, 10))

  expect_gg(g2 <- mcmc_trace(arr, regex_pars = "beta", n_warmup = 10))
  ll <- g2$labels
  expect_true(all(c("xmin", "xmax", "ymin", "ymax") %in% names(ll)))
})
