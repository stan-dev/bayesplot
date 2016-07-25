library(bayesplot)
context("MCMC: diagnostics")

test_that("rhat and neff plots return a ggplot object", {
  rhat <- runif(100, 1, 1.5)
  expect_gg(mcmc_rhat(rhat))
  expect_gg(mcmc_rhat_hist(rhat))

  ratio <- runif(100, 0, 1)
  expect_gg(mcmc_neff(ratio))
  expect_gg(mcmc_neff_hist(ratio))

  # 1-D array ok
  expect_gg(mcmc_rhat(array(rhat)))
  expect_gg(mcmc_rhat_hist(array(rhat)))
  expect_gg(mcmc_neff(array(ratio)))
  expect_gg(mcmc_neff_hist(array(ratio)))
})

test_that("rhat and neff plot functions throw correct errors & warnings", {
  # need vector or 1D array
  expect_error(mcmc_rhat_hist(cbind(1:2)), "vector_or_1Darray")
  expect_error(mcmc_neff_hist(list(1,2)), "vector_or_1Darray")

  # need positive rhat values
  expect_error(mcmc_rhat(c(-1, 1, 1)), "must be positive")

  # need ratios between 0 and 1
  expect_error(mcmc_neff(c(-1, 0.5, 0.7)), "must be between 0 and 1")

  # drop NAs and warn
  expect_warning(mcmc_rhat(c(1, 1, NA)), "Dropped 1 NAs")
  expect_warning(mcmc_neff(c(0.2, NA, 1, NA)), "Dropped 2 NAs")
})

