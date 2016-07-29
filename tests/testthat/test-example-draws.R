library(bayesplot)
context("Example draws")

test_that("example_mcmc_draws throws correct errors", {
  expect_error(example_mcmc_draws(chains = 5), "chains <= 4")
  expect_error(example_mcmc_draws(chains = 0), "chains >= 1")
  expect_error(example_mcmc_draws(params = 7), "params <= 6")
  expect_error(example_mcmc_draws(params = 0), "params >= 1")
})
test_that("example_mcmc_draws returns correct structure", {
  expect_identical(dim(example_mcmc_draws()),
                   c(250L, 4L, 4L))
  expect_identical(dim(example_mcmc_draws(chains = 1, params = 6)),
                   c(250L, 6L))
  expect_identical(dim(example_mcmc_draws(params = 1)),
                   c(250L, 4L, 1L))
  expect_identical(dimnames(example_mcmc_draws(4, 6))[[3]],
                   c("alpha", "sigma", paste0("beta[", 1:4,"]")))
})

