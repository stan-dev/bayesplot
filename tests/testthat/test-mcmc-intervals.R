library(bayesplot)
context("MCMC: intervals")

source(test_path("data-for-mcmc-tests.R"))

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

test_that("mcmc_intervals/areas with rhat", {
  r <- runif(ncol(mat), 0.9, 1.3)
  rbad <- c(NA, r[-1])

  expect_error(mcmc_intervals(arr, rhat = r[-1]), "'rhat' has length")
  expect_error(expect_warning(mcmc_intervals(arr, rhat = rbad)))

  expect_gg(g <- mcmc_intervals(arr, rhat = r))
  rhat_map <- g$layers[[3]][["mapping"]]
  expect_identical(rhat_map$colour, as.name("rhat"))

  expect_gg(g2 <- mcmc_areas(arr, rhat = r))
  rhat_map2 <- g2$layers[[2]][["mapping"]]
  expect_identical(rhat_map2$fill, as.name("rhat"))
  for (j in 3:length(g2$layers)) {
    rhat_map2 <- g2$layers[[j]][["mapping"]]
    expect_identical(rhat_map2$colour, as.name("rhat"),
                     info = paste("layer: ", j))
  }
})

