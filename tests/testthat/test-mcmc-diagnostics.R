library(bayesplot)
context("MCMC: diagnostics")

source(test_path("data-for-mcmc-tests.R"))

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

  # named ok
  rhat <- setNames(runif(5, 1, 1.5), paste0("alpha[", 1:5, "]"))
  expect_gg(mcmc_rhat(rhat))
})

test_that("rhat and neff plot functions throw correct errors & warnings", {
  # need vector or 1D array
  expect_error(mcmc_rhat_hist(cbind(1:2)), "is.array")
  expect_error(mcmc_neff_hist(list(1,2)), "is.numeric")

  # need positive rhat values
  expect_error(mcmc_rhat(c(-1, 1, 1)), "must be positive")

  # need ratios between 0 and 1
  expect_error(mcmc_neff(c(-1, 0.5, 0.7)), "must be between 0 and 1")

  # drop NAs and warn
  expect_warning(mcmc_rhat(c(1, 1, NA)), "Dropped 1 NAs")
  expect_warning(mcmc_neff(c(0.2, NA, 1, NA)), "Dropped 2 NAs")
})


test_that("duplicated rhats and neffs are kept (#105)", {
  # https://github.com/stan-dev/bayesplot/issues/105
  rhats <- runif(3, 1, 1.2)
  rhats <- c(rhats, rhats, rhats)
  df <- mcmc_rhat_data(rhats)
  expect_equal(nrow(df), length(rhats))

  ratios <- runif(3, 0, 1)
  ratios <- c(ratios, ratios, ratios)
  df <- mcmc_neff_data(ratios)
  expect_equal(nrow(df), length(ratios))
})

test_that("mcmc_acf & mcmc_acf_bar return a ggplot object", {
  expect_gg(mcmc_acf(arr, pars = "beta[1]", regex_pars = "x\\:[2,5]"))
  expect_gg(mcmc_acf_bar(arr, pars = "beta[1]", regex_pars = "x\\:[2,5]"))
  expect_gg(mcmc_acf(arr1chain, regex_pars = "beta"))
  expect_gg(mcmc_acf_bar(arr1chain, regex_pars = "beta"))

  for (x in c("arr", "mat", "dframe", "dframe_multiple_chains",
              "arr1", "mat1", "dframe1")) {
    xx <- get(x)
    expect_gg(mcmc_acf(xx))
    expect_gg(mcmc_acf_bar(xx))
  }
})

test_that("mcmc_acf & mcmc_acf_bar throw correct errors", {
  expect_error(mcmc_acf(arr, regex_pars = "beta", lags = 200),
               regexp = "Too few iterations for lags=200")
})

