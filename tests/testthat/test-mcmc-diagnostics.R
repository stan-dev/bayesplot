library(bayesplot)
context("MCMC: diagnostics")

source(test_path("data-for-mcmc-tests.R"))

test_that("rhat and neff plots return a ggplot object", {
  rhat <- runif(100, 1, 1.5)
  expect_gg(mcmc_rhat(rhat))
  expect_gg(mcmc_rhat_hist(rhat, binwidth = .01))

  ratio <- runif(100, 0, 1)
  expect_gg(mcmc_neff(ratio))
  expect_gg(mcmc_neff_hist(ratio, binwidth = .01))

  # 1-D array ok
  expect_gg(mcmc_rhat(array(rhat)))
  expect_gg(mcmc_rhat_hist(array(rhat), binwidth = .01))
  expect_gg(mcmc_neff(array(ratio)))
  expect_gg(mcmc_neff_hist(array(ratio), binwidth = .01))

  # named ok
  rhat <- setNames(runif(5, 1, 1.5), paste0("alpha[", 1:5, "]"))
  expect_gg(mcmc_rhat(rhat))

  # doesn't error with ratios > 1 (not common but can happen)
  expect_gg(mcmc_neff(ratio = c(0.5, 1, 1.25)))
  expect_gg(mcmc_neff(ratio = c(0.5, 1, 2)))
})

test_that("rhat and neff plot functions throw correct errors & warnings", {
  # need vector or 1D array
  expect_error(mcmc_rhat_hist(cbind(1:2)), "is.array")
  expect_error(mcmc_neff_hist(list(1,2)), "is.numeric")

  # need positive rhat values
  expect_error(mcmc_rhat(c(-1, 1, 1)), "must be positive")

  # need ratios between 0 and 1
  expect_error(mcmc_neff(c(-1, 0.5, 0.7)), "must be positive")

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

test_that("'description' & 'rating' columns are correct (#176)", {
  # https://github.com/stan-dev/bayesplot/issues/176
  rhats <- c(1, 1.07, 1.19, 1.07, 1.3, 1)
  expected_rhats <- sort(rhats)
  expected_ratings <- rep(c("low", "ok", "high"), each = 2)
  expected_descriptions <-
    rep(c("hat(R) <= 1.05", "hat(R) <= 1.1", "hat(R) > 1.1"), each = 2)

  df <- mcmc_rhat_data(rhats)
  expect_equal(df$value, expected_rhats)
  expect_equal(as.character(df$rating), expected_ratings)
  expect_equal(df$description, expected_descriptions)

  ratios <- c(0.4, 0.05, 0.6)
  expected_ratios <- sort(ratios)
  expected_ratings <- c("low", "ok", "high")
  expected_descriptions <-
    c("N[eff]/N <= 0.1", "N[eff]/N <= 0.5", "N[eff]/N > 0.5")

  df <- mcmc_neff_data(ratios)
  expect_equal(df$value, expected_ratios)
  expect_equal(as.character(df$rating), expected_ratings)
  expect_equal(df$description, expected_descriptions)
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




# Visual tests -----------------------------------------------------------------

test_that("mcmc_rhat renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  rhats <- seq(from = 1, to = 1.20, length.out = 10)

  p_base <- mcmc_rhat(rhats)
  vdiffr::expect_doppelganger("mcmc_rhat (default)", p_base)

  p_size <- mcmc_rhat(rhats, size = 3)
  vdiffr::expect_doppelganger("mcmc_rhat (sized)", p_size)
})

test_that("mcmc_rhat_hist renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  rhats <- seq(from = 1, to = 1.20, length.out = 10)

  p_base <- mcmc_rhat_hist(rhats)
  vdiffr::expect_doppelganger("mcmc_rhat_hist (default)", p_base)

  p_binwidth <- mcmc_rhat_hist(rhats, binwidth = .02)
  vdiffr::expect_doppelganger("mcmc_rhat_hist (binwidth)", p_binwidth)
})


test_that("mcmc_neff renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  neffs <- seq(from = 0, to = 1, length.out = 20)

  p_base <- mcmc_neff(neffs)
  vdiffr::expect_doppelganger("mcmc_neff (default)", p_base)
})

test_that("mcmc_neff renders legend correctly even if some levels missing", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  neffs <- c(0.1, 0.2, 0.3, 0.4) # above 0.5 is missing but should still appear in legend

  p_base <- mcmc_neff(neffs)
  vdiffr::expect_doppelganger("mcmc_neff (missing levels)", p_base)
})

test_that("mcmc_neff_hist renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  neffs <- seq(from = 0, to = 1, length.out = 20)

  p_base <- mcmc_neff_hist(neffs)
  vdiffr::expect_doppelganger("mcmc_neff_hist (default)", p_base)

  p_binwidth <- mcmc_neff_hist(neffs, binwidth = .05)
  vdiffr::expect_doppelganger("mcmc_neff_hist (binwidth)", p_binwidth)
})

test_that("mcmc_acf renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_acf(vdiff_dframe)
  vdiffr::expect_doppelganger("mcmc_acf (default)", p_base)

  p_lags <- mcmc_acf(vdiff_dframe, lags = 5)
  vdiffr::expect_doppelganger("mcmc_acf (lags)", p_lags)
})

test_that("mcmc_acf_bar renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_acf_bar(vdiff_dframe)
  vdiffr::expect_doppelganger("mcmc_acf_bar (default)", p_base)

  p_lags <- mcmc_acf_bar(vdiff_dframe, lags = 5)
  vdiffr::expect_doppelganger("mcmc_acf_bar (lags)", p_lags)
})
