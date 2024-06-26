library(bayesplot)
context("MCMC: recover")

set.seed(123)
draws <- matrix(rnorm(4 * 1000), nrow = 1000)
colnames(draws) <- c("alpha", "beta[1]", "beta[2]", "sigma")
true <- c(-1, 0, 0.5, 1)

test_that("mcmc_recover_intervals throws correct errors", {
  expect_error(
    mcmc_recover_intervals(draws, letters[1:ncol(draws)]),
    "is.numeric(true) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    mcmc_recover_intervals(draws, true[-1]),
    "ncol(x) == length(true) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    mcmc_recover_intervals(draws, true, batch = 1:3),
    "length(batch) == length(true) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    mcmc_recover_intervals(draws, true, prob = 0.8, prob_outer = 0.5),
    "prob_outer >= prob is not TRUE",
    fixed = TRUE
  )
  expect_error(
    mcmc_recover_intervals(draws, true, prob = 0, prob_outer = 0.5),
    "prob > 0 is not TRUE",
    fixed = TRUE
  )
  expect_error(
    mcmc_recover_intervals(draws, true, prob = .5, prob_outer = 1.1),
    "prob_outer <= 1 is not TRUE",
    fixed = TRUE
  )
})

test_that("mcmc_recover_intervals returns a ggplot object", {
  expect_gg(mcmc_recover_intervals(draws, true))
  expect_gg(mcmc_recover_intervals(draws, true, batch = c(1, 2, 2, 1),
                                   point_est = "mean"))
  expect_gg(mcmc_recover_intervals(draws, true, batch = grepl("beta", colnames(draws))))
  expect_gg(mcmc_recover_intervals(draws, true, batch = grepl("beta", colnames(draws)),
                                   facet_args = list(ncol = 1)))
})

test_that("mcmc_recover_intervals works when point_est = 'none'", {
  a <- mcmc_recover_intervals(draws, true, batch = 1:4, point_est = "none")
  expect_gg(a)
  expect_equal(a$data$Point, rep(NA, ncol(draws)))
})


test_that("mcmc_recover_scatter returns a ggplot object", {
  expect_gg(
    mcmc_recover_scatter(draws, true)
  )
  expect_gg(
    mcmc_recover_scatter(
      draws,
      true,
      batch = 1:4,
      point_est = "mean",
      facet_args = list(scales = "fixed")
    )
  )
  expect_gg(
    mcmc_recover_scatter(
      draws,
      true,
      batch = c(1, 2, 2, 1),
      point_est = "mean"
    )
  )
  expect_gg(
    mcmc_recover_scatter(
      draws,
      true,
      batch = grepl("beta", colnames(draws))
    )
  )
  expect_gg(
    mcmc_recover_scatter(
      draws,
      true,
      batch = grepl("beta", colnames(draws)),
      facet_args = list(ncol = 1)
    )
  )
})


test_that("mcmc_recover_hist returns a ggplot object", {
  expect_gg(mcmc_recover_hist(draws, true, binwidth = 0.1))
  expect_gg(mcmc_recover_hist(draws, true, binwidth = 0.1,
                              facet_args = list(nrow = 1)))
})


# Visual tests -----------------------------------------------------------------

test_that("mcmc_recover_hist renders correctly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_recover_hist(draws, true, binwidth = 0.01)
  vdiffr::expect_doppelganger("mcmc_recover_hist (default)", p_base)
})

test_that("mcmc_recover_intervals renders correctly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_recover_intervals(draws, true)
  vdiffr::expect_doppelganger("mcmc_recover_intervals (default)", p_base)

  p_custom <- mcmc_recover_intervals(draws, true, prob = 0.6, prob_outer = 0.8)
  vdiffr::expect_doppelganger("mcmc_recover_intervals (prob)", p_custom)
})

test_that("mcmc_recover_scatter renders correctly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_recover_scatter(draws, true)
  vdiffr::expect_doppelganger("mcmc_recover_scatter (default)", p_base)

  p_custom <- mcmc_recover_scatter(draws, true, size = 6)
  vdiffr::expect_doppelganger("mcmc_recover_scatter (size)", p_custom)
})
