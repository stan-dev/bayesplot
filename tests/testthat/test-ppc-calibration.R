library(bayesplot)
context("PPC: calibration")

if (!exists("expect_gg")) expect_gg <- bayesplot:::expect_gg

# Create binary test data for calibration plots
set.seed(1234)
n_obs <- 100
n_draws <- 50
p_true <- runif(n_obs)
calib_y <- rbinom(n_obs, 1, p_true)
calib_prep <- matrix(pmin(1,pmax(0, p_true + rnorm(n_obs * n_draws, 0, .1))), nrow = n_draws, ncol = n_obs)
calib_yrep <- t(apply(calib_prep, 1, rbinom, n = n_obs, size = 1))
calib_group <- gl(2, n_obs / 2, labels = c("A", "B"))
calib_lw <- matrix(runif(n_obs * n_draws), ncol = n_obs, nrow = n_draws)

test_that("ppc_calibration_overlay returns a ggplot object", {
  expect_gg(ppc_calibration_overlay(calib_y, calib_prep))
  expect_gg(ppc_calibration_overlay(calib_y, calib_prep[1:5, ], 
                                   linewidth = 0.5, alpha = 0.3))
})

test_that("ppc_calibration_overlay_grouped returns a ggplot object", {
  expect_gg(ppc_calibration_overlay_grouped(calib_y, calib_prep, calib_group))
  expect_gg(ppc_calibration_overlay_grouped(calib_y, calib_prep[1:3, ], calib_group,
                                           linewidth = 0.5, alpha = 0.3))
})

test_that("ppc_calibration returns a ggplot object", {
  expect_gg(ppc_calibration(calib_y, calib_prep))
  expect_gg(ppc_calibration(calib_y, calib_yrep))
  expect_gg(ppc_calibration(calib_y, calib_prep, prob = 0.9,
                           linewidth = 0.8, alpha = 0.5))
  # Test new interval_type parameter
  expect_gg(ppc_calibration(calib_y, calib_prep, interval_type = "confidence"))
  expect_gg(ppc_calibration(calib_y, calib_prep, interval_type = "consistency"))
})

test_that("ppc_calibration_grouped returns a ggplot object", {
  expect_gg(ppc_calibration_grouped(calib_y, calib_prep, calib_group))
  expect_gg(ppc_calibration_grouped(calib_y, calib_yrep, calib_group))
  expect_gg(ppc_calibration_grouped(calib_y, calib_prep[1:5, ], calib_group,
                                   prob = 0.9, linewidth = 0.8, alpha = 0.5))
  # Test new interval_type parameter
  expect_gg(ppc_calibration_grouped(calib_y, calib_prep, calib_group, interval_type = "confidence"))
  expect_gg(ppc_calibration_grouped(calib_y, calib_prep, calib_group, interval_type = "consistency"))
})

test_that("ppc_loo_calibration returns a ggplot object", {
  # Note: This function now returns interval plots instead of overlay plots
  # Testing basic functionality for now
  expect_gg(ppc_loo_calibration(calib_y, calib_yrep, calib_prep))
  expect_gg(ppc_loo_calibration(calib_y, calib_yrep[1:3, ], calib_prep[1:3, ],
                               prob = 0.9, linewidth = 0.8, alpha = 0.5))
})

test_that("ppc_loo_calibration_grouped returns a ggplot object", {
  # Note: This function now returns interval plots instead of overlay plots
  # Testing basic functionality for now
  expect_gg(ppc_loo_calibration_grouped(calib_y, calib_prep, calib_group, calib_lw))
  expect_gg(ppc_loo_calibration_grouped(calib_y, calib_prep[1:3, ], calib_group, calib_lw[1:3, ],
                                       prob = 0.9, linewidth = 0.8, alpha = 0.5))
})

test_that("calibration functions handle edge cases", {
  # Single observation
  expect_gg(ppc_calibration_overlay(1, matrix(0.5, nrow = 1, ncol = 1)))
  expect_gg(ppc_calibration(1, matrix(0.5, nrow = 1, ncol = 1)))
  
  # Single draw
  expect_gg(ppc_calibration_overlay(calib_y, calib_prep[1, , drop = FALSE]))
  expect_gg(ppc_calibration(calib_y, calib_prep[1, , drop = FALSE]))
  
  # All zeros or ones
  expect_gg(ppc_calibration_overlay(rep(0, 10), matrix(0.1, nrow = 5, ncol = 10)))
  expect_gg(ppc_calibration_overlay(rep(1, 10), matrix(0.9, nrow = 5, ncol = 10)))
})

test_that("calibration functions validate inputs correctly", {
  # Invalid probabilities (outside [0,1])
  expect_error(ppc_calibration_overlay(calib_y, matrix(1.5, nrow = 5, ncol = 50)),
               "Values of ´prep´ should be predictive probabilities between 0 and 1")
  expect_error(ppc_calibration_overlay(calib_y, matrix(-0.1, nrow = 5, ncol = 50)),
               "Values of ´prep´ should be predictive probabilities between 0 and 1")
  
  # Mismatched dimensions
  expect_error(ppc_calibration_overlay(calib_y, calib_prep[, 1:25]),
               "ncol(yrep) must be equal to length(y).", fixed=TRUE)
  
  # Invalid group
  expect_error(ppc_calibration_overlay_grouped(calib_y, calib_prep, calib_group[1:25]),
               "length(group) must be equal to the number of observations.",
              fixed=TRUE)
  
  # Invalid interval_type
  expect_error(ppc_calibration(calib_y, calib_prep, interval_type = "invalid"),
               "should be one of")
})

test_that("calibration functions work with different group types", {
  # Numeric groups
  expect_gg(ppc_calibration_overlay_grouped(calib_y, calib_prep, as.numeric(calib_group)))
  expect_gg(ppc_calibration_grouped(calib_y, calib_prep, as.numeric(calib_group)))
  
  # Integer groups
  expect_gg(ppc_calibration_overlay_grouped(calib_y, calib_prep, as.integer(calib_group)))
  expect_gg(ppc_calibration_grouped(calib_y, calib_prep, as.integer(calib_group)))
  
  # Character groups
  expect_gg(ppc_calibration_overlay_grouped(calib_y, calib_prep, as.character(calib_group)))
  expect_gg(ppc_calibration_grouped(calib_y, calib_prep, as.character(calib_group)))
})

# Visual tests -----------------------------------------------------------------

test_that("ppc_calibration_overlay renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_calibration_overlay(calib_y, calib_prep)
  vdiffr::expect_doppelganger("ppc_calibration_overlay (default)", p_base)

  p_custom <- ppc_calibration_overlay(
    calib_y,
    calib_prep,
    prob = .99,
    linewidth = 0.5,
    alpha = 0.3
  )
  vdiffr::expect_doppelganger("ppc_calibration_overlay (custom)", p_custom)
})

test_that("ppc_calibration_overlay_grouped renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_calibration_overlay_grouped(calib_y, calib_prep, calib_group)
  vdiffr::expect_doppelganger("ppc_calibration_overlay_grouped (default)", p_base)

  p_custom <- ppc_calibration_overlay_grouped(
    calib_y,
    calib_prep,
    calib_group,
    prob = .99,
    linewidth = 0.5,
    alpha = 0.3
  )
  vdiffr::expect_doppelganger("ppc_calibration_overlay_grouped (custom)", p_custom)
})

test_that("ppc_calibration renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_calibration(calib_y, calib_prep)
  vdiffr::expect_doppelganger("ppc_calibration (default)", p_base)

  p_custom <- ppc_calibration(
    calib_y,
    calib_prep,
    prob = 0.9,
    linewidth = 0.8,
    alpha = 0.5
  )
  vdiffr::expect_doppelganger("ppc_calibration (custom)", p_custom)
  
  # Test interval_type variants
  p_confidence <- ppc_calibration(
    calib_y,
    calib_prep,
    interval_type = "confidence"
  )
  vdiffr::expect_doppelganger("ppc_calibration (confidence)", p_confidence)
  
  p_consistency <- ppc_calibration(
    calib_y,
    calib_prep,
    interval_type = "consistency"
  )
  vdiffr::expect_doppelganger("ppc_calibration (consistency)", p_consistency)
})

test_that("ppc_calibration_grouped renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_calibration_grouped(calib_y, calib_prep, calib_group)
  vdiffr::expect_doppelganger("ppc_calibration_grouped (default)", p_base)

  p_custom <- ppc_calibration_grouped(
    calib_y,
    calib_prep,
    calib_group,
    prob = 0.9,
    linewidth = 0.8,
    alpha = 0.5
  )
  vdiffr::expect_doppelganger("ppc_calibration_grouped (custom)", p_custom)
  
  # Test interval_type variants
  p_confidence <- ppc_calibration_grouped(
    calib_y,
    calib_prep,
    calib_group,
    interval_type = "confidence"
  )
  vdiffr::expect_doppelganger("ppc_calibration_grouped (confidence)", p_confidence)
  
  p_consistency <- ppc_calibration_grouped(
    calib_y,
    calib_prep,
    calib_group,
    interval_type = "consistency"
  )
  vdiffr::expect_doppelganger("ppc_calibration_grouped (consistency)", p_consistency)
})

test_that("ppc_loo_calibration renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_loo_calibration(calib_y, calib_yrep, calib_lw)
  vdiffr::expect_doppelganger("ppc_loo_calibration (default)", p_base)

  p_custom <- ppc_loo_calibration(
    calib_y,
    calib_yrep,
    calib_lw,
    linewidth = 0.5,
    alpha = 0.3
  )
  vdiffr::expect_doppelganger("ppc_loo_calibration (custom)", p_custom)
})

test_that("ppc_loo_calibration_grouped renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_loo_calibration_grouped(calib_y, calib_yrep, calib_group, calib_lw)
  vdiffr::expect_doppelganger("ppc_loo_calibration_grouped (default)", p_base)

  p_custom <- ppc_loo_calibration_grouped(
    calib_y,
    calib_yrep,
    calib_group,
    calib_lw,
    linewidth = 0.5,
    alpha = 0.3
  )
  vdiffr::expect_doppelganger("ppc_loo_calibration_grouped (custom)", p_custom)
})
