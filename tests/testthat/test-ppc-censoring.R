library(bayesplot)
context("PPC: censoring")

source(test_path("data-for-ppc-tests.R"))

test_that("ppc_km_overlay returns a ggplot object", {
  skip_if_not_installed("ggfortify")
  expect_gg(ppc_km_overlay(y, yrep, status_y = status_y, left_truncation_y = left_truncation_y, size = 0.5, alpha = 0.2, extrapolation_factor = Inf))
  expect_gg(ppc_km_overlay(y, yrep, status_y = status_y, left_truncation_y = left_truncation_y, size = 0.5, alpha = 0.2, extrapolation_factor = 1))
  expect_gg(ppc_km_overlay(y2, yrep2, status_y = status_y2))
})

test_that("ppc_km_overlay_grouped returns a ggplot object", {
  skip_if_not_installed("ggfortify")
  expect_gg(ppc_km_overlay_grouped(y, yrep, group,
                                   status_y = status_y,
                                   left_truncation_y = left_truncation_y,
                                   size = 0.5, alpha = 0.2))
  expect_gg(ppc_km_overlay_grouped(y, yrep, as.numeric(group),
                                   status_y = status_y,
                                   left_truncation_y = left_truncation_y,
                                   size = 0.5, alpha = 0.2))
  expect_gg(ppc_km_overlay_grouped(y, yrep, as.integer(group),
                                   status_y = status_y,
                                   left_truncation_y = left_truncation_y,
                                   size = 0.5, alpha = 0.2))

  expect_gg(ppc_km_overlay_grouped(y2, yrep2, group2,
                                   status_y = status_y2))
  expect_gg(ppc_km_overlay_grouped(y2, yrep2, as.numeric(group2),
                                   status_y = status_y2))
  expect_gg(ppc_km_overlay_grouped(y2, yrep2, as.integer(group2),
                                   status_y = status_y2))
})

test_that("ppc_km_overlay errors if bad status_y value", {
  skip_if_not_installed("ggfortify")
  expect_error(
    ppc_km_overlay(y, yrep, status_y = FALSE),
    "`status_y` must be a numeric vector of 0s and 1s the same length as `y`."
  )
  expect_error(
    ppc_km_overlay(y, yrep, status_y = 1:10),
    "`status_y` must be a numeric vector of 0s and 1s the same length as `y`."
  )
  expect_error(
    ppc_km_overlay(y, yrep, status_y = rep(10, length(y))),
    "`status_y` must be a numeric vector of 0s and 1s the same length as `y`."
  )
})

test_that("ppc_km_overlay errors if bad left_truncation_y value", {
  skip_if_not_installed("ggfortify")
  expect_error(
    ppc_km_overlay(y, yrep, status_y = status_y, left_truncation_y = "a"),
    "`left_truncation_y` must be a numeric vector of the same length as `y`"
  )
  expect_error(
    ppc_km_overlay(y, yrep, status_y = status_y, left_truncation_y = 1:10),
    "`left_truncation_y` must be a numeric vector of the same length as `y`"
  )
})

test_that("ppc_km_overlay errors if bad extrapolation_factor value", {
  skip_if_not_installed("ggfortify")
  expect_error(
    ppc_km_overlay(y, yrep, status_y = status_y, extrapolation_factor = 0.99),
    "`extrapolation_factor` must be greater than or equal to 1."
  )
})

test_that("ppc_km_overlay messages if extrapolation_factor left at default value", {
  skip_if_not_installed("ggfortify")
  expect_message(
    ppc_km_overlay(y, yrep, status_y = status_y),
    "To display all posterior predictive draws, set `extrapolation_factor = Inf`.",
  )
})

# Visual tests -----------------------------------------------------------------

test_that("ppc_km_overlay renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_if_not_installed("ggfortify")
  skip_on_r_oldrel()

  p_base <- ppc_km_overlay(vdiff_y2, vdiff_yrep2, status_y = vdiff_status_y2)
  vdiffr::expect_doppelganger("ppc_km_overlay (default)", p_base)

  p_custom <- ppc_km_overlay(
    vdiff_y2,
    vdiff_yrep2,
    status_y = vdiff_status_y2,
    size = 2,
    alpha = .2)
  vdiffr::expect_doppelganger("ppc_km_overlay (size, alpha)", p_custom)

  p_base2 <- ppc_km_overlay(vdiff_y3, vdiff_yrep3, status_y = vdiff_status_y3)
  vdiffr::expect_doppelganger("ppc_km_overlay (default 2)", p_base2)

  p_custom2_left_truncation <- ppc_km_overlay(
    vdiff_y3,
    vdiff_yrep3,
    status_y = vdiff_status_y3,
    left_truncation_y = vdiff_left_truncation_y3)
  vdiffr::expect_doppelganger("ppc_km_overlay (left_truncation_y)",
                              p_custom2_left_truncation)

  p_custom2_no_extrapolation <- ppc_km_overlay(
    vdiff_y3,
    vdiff_yrep3,
    status_y = vdiff_status_y3,
    extrapolation_factor = 1
  )
  vdiffr::expect_doppelganger("ppc_km_overlay (no extrapolation)",
                              p_custom2_no_extrapolation)

  p_custom2_max_extrapolation <- ppc_km_overlay(
    vdiff_y3,
    vdiff_yrep3,
    status_y = vdiff_status_y3,
    extrapolation_factor = Inf
  )
  vdiffr::expect_doppelganger("ppc_km_overlay (max extrapolation)",
                              p_custom2_max_extrapolation)
})

test_that("ppc_km_overlay_grouped renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_if_not_installed("ggfortify")
  skip_on_r_oldrel()

  p_base <- ppc_km_overlay_grouped(vdiff_y2, vdiff_yrep2, vdiff_group2,
                                   status_y = vdiff_status_y2)
  vdiffr::expect_doppelganger("ppc_km_overlay_grouped (default)", p_base)

  p_custom <- ppc_km_overlay_grouped(
    vdiff_y2,
    vdiff_yrep2,
    vdiff_group2,
    status_y = vdiff_status_y2,
    size = 2,
    alpha = .2
  )

  vdiffr::expect_doppelganger(
    "ppc_km_overlay_grouped (size, alpha)",
    p_custom
  )

  p_base2 <- ppc_km_overlay_grouped(vdiff_y3, vdiff_yrep3, vdiff_group3,
                                   status_y = vdiff_status_y3)
  vdiffr::expect_doppelganger("ppc_km_overlay_grouped (default 2)", p_base2)

  p_custom2_left_truncation <- ppc_km_overlay_grouped(
    vdiff_y3,
    vdiff_yrep3,
    vdiff_group3,
    status_y = vdiff_status_y3,
    left_truncation_y = vdiff_left_truncation_y3
  )

  vdiffr::expect_doppelganger(
    "ppc_km_overlay_grouped (left_truncation_y)",
    p_custom2_left_truncation
  )

  p_custom2_no_extrapolation <- ppc_km_overlay_grouped(
    vdiff_y3,
    vdiff_yrep3,
    vdiff_group3,
    status_y = vdiff_status_y3,
    extrapolation_factor = 1
  )

  vdiffr::expect_doppelganger(
    "ppc_km_overlay_grouped (no extrapolation)",
    p_custom2_no_extrapolation
  )

  p_custom2_max_extrapolation <- ppc_km_overlay_grouped(
    vdiff_y3,
    vdiff_yrep3,
    vdiff_group3,
    status_y = vdiff_status_y3,
    extrapolation_factor = Inf
  )

  vdiffr::expect_doppelganger(
    "ppc_km_overlay_grouped (max extrapolation)",
    p_custom2_max_extrapolation
  )
})
