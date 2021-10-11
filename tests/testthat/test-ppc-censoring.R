library(bayesplot)
context("PPC: censoring")

source(test_path("data-for-ppc-tests.R"))

test_that("ppc_km_overlay returns a ggplot object", {
  skip_if_not_installed("ggfortify")
  expect_gg(ppc_km_overlay(y, yrep, status_y = status_y, size = 0.5, alpha = 0.2))
  expect_gg(ppc_km_overlay(y2, yrep2, status_y = status_y2))
})

test_that("ppc_km_overlay_grouped returns a ggplot object", {
  skip_if_not_installed("ggfortify")
  expect_gg(ppc_km_overlay_grouped(y, yrep, group,
                                   status_y = status_y))
  expect_gg(ppc_km_overlay_grouped(y, yrep, as.numeric(group),
                                   status_y = status_y))
  expect_gg(ppc_km_overlay_grouped(y, yrep, as.integer(group),
                                   status_y = status_y))

  expect_gg(ppc_km_overlay_grouped(y2, yrep2, group2,
                                   status_y = status_y2))
  expect_gg(ppc_km_overlay_grouped(y2, yrep2, as.numeric(group2),
                                   status_y = status_y2))
  expect_gg(ppc_km_overlay_grouped(y2, yrep2, as.integer(group2),
                                   status_y = status_y2))
})

# Visual tests -----------------------------------------------------------------

test_that("ppc_km_overlay renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_if_not_installed("ggfortify")

  p_base <- ppc_km_overlay(vdiff_y2, vdiff_yrep2, status_y = vdiff_status_y2)
  vdiffr::expect_doppelganger("ppc_km_overlay (default)", p_base)

  p_custom <- ppc_km_overlay(
    vdiff_y2,
    vdiff_yrep2,
    status_y = vdiff_status_y2,
    size = 2,
    alpha = .2)
  vdiffr::expect_doppelganger("ppc_km_overlay (size, alpha)", p_custom)
})

test_that("ppc_km_overlay_grouped renders correctly", {
  testthat::skip_on_cran()

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
})
