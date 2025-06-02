library(bayesplot)
context("PPC: scatterplots")

source(test_path("data-for-ppc-tests.R"))

test_that("ppc_scatter returns ggplot object", {
  expect_gg(ppc_scatter(y, yrep[1,, drop = FALSE]))
  expect_gg(ppc_scatter(y, yrep[1:3, ]))
  expect_gg(ppc_scatter(y2, yrep2))
})

test_that("ppc_scatter_avg returns ggplot object", {
  expect_gg(ppc_scatter_avg(y, yrep))
  expect_gg(ppc_scatter_avg(y2, yrep2))
})

test_that("ppc_scatter_avg_data same as ppc_scatter_data if nrow(yrep) = 1", {
  # really only a few columns are _exactly_ the same
  cols <- c("y_id", "y_obs", "value")
  d1 <- ppc_scatter_data(y2, yrep2)
  d2 <- ppc_scatter_avg_data(y2, yrep2)
  expect_equal(d1[, cols], d2[, cols])
})

test_that("ppc_scatter_avg_grouped returns a ggplot object", {
  expect_gg(ppc_scatter_avg_grouped(y, yrep, group))
  expect_gg(ppc_scatter_avg_grouped(y, yrep, as.numeric(group)))
  expect_gg(ppc_scatter_avg_grouped(y, yrep, as.integer(group)))
})

test_that("ppc_scatter_avg_data can take a custom fun_avg", {
  # using the colMeans() and colSums() to avoid using apply(yrep, 2, fun)
  # because apply() is used in ppc_scatter_avg_data()
  means <- ppc_scatter_avg_data(y, yrep)
  expect_equal(means$value, colMeans(yrep))
  sums <- ppc_scatter_avg_data(y, yrep, stat = "sum")
  expect_equal(sums$value, colSums(yrep))
})



# Visual tests ------------------------------------------------------------

test_that("ppc_scatter renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_scatter(vdiff_y, vdiff_yrep[1:6, ])
  vdiffr::expect_doppelganger("ppc_scatter (default)", p_base)

  p_custom <- ppc_scatter(
    y = vdiff_y,
    yrep = vdiff_yrep[1:6, ],
    size = 1,
    alpha = 1
  )

  vdiffr::expect_doppelganger(
    title = "ppc_scatter (size, alpha)",
    fig = p_custom)
})

test_that("ppc_scatter_avg renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_scatter_avg(vdiff_y, vdiff_yrep)
  vdiffr::expect_doppelganger("ppc_scatter_avg (default)", p_base)

  p_custom <- ppc_scatter_avg(
    y = vdiff_y,
    yrep = vdiff_yrep,
    size = 1.5,
    alpha = .1
  )

  vdiffr::expect_doppelganger(
    title = "ppc_scatter_avg (size, alpha)",
    fig = p_custom)
})

test_that("ppc_scatter_avg_grouped renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_scatter_avg_grouped(vdiff_y, vdiff_yrep, vdiff_group)
  vdiffr::expect_doppelganger("ppc_scatter_avg_grouped (default)", p_base)

  p_custom <- ppc_scatter_avg_grouped(
    y = vdiff_y,
    yrep = vdiff_yrep,
    group = vdiff_group,
    size = 3,
    alpha = 0.25,
    ref_line = FALSE
  )

  vdiffr::expect_doppelganger(
    title = "ppc_scatter_avg_grouped (size, alpha, ref_line)",
    fig = p_custom)
})
