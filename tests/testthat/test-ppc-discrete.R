library(bayesplot)
suppressPackageStartupMessages(library(rstanarm))
context("PPC: discrete")


# bar plots ---------------------------------------------------------------
data("esoph", package = "datasets")
fit <- stan_polr(tobgp ~ agegp, data = esoph, method = "probit", prior = R2(0.2, "mean"),
                 init_r = 0.1, seed = 12345, chains = 1, iter = 500, refresh = 0)
y <- as.integer(fit$y)
yrep_char <- posterior_predict(fit, draws = 10)
yrep <- sapply(data.frame(yrep_char, stringsAsFactors = TRUE), as.integer)

test_that("ppc_bars & ppc_bars_grouped return a ggplot object", {
  expect_gg(ppc_bars(y, yrep))
  expect_gg(ppc_bars(y, yrep, prob = 0))
  expect_gg(ppc_bars_grouped(y, yrep, group = esoph$agegp))
})

test_that("freq argument to ppc_bars works", {
  p_freq <- ggplot2::ggplot_build(ppc_bars(y, yrep, freq = TRUE))
  p_prop <- ggplot2::ggplot_build(ppc_bars(y, yrep, freq = FALSE))

  y_freq <- p_freq$data[[1]]$y
  y_prop <- p_prop$data[[1]]$y
  expect_equal(y_freq, as.integer(y_freq))
  expect_true(all(y_prop < 1) && all(y_prop > 0))
})

test_that("ppc_bars works with negative integers", {
  y <- round(rnorm(100, -10, 1))
  yrep <- round(matrix(rnorm(100 * 500, -10, 1), 500, 100))
  expect_gg(ppc_bars(y, yrep))
})

test_that("ppc_bars(_grouped) errors if y/yrep not discrete", {
  expect_error(ppc_bars(y + 0.5, yrep),
               "ppc_bars expects 'y' to be discrete")
  expect_error(ppc_bars(y, yrep + 0.5),
               "ppc_bars expects 'yrep' to be discrete")
  expect_error(ppc_bars_grouped(y + 0.5, yrep, group = esoph$agegp),
               "ppc_bars_grouped expects 'y' to be discrete")
  expect_error(ppc_bars_grouped(y, yrep + 0.5, group = esoph$agegp),
               "ppc_bars_grouped expects 'yrep' to be discrete")
})


# rootograms -----------------------------------------------------------
rm(list = ls())
source(test_path("data-for-ppc-tests.R"))

yrep3 <- matrix(yrep2, nrow = 5, ncol = ncol(yrep2), byrow = TRUE)

test_that("ppc_rootogram returns a ggplot object", {
  expect_gg(ppc_rootogram(y2, yrep2))
  expect_gg(ppc_rootogram(y2, yrep3, style = "hanging", prob = 0.5))
  expect_gg(ppc_rootogram(y2, yrep3, style = "suspended"))
})

test_that("ppc_rootogram errors if y/yrep not counts", {
  expect_error(ppc_rootogram(y, yrep),
               "ppc_rootogram expects counts as inputs to 'y'")
  expect_error(ppc_rootogram(y2, yrep[1:5, seq_along(y2)]),
               "ppc_rootogram expects counts as inputs to 'yrep'")
  expect_error(ppc_rootogram(y, yrep3),
               "ncol(yrep) must be equal to length(y)", fixed = TRUE)
})



# Visual tests ------------------------------------------------------------
test_that("ppc_bars renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  p_base <- ppc_bars(vdiff_y2, vdiff_yrep2)
  vdiffr::expect_doppelganger("ppc_bars (default)", p_base)

  p_custom <- ppc_bars(
    y = vdiff_y2,
    yrep = vdiff_yrep2,
    width = 0.5,
    size = 0.5,
    fatten = 5
  )

  vdiffr::expect_doppelganger(
    title = "ppc_bars (width, size, fatten)",
    fig = p_custom)

  p_custom_prob <- ppc_bars(
    y = vdiff_y2,
    yrep = vdiff_yrep2,
    prob = 0.33,
    width = 0.5,
    size = 0.5,
    fatten = 5
  )

  vdiffr::expect_doppelganger(
    title = "ppc_bars (prob=0.33, width, size, fatten)",
    fig = p_custom_prob)
})

test_that("ppc_bars_grouped renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  p_base <- ppc_bars_grouped(vdiff_y2, vdiff_yrep2, vdiff_group2)
  vdiffr::expect_doppelganger("ppc_bars_grouped (default)", p_base)

  p_custom <- ppc_bars_grouped(
    y = vdiff_y2,
    yrep = vdiff_yrep2,
    group = vdiff_group2,
    facet_args = list(nrow = 2),
    prob = 0.5,
    size = 0.5
  )

  vdiffr::expect_doppelganger(
    title = "ppc_bars_grouped (facet_args, prob, size)",
    fig = p_custom)
})

test_that("ppc_rootogram renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  p_base <- ppc_rootogram(vdiff_y2, vdiff_yrep2)
  vdiffr::expect_doppelganger("ppc_rootogram (default)", p_base)

  p_custom_hanging <- ppc_rootogram(
    y = vdiff_y2,
    yrep = vdiff_yrep2,
    prob = 2/3,
    size = 3,
    style = "hanging"
  )

  vdiffr::expect_doppelganger(
    title = "ppc_rootogram (style='hanging', prob, size)",
    fig = p_custom_hanging)
})


