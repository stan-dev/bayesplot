library(bayesplot)
context("PPC: test-statistics")

source("data-for-ppc-tests.R")

q25 <- function(x) quantile(x, 0.25)
prop0 <- function(x) mean(x == 0)

test_that("ppc_stat throws errors if function not found", {
  expect_error(ppc_stat(y, yrep, stat = "9999"), "not found")
  expect_error(ppc_stat_grouped(y, yrep, group, stat = "9999"), "not found")
  expect_error(ppc_stat_freqpoly_grouped(y, yrep, group, stat = "9999"), "not found")
})

test_that("ppc_stat throws errors if 'stat' wrong length", {
  expect_error(ppc_stat(y, yrep, stat = c("mean", "sd")), "not a function")
  expect_error(ppc_stat_grouped(y, yrep, group, stat = c("mean", "sd")), "not a function")
  expect_error(ppc_stat_freqpoly_grouped(y, yrep, group, stat = c(mean, sd)), "not a function")
})

test_that("ppc_stat returns ggplot object", {
  expect_gg(ppc_stat(y, yrep))
  expect_gg(ppc_stat(y, yrep, stat = "sd"))
  expect_gg(ppc_stat(y, yrep, stat = sd))
  expect_gg(ppc_stat(y, yrep, stat = "q25"))
  expect_gg(ppc_stat(y, yrep, stat = q25))
  expect_gg(ppc_stat(y, yrep, stat = function(x) median(x)))

  expect_gg(ppc_stat(y2, yrep2))
  expect_gg(ppc_stat(y2, yrep2, stat = "prop0"))
})

test_that("ppc_stat_2d returns ggplot object", {
  expect_gg(ppc_stat_2d(y, yrep))
  expect_gg(ppc_stat_2d(y, yrep, stat = c("q25", "median")))
  expect_gg(ppc_stat_2d(y, yrep, stat = c("q25", median)))
  expect_gg(ppc_stat_2d(y, yrep, stat = c(function(x) mean(x), function(y) sd(y))))

  expect_gg(ppc_stat_2d(y2, yrep2))
})

test_that("ppc_stat_2d erros if more than 2 stats", {
  expect_error(ppc_stat_2d(y, yrep, stat = c("mean", "sd", "var")),
               "argument must have length 2")
})

test_that("ppc_stat_grouped returns ggplot object", {
  expect_gg(ppc_stat_grouped(y, yrep, group))
  expect_gg(ppc_stat_grouped(y, yrep, as.numeric(group), stat = function(z) var(z)))
  expect_gg(ppc_stat_grouped(y, yrep, as.integer(group), stat = "sd"))

  expect_error(ppc_stat_grouped(y2, yrep2, group2),
               "'group' must have more than one unique value")
})
test_that("ppc_stat_freqpoly_grouped returns ggplot object", {
  expect_gg(ppc_stat_freqpoly_grouped(y, yrep, group, stat = "sd", freq = FALSE))
  expect_gg(ppc_stat_freqpoly_grouped(y, yrep, group, stat = function(x) sd(x), freq = TRUE))
})


# Visual tests ------------------------------------------------------------

test_that("ppc_stat renders correctly", {
  testthat::skip_on_cran()

  p_base <- ppc_stat(vdiff_y, vdiff_yrep) + yaxis_text()
  vdiffr::expect_doppelganger("ppc_stat (default)", p_base)

  p_custom <- ppc_stat(
    y = vdiff_y,
    yrep = vdiff_yrep,
    stat = "mad",
    binwidth = .05,
    freq = FALSE
  ) + yaxis_text()

  vdiffr::expect_doppelganger(
    title = "ppc_stat (stat, binwidth, freq)",
    fig = p_custom)
})

test_that("ppc_stat_2d renders correctly", {
  testthat::skip_on_cran()

  p_base <- ppc_stat_2d(vdiff_y, vdiff_yrep)
  vdiffr::expect_doppelganger("ppc_stat_2d (default)", p_base)

  p_custom <- ppc_stat_2d(
    y = vdiff_y,
    yrep = vdiff_yrep,
    stat = c("median", "mad"),
    size = 5,
    alpha = 1
  )

  vdiffr::expect_doppelganger(
    title = "ppc_stat_2d (stat, size, alpha)",
    fig = p_custom)
})

test_that("ppc_stat_grouped renders correctly", {
  testthat::skip_on_cran()

  p_base <- ppc_stat_grouped(vdiff_y, vdiff_yrep, vdiff_group)
  vdiffr::expect_doppelganger("ppc_stat_grouped (default)", p_base)

  p_custom <- ppc_stat_grouped(
    y = vdiff_y,
    yrep = vdiff_yrep,
    group = vdiff_group,
    stat = stats::var,
    facet_args = list(scales = "fixed", ncol = 1),
    binwidth = .25
  )

  vdiffr::expect_doppelganger(
    title = "ppc_stat_grouped (stat, facet_args, binwidth)",
    fig = p_custom)
})

test_that("ppc_stat_freqpoly_grouped renders correctly", {
  testthat::skip_on_cran()

  p_base <- ppc_stat_freqpoly_grouped(vdiff_y, vdiff_yrep, vdiff_group)
  vdiffr::expect_doppelganger("ppc_stat_freqpoly_grouped (default)", p_base)

  p_custom <- ppc_stat_freqpoly_grouped(
    y = vdiff_y,
    yrep = vdiff_yrep,
    group = vdiff_group,
    stat = "sum",
    facet_args = list(scales = "fixed", ncol = 1),
    binwidth = .5
  )

  vdiffr::expect_doppelganger(
    title = "ppc_stat_freqpoly_grouped (stat, facet_args, binwidth)",
    fig = p_custom)
})


