library(bayesplot)
context("PPC: test-statistics")

source(test_path("data-for-ppc-tests.R"))

q25 <- function(x) quantile(x, 0.25)
prop0 <- function(x) mean(x == 0)

test_that("ppc_stat throws errors if function not found", {
  expect_error(ppc_stat(y, yrep, stat = "9999"), "not found")
  expect_error(ppc_stat_freqpoly(y, yrep, stat = "9999"), "not found")
  expect_error(ppc_stat_grouped(y, yrep, group, stat = "9999"), "not found")
  expect_error(ppc_stat_freqpoly_grouped(y, yrep, group, stat = "9999"), "not found")
})

test_that("ppc_stat throws errors if 'stat' wrong length", {
  expect_error(ppc_stat(y, yrep, stat = c("mean", "sd")),
               "length(stat) == 1 is not TRUE", fixed = TRUE)
  expect_error(ppc_stat_grouped(y, yrep, group, stat = c("mean", "sd")),
               "length(stat) == 1 is not TRUE", fixed = TRUE)
  expect_error(ppc_stat_freqpoly(y, yrep, stat = c("mean", "sd")),
               "length(stat) == 1 is not TRUE", fixed = TRUE)
  expect_error(ppc_stat_freqpoly_grouped(y, yrep, group, stat = c(mean, sd)),
               "length(stat) == 1 is not TRUE", fixed = TRUE)
})

test_that("ppc_stat and ppc_stat_freqpoly message if stat='mean'", {
  expect_message(
    ppc_stat(y, yrep),
    "'mean' is too weak to detect anything of interest"
  )
  expect_silent(
    ppc_stat(y, yrep, stat = "mad")
  )
  expect_message(
    ppc_stat_grouped(y, yrep, group),
    "'mean' is too weak to detect anything of interest"
  )
  expect_silent(
    ppc_stat_grouped(y, yrep, group, stat = "mad")
  )
  expect_message(
    ppc_stat_freqpoly(y, yrep),
    "'mean' is too weak to detect anything of interest"
  )
  expect_silent(
    ppc_stat_freqpoly(y, yrep, group, stat = "mad")
  )
  expect_message(
    ppc_stat_freqpoly_grouped(y, yrep, group),
    "'mean' is too weak to detect anything of interest"
  )
  expect_silent(
    ppc_stat_freqpoly_grouped(y, yrep, group, stat = "mad")
  )
})

test_that("ppc_stat returns ggplot object", {
  expect_gg(ppc_stat(y, yrep, binwidth = 0.05))
  expect_gg(ppc_stat(y, yrep, stat = "sd", binwidth = 0.05))
  expect_gg(ppc_stat(y, yrep, stat = sd, binwidth = 0.05))
  expect_gg(ppc_stat(y, yrep, stat = "q25", binwidth = 0.05))
  expect_gg(ppc_stat(y, yrep, stat = q25, binwidth = 0.05))
  expect_gg(ppc_stat(y, yrep, stat = function(x) median(x), binwidth = 0.05))
  expect_gg(ppc_stat(y2, yrep2, binwidth = 0.05))
  expect_gg(ppc_stat(y2, yrep2, stat = "prop0", binwidth = 0.05))
  expect_gg(ppc_stat(y2, yrep2, discrete = TRUE))
  expect_gg(ppc_stat(y2, yrep2, stat = "prop0", discrete = TRUE))

  # ppd versions
  expect_gg(ppd_stat(yrep, stat = "q25", binwidth = 0.05))
  expect_gg(ppd_stat(yrep, stat = q25, binwidth = 0.05))
  expect_gg(ppd_stat(yrep2, stat = "prop0", binwidth = 0.05))
  expect_gg(ppd_stat(yrep2, stat = "prop0", discrete = TRUE))
})

test_that("ppc_stat_2d returns ggplot object", {
  expect_gg(ppc_stat_2d(y, yrep))
  expect_gg(ppc_stat_2d(y, yrep, stat = c("q25", "median")))
  expect_gg(ppc_stat_2d(y, yrep, stat = c("q25", median)))
  expect_gg(ppc_stat_2d(y, yrep, stat = c(function(x) mean(x), function(y) sd(y))))
  expect_gg(ppc_stat_2d(y2, yrep2))

  # ppd versions
  expect_gg(ppd_stat_2d(yrep, stat = c("q25", median)))
  expect_gg(ppd_stat_2d(yrep, stat = c(function(x) mean(x), function(y) sd(y))))
  expect_gg(ppd_stat_2d(yrep2))
})

test_that("ppc_stat_2d errors if more than 2 stats", {
  expect_error(ppc_stat_2d(y, yrep, stat = c("mean", "sd", "var")),
               "argument must have length 2")
})

test_that("ppc_stat_grouped returns ggplot object", {
  expect_gg(ppc_stat_grouped(y, yrep, group, binwidth = 0.05))
  expect_gg(ppc_stat_grouped(y, yrep, as.numeric(group), stat = function(z) var(z), binwidth = 0.05))
  expect_gg(ppc_stat_grouped(y, yrep, as.integer(group), stat = "sd", binwidth = 0.05))
  expect_gg(ppc_stat_grouped(y2, yrep2, group2, discrete = TRUE))

  # ppd version
  expect_gg(ppd_stat_grouped(yrep, group, binwidth = 0.05))
  expect_gg(ppd_stat_grouped(yrep2, group2, discrete = TRUE))
})

test_that("ppc_stat_freqpoly_grouped returns ggplot object", {
  expect_gg(ppc_stat_freqpoly_grouped(y, yrep, group, stat = "sd", freq = FALSE, binwidth = 0.05))
  expect_gg(ppc_stat_freqpoly_grouped(y, yrep, group, stat = function(x) sd(x), freq = TRUE, binwidth = 0.05))

  # ppd version
  expect_gg(ppd_stat_freqpoly_grouped(yrep, group, stat = "sd", freq = FALSE, binwidth = 0.05))
})

test_that("ppc_stat_data without the y values equal to ppd_stat_data", {
  d <- ppc_stat_data(y, yrep, group, stat = "median")
  d2 <- ppd_stat_data(yrep, group, stat = median)
  expect_equal(d$value[d$variable != "y"], d2$value)
  expect_equal(d$group[d$variable != "y"], d2$group)

  # with 2 stats
  d <- ppc_stat_data(y, yrep, group, stat = c(mean, median))
  d2 <- ppd_stat_data(yrep, group, stat = c("mean", "median"))
  expect_equal(d$value[d$variable != "y"], d2$value)
  expect_equal(d$value2[d$variable != "y"], d2$value2)
  expect_equal(d$group[d$variable != "y"], d2$group)
})

test_that("ppc_stat_data and ppd_stat_data throw correct errors", {
  expect_error(ppc_stat_data(y, yrep, stat = letters), "'stat' must have length 1 or 2")
  expect_error(ppd_stat_data(yrep, stat = letters), "'stat' must have length 1 or 2")
  expect_error(ppd_stat_data(yrep, stat = "not_a_known_function"),
               "object 'not_a_known_function' of mode 'function' was not found")
})


# Visual tests ------------------------------------------------------------

test_that("ppc_stat renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_stat(vdiff_y, vdiff_yrep, binwidth = 0.05) + yaxis_text()
  vdiffr::expect_doppelganger("ppc_stat (default)", p_base)

  p_custom <- ppc_stat(
    y = vdiff_y,
    yrep = vdiff_yrep,
    stat = "mad",
    binwidth = 0.05,
    freq = FALSE
  ) + yaxis_text()
  vdiffr::expect_doppelganger(
    title = "ppc_stat (stat, binwidth, freq)",
    fig = p_custom)

  p_discrete <- ppc_stat(
    y = vdiff_y2,
    yrep = vdiff_yrep2,
    stat = "prop0",
    discrete = TRUE
  ) + yaxis_text()
  vdiffr::expect_doppelganger(
    title = "ppc_stat (discrete, stat)",
    fig = p_discrete)

  # ppd versions
  p_base <- ppd_stat(vdiff_yrep, binwidth = 0.05) + yaxis_text()
  vdiffr::expect_doppelganger("ppd_stat (default)", p_base)

  p_custom <- ppd_stat(
    ypred = vdiff_yrep,
    stat = "mad",
    binwidth = 0.05,
    freq = FALSE
  ) + yaxis_text()
  vdiffr::expect_doppelganger(
    title = "ppd_stat (stat, binwidth, freq)",
    fig = p_custom)

  p_discrete <- ppd_stat(
    ypred = vdiff_yrep2,
    stat = "prop0",
    discrete = TRUE
  ) + yaxis_text()
  vdiffr::expect_doppelganger(
    title = "ppd_stat (discrete, stat)",
    fig = p_discrete)
})

test_that("ppc_stat_2d renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

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

  # ppd versions
  p_base <- ppd_stat_2d(vdiff_yrep)
  vdiffr::expect_doppelganger("ppd_stat_2d (default)", p_base)

  p_custom <- ppd_stat_2d(
    ypred = vdiff_yrep,
    stat = c("median", "mad"),
    size = 5,
    alpha = 1
  )
  vdiffr::expect_doppelganger(
    title = "ppd_stat_2d (stat, size, alpha)",
    fig = p_custom)
})

test_that("ppc_stat_grouped renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_stat_grouped(vdiff_y, vdiff_yrep, vdiff_group, binwidth = 0.05)
  vdiffr::expect_doppelganger("ppc_stat_grouped (default)", p_base)

  p_custom <- ppc_stat_grouped(
    y = vdiff_y,
    yrep = vdiff_yrep,
    group = vdiff_group,
    stat = stats::var,
    facet_args = list(scales = "fixed", ncol = 1),
    binwidth = 0.25
  )
  vdiffr::expect_doppelganger(
    title = "ppc_stat_grouped (stat, facet_args, binwidth)",
    fig = p_custom)

  p_discrete <- ppc_stat_grouped(
    y = vdiff_y2,
    yrep = vdiff_yrep2,
    group = vdiff_group2,
    stat = "prop0",
    discrete = TRUE
  )
  vdiffr::expect_doppelganger(
    title = "ppc_stat_grouped (discrete, stat)",
    fig = p_discrete)

  # ppd versions
  p_base <- ppd_stat_grouped(vdiff_yrep, vdiff_group, binwidth = 0.05)
  vdiffr::expect_doppelganger("ppd_stat_grouped (default)", p_base)

  p_custom <- ppd_stat_grouped(
    ypred = vdiff_yrep,
    group = vdiff_group,
    stat = stats::var,
    facet_args = list(scales = "fixed", ncol = 1),
    binwidth = 0.25
  )
  vdiffr::expect_doppelganger(
    title = "ppd_stat_grouped (stat, facet_args, binwidth)",
    fig = p_custom)

  p_discrete <- ppd_stat_grouped(
    ypred = vdiff_yrep2,
    group = vdiff_group2,
    stat = "prop0",
    discrete = TRUE
  )
  vdiffr::expect_doppelganger(
    title = "ppd_stat_grouped (discrete, stat)",
    fig = p_discrete)
})

test_that("ppc_stat_freqpoly_grouped renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_stat_freqpoly_grouped(vdiff_y, vdiff_yrep, vdiff_group, binwidth = 0.05)
  vdiffr::expect_doppelganger("ppc_stat_freqpoly_grouped (default)", p_base)

  p_custom <- ppc_stat_freqpoly_grouped(
    y = vdiff_y,
    yrep = vdiff_yrep,
    group = vdiff_group,
    stat = "sum",
    facet_args = list(scales = "fixed", ncol = 1),
    binwidth = 0.5
  )
  vdiffr::expect_doppelganger(
    title = "ppc_stat_freqpoly_grouped (stat, facets, bw)",
    fig = p_custom)

  # ppd versions
  p_base <- ppd_stat_freqpoly_grouped(vdiff_yrep, vdiff_group, binwidth = 0.05)
  vdiffr::expect_doppelganger("ppd_stat_freqpoly_grouped (default)", p_base)

  p_custom <- ppd_stat_freqpoly_grouped(
    ypred = vdiff_yrep,
    group = vdiff_group,
    stat = "sum",
    facet_args = list(scales = "fixed", ncol = 1),
    binwidth = 0.5
  )
  vdiffr::expect_doppelganger(
    title = "ppd_stat_freqpoly_grouped (stat, facets, bw)",
    fig = p_custom)
})
