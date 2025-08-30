library(bayesplot)
context("PPC: loo")

options(useFancyQuotes = FALSE)

if (requireNamespace("rstanarm", quietly = TRUE) &&
    requireNamespace("loo", quietly = TRUE)) {
  suppressPackageStartupMessages(library(rstanarm))
  suppressPackageStartupMessages(library(loo))
  ITER <- 1000
  CHAINS <- 3
  fit <- stan_glm(mpg ~ wt + am, data = mtcars,
                  iter = ITER, chains = CHAINS,
                  refresh = 0)
  y <- fit$y
  yrep <- posterior_predict(fit)
  suppressWarnings(
    psis1 <- psis(-log_lik(fit), cores = 2)
  )
  lw <- weights(psis1)
  suppressWarnings(
    pits <- rstantools::loo_pit(yrep, y, lw)
  )
}


test_that("ppc_loo_pit gives deprecation warning but still works", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_warning(p1 <- ppc_loo_pit(y, yrep, lw), "deprecated")
  expect_gg(p1)
})

test_that("ppc_loo_pit_overlay returns ggplot object", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_gg(ppc_loo_pit_overlay(y, yrep, lw, samples = 25))
  expect_gg(ppc_loo_pit_overlay(y, yrep, psis_object = psis1, samples = 25))
})

test_that("ppc_loo_pit_overlay warns about binary data", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_warning(
    ppc_loo_pit_overlay(rep(1, length(y)), yrep, lw),
    "not recommended for binary data"
  )
})

test_that("ppc_loo_pit_overlay works with boundary_correction=TRUE", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_message(p1 <- ppc_loo_pit_overlay(y, yrep, lw, boundary_correction = TRUE),
                 "continuous observations")
  expect_gg(p1)
})

test_that("ppc_loo_pit_overlay works with boundary_correction=FALSE", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  p1 <- ppc_loo_pit_overlay(y, yrep, lw, boundary_correction = FALSE)
  expect_gg(p1)
})

test_that("ppc_loo_pit_qq returns ggplot object", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_gg(p1 <- ppc_loo_pit_qq(y, yrep, lw))
  expect_gg(p2 <- ppc_loo_pit_qq(y, yrep, psis_object = psis1))
  if ("get_labs" %in% getNamespaceExports("ggplot2")) {
    ll1 <- ggplot2::get_labs(p1)
  } else {
    ll1 <- p1$labels
  }
  expect_equal(ll1$x, "Uniform")
  expect_equal(p1$data, p2$data)
  expect_gg(p3 <- ppc_loo_pit_qq(y, yrep, lw, compare = "normal"))
  if ("get_labs" %in% getNamespaceExports("ggplot2")) {
    ll3 <- ggplot2::get_labs(p3)
  } else {
    ll3 <- p3$labels
  }
  expect_equal(ll3$x, "Normal")
})

test_that("ppc_loo_pit_ecdf returns a ggplot object", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_gg(p0 <- ppc_loo_pit_ecdf(y, yrep, lw))
  expect_gg(p1 <- ppc_loo_pit_ecdf(y, yrep, lw))
  expect_gg(p2 <- ppc_loo_pit_ecdf(y, yrep, psis_object = psis1))
  if ("get_labs" %in% getNamespaceExports("ggplot2")) {
    ll1 <- ggplot2::get_labs(p1)
  } else {
    ll1 <- p1$labels
  }
  expect_equal(ll1$x, "LOO PIT")
  expect_equal(ll1$y, "ECDF")
  expect_equal(p1$data, p2$data)
  expect_gg(p3 <- ppc_loo_pit_ecdf(y, yrep, lw, plot_diff = TRUE))
  if ("get_labs" %in% getNamespaceExports("ggplot2")) {
    ll3 <- ggplot2::get_labs(p3)
  } else {
    ll3 <- p3$labels
  }
  expect_equal(ll3$y, "ECDF difference")
})

test_that("ppc_loo_pit functions work when pit specified instead of y, yrep, and lw", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_gg(ppc_loo_pit_qq(pit = pits))
  # ppc_loo_pit_qq
  expect_message(
    p1 <- ppc_loo_pit_qq(y = y, yrep = yrep, lw = lw, pit = pits),
    "'pit' specified so ignoring 'y','yrep','lw' if specified"
  )
  expect_message(
    p2 <- ppc_loo_pit_qq(pit = pits)
  )
  expect_equal(p1$data, p2$data)

  # ppc_loo_pit_ecdf
  expect_gg(ppc_loo_pit_ecdf(pit = rep(pits, 4)))
  expect_message(
    p1 <- ppc_loo_pit_ecdf(y = y, yrep = yrep, lw = lw, pit = rep(pits, 4)),
    "'pit' specified so ignoring 'y','yrep','lw' if specified"
  )
  expect_message(
    p2 <- ppc_loo_pit_ecdf(pit = rep(pits, 4))
  )
  expect_equal(p1$data, p2$data)

  # ppc_loo_pit_overlay
  expect_gg(p1 <- ppc_loo_pit_overlay(pit = pits))
  expect_message(
    ppc_loo_pit_overlay(y = y, yrep = yrep, lw = lw, pit = pits),
    "'pit' specified so ignoring 'y','yrep','lw' if specified"
  )
})


test_that("ppc_loo_intervals returns ggplot object", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_gg(ppc_loo_intervals(y, yrep, psis_object = psis1))
  expect_gg(g <- ppc_loo_intervals(y, yrep, psis_object = psis1, order = "median"))
  expect_s3_class(g$data$x, "factor")
  expect_equal(nlevels(g$data$x), length(g$data$x))

  # subset argument
  expect_gg(g <- ppc_loo_intervals(y, yrep, psis_object = psis1, subset = 1:25))
  expect_equal(nrow(g$data), 25)
})

test_that("ppc_loo_ribbon returns ggplot object", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_gg(ppc_loo_ribbon(y, yrep, psis_object = psis1, prob = 0.7, alpha = 0.1))
  expect_gg(g <- ppc_loo_ribbon(y, yrep, psis_object = psis1, subset = 1:25))
  expect_equal(nrow(g$data), 25)
})

test_that("ppc_loo_intervals/ribbon work when 'intervals' specified", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  intervals <- t(apply(yrep, 2, quantile, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)))
  expect_gg(ppc_loo_intervals(y, intervals = intervals))
  expect_gg(ppc_loo_ribbon(y, intervals = intervals))
  expect_message(ppc_loo_ribbon(y, intervals = intervals),
                 "'intervals' specified so ignoring 'yrep', 'psis_object', 'subset', if specified")
  expect_message(ppc_loo_intervals(y, yrep, psis_object = psis1, intervals = intervals),
                 "'intervals' specified so ignoring 'yrep', 'psis_object', 'subset', if specified")
})

test_that("ppc_loo_intervals/ribbon work when 'intervals' has 3 columns", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  intervals <- t(apply(yrep, 2, quantile, probs = c(0.1, 0.5, 0.9)))
  expect_gg(ppc_loo_intervals(y, intervals = intervals))
  expect_gg(ppc_loo_ribbon(y, intervals = intervals))
})

test_that("errors if dimensions of yrep and lw don't match", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_error(
    ppc_loo_pit_overlay(y, yrep, lw[, 1:5]),
    "identical(dim(yrep), dim(lw)) is not TRUE",
    fixed = TRUE
  )
})

test_that("error if subset is bigger than num obs", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_error(.psis_subset(psis1, 1:1000), "too many elements")
  expect_error(
    ppc_loo_intervals(y, yrep, psis_object = psis1, subset = 1:1000),
    "length(y) >= length(subset) is not TRUE",
    fixed = TRUE
  )
})


# Visual tests ------------------------------------------------------------

source(test_path("data-for-ppc-tests.R"))
set.seed(123)

test_that("ppc_loo_pit_overlay renders correctly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("loo")
  skip_on_r_oldrel()
  skip_if(packageVersion("rstantools") > "2.4.0")

  p_base <- suppressMessages(ppc_loo_pit_overlay(vdiff_loo_y, vdiff_loo_yrep, vdiff_loo_lw))
  vdiffr::expect_doppelganger("ppc_loo_pit_overlay (default)", p_base)

  p_custom <- suppressMessages(ppc_loo_pit_overlay(
    vdiff_loo_y,
    vdiff_loo_yrep,
    vdiff_loo_lw,
    boundary_correction = FALSE
  ))
  vdiffr::expect_doppelganger("ppc_loo_pit_overlay (boundary)", p_custom)
})

test_that("ppc_loo_pit_qq renders correctly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("loo")
  skip_on_r_oldrel()
  skip_if(packageVersion("rstantools") > "2.4.0")

  p_base <- ppc_loo_pit_qq(vdiff_loo_y, vdiff_loo_yrep, vdiff_loo_lw)
  vdiffr::expect_doppelganger("ppc_loo_pit_qq (default)", p_base)
})

test_that("ppc_loo_intervals renders correctly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("loo")
  skip_on_r_oldrel()

  psis_object <- suppressWarnings(loo::psis(-vdiff_loo_lw))
  p_base <- ppc_loo_intervals(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object
  )
  vdiffr::expect_doppelganger("ppc_loo_intervals (default)", p_base)

  p_custom <- ppc_loo_intervals(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object,
    prob = 0.6,
    prob_outer = 0.7
  )
  vdiffr::expect_doppelganger("ppc_loo_intervals (prob)", p_custom)

  p_custom <- ppc_loo_intervals(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object,
    order = "median"
  )
  vdiffr::expect_doppelganger("ppc_loo_intervals (order)", p_custom)
})

test_that("ppc_loo_ribbon renders correctly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("loo")
  skip_on_r_oldrel()

  psis_object <- suppressWarnings(loo::psis(-vdiff_loo_lw))
  p_base <- ppc_loo_ribbon(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object
  )
  vdiffr::expect_doppelganger("ppc_loo_ribbon (default)", p_base)

  p_custom <- ppc_loo_ribbon(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object,
    prob = 0.6,
    prob_outer = 0.7
  )
  vdiffr::expect_doppelganger("ppc_loo_ribbon (prob)", p_custom)

  p_custom <- ppc_loo_ribbon(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object,
    subset = 1:10
  )
  vdiffr::expect_doppelganger("ppc_loo_ribbon (subset)", p_custom)
})

test_that("ppc_loo_pit_ecdf renders correctly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("loo")
  skip_on_r_oldrel()
  skip_if(packageVersion("rstantools") <= "2.4.0")

  psis_object <- suppressWarnings(loo::psis(-vdiff_loo_lw))
  p_base <- ppc_loo_pit_ecdf(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object
  )
  vdiffr::expect_doppelganger("ppc_loo_pit_ecdf (default)", p_base)

  p_custom <- ppc_loo_pit_ecdf(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object,
    K = 50
  )
  vdiffr::expect_doppelganger("ppc_loo_pit_ecdf (K)", p_custom)

  p_custom <- ppc_loo_pit_ecdf(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object,
    prob = 0.95
  )
  vdiffr::expect_doppelganger("ppc_loo_pit_ecdf (prob)", p_custom)

  p_custom <- ppc_loo_pit_ecdf(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object,
    plot_diff = TRUE,
    K = 100
  )
  vdiffr::expect_doppelganger("ppc_loo_pit_ecdf (ecdf difference)", p_custom)
})
