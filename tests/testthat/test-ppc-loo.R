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

test_that("ppc_loo_pit_ecdf with method='correlated' returns ggplot object", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")

  x <- 1 - (1 - runif(300))^(1.2)
  ppc_loo_pit_ecdf(pit=x, method = "correlated", prob = 0.95, plot_diff = TRUE)

  # Test with POT-C (default)
  expect_gg(p1 <- ppc_loo_pit_ecdf(y, yrep, lw, method = "correlated"))
  expect_gg(p2 <- ppc_loo_pit_ecdf(y, yrep, lw, method = "correlated", test = "POT"))
  
  # Test with PRIT-C
  expect_gg(p3 <- ppc_loo_pit_ecdf(y, yrep, lw, method = "correlated", test = "PRIT"))
  
  # Test with PIET-C
  expect_gg(p4 <- ppc_loo_pit_ecdf(y, yrep, lw, method = "correlated", test = "PIET"))
  
  # Test with plot_diff = TRUE
  expect_gg(p5 <- ppc_loo_pit_ecdf(y, yrep, lw, method = "correlated", plot_diff = TRUE))
  
  # Test with infl_points_only = TRUE
  expect_gg(p6 <- ppc_loo_pit_ecdf(y, yrep, lw, method = "correlated", infl_points_only = TRUE))
  
  # Test with gamma specified
  expect_gg(p7 <- ppc_loo_pit_ecdf(y, yrep, lw, method = "correlated", gamma = 0.1))
})

test_that("ppc_loo_pit_ecdf method argument works correctly", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  
  # Test default (should inform about upcoming change)
  expect_message(
    p1 <- ppc_loo_pit_ecdf(y, yrep, lw),
    "In the next major release"
  )
  expect_gg(p1)
  
  # Test explicit independent method (should inform about supersession)
  expect_message(
    p2 <- ppc_loo_pit_ecdf(y, yrep, lw, method = "independent"),
    "superseded by the 'correlated' method"
  )
  expect_gg(p2)
  
  # Test correlated method (no message expected)
  expect_gg(p3 <- ppc_loo_pit_ecdf(y, yrep, lw, method = "correlated"))
  
  # Test that independent and correlated produce different plots
  expect_true(!identical(p2$data, p3$data) || !identical(p2$layers, p3$layers))
})

# Tests for the dependence-aware uniformity tests ------------------------------
test_that("piet_test computes correct p-values", {
  x <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  # Equivalent to 2 * min(1-x, x)
  expected <- c(0.2, 0.5, 1.0, 0.5, 0.2)
  
  expect_equal(piet_test(x), expected, tolerance = 1e-7)
})

test_that("piet_test handles boundary values of 0 and 1", {
  x <- c(0, 1)
  expected <- c(0, 0)
  
  expect_equal(piet_test(x), expected)
})

test_that("piet_test handles extreme values stably", {
  # Testing values very close to 0 and 1
  x <- c(1e-17, 1 - 1e-17)
  expected <- c(2e-17, 2e-17)
  
  # Tolerance needs to be adjusted for very small numbers
  expect_equal(piet_test(x), expected, tolerance = 1e-16)
})

test_that("piet_test handles NA, NaN, and empty inputs correctly", {
  # NA and NaN propagation
  x_na <- c(0.5, NA, NaN)
  res_na <- piet_test(x_na)
  
  expect_equal(res_na[1], 1.0)
  expect_true(is.na(res_na[2]))
  expect_true(is.nan(res_na[3]))
})

test_that("pot_test calculates correct p-values", {
  # Manually calculating expected values for x = c(0.2, 0.8), n = 2. 
  # Note: If X ~ Beta(1, b) then X ~ Kumaraswamy(1, b) with CDF 1 - (1 - x)^b 
  # and if X ~ Beta(a, 1) then X ~ Kumaraswamy(a, 1) with CDF to x^a
  # Beta(1, 2) CDF at 0.2 is 1 - (1 - 0.2)^2 = 0.36
  # Beta(2, 1) CDF at 0.8 is 0.8^2 = 0.64
  # p-values: 2 * min(0.36, 1-0.36) = 0.72; 2 * min(0.64, 1-0.64) = 0.72
  x <- c(0.8, 0.2)
  expected <- c(0.72, 0.72)
  
  expect_equal(pot_test(x), expected)
})

test_that("pot_test handles boundary values correctly", {
  x <- c(0, 1)
  expected <- c(0, 0)
  
  expect_equal(pot_test(x), expected)
})

test_that("pot_test bounds p-values between 0 and 1 for extreme out-of-bounds inputs", {
  # pbeta handles values outside [0, 1] by returning 0
  x <- c(-0.5, 1.5)
  expected <- c(0, 0)
  
  expect_equal(pot_test(x), expected)
})

test_that("pot_test handles NAs", {
  x_na <- c(0.5, NA) # resulting in a = 2, b = 1
  # Beta(2, 1) at 0.5 = 0.25 -> 2 * min(0.25, 0.75) = 0.5
  expected <- c(0.5, NA)

  expect_equal(pot_test(x_na), expected)
})

test_that("prit_test computes correct p-values", {
  # Let n = 2, x = c(0.5, 0.5) 
  # scaled_ecdf = 2 * c(1, 1) = c(2, 2)
  # probs1 = pbinom(1, 2, 0.5) = 0.75
  # probs2 = pbinom(2, 2, 0.5) = 1.00
  # p_val = 2 * min(1 - 0.75, 1.00) = 2 * 0.25 = 0.5
  
  x <- c(0.5, 0.5)
  expect_equal(prit_test(x), c(0.5, 0.5))
})

test_that("helper functions for dependence-aware tests work correctly", {
  # Test Cauchy space transformation
  x <- runif(100)
  
  cauchy_piet <- bayesplot:::cauchy_space(x)
  expect_true(is.numeric(cauchy_piet))
  expect_true(length(cauchy_piet) == length(x))
  expect_true(all(is.finite(cauchy_piet)))
  
  # Test Shapley values
  sh_val <- bayesplot:::shapley_mean_closedform(cauchy_pot)
  expect_true(is.numeric(sh_val))
  expect_true(length(sh_val) == length(cauchy_pot))
  
  # Test influential points
  infl_idx <- bayesplot:::influential_points_idx(sh_val, alpha = 0.05)
  expect_true(is.integer(infl_idx) || is.numeric(infl_idx))
  expect_true(all(infl_idx >= 1 & infl_idx <= length(sh_val)))
  
  # Test Cauchy aggregation
  pvals <- runif(50, 0, 0.5)
  agg_pval <- bayesplot:::cauchy_agg(pvals, truncate = FALSE)
  expect_true(is.numeric(agg_pval))
  expect_true(agg_pval >= 0 && agg_pval <= 1)
  
  # Test truncated Cauchy aggregation
  t_agg_pval <- bayesplot:::cauchy_agg(pvals, truncate = TRUE)
  expect_true(is.numeric(t_agg_pval))
  expect_true(t_agg_pval >= 0 && t_agg_pval <= 1)
})

test_that("ppc_loo_pit_ecdf correlated method handles edge cases", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  
  # Test with small sample
  small_pit <- runif(10)
  expect_gg(p1 <- ppc_loo_pit_ecdf(pit = small_pit, method = "correlated"))
  
  # Test with perfect uniform
  uniform_pit <- seq(0, 1, length.out = 100)
  expect_gg(p2 <- ppc_loo_pit_ecdf(pit = uniform_pit, method = "correlated"))
  
  # Test with extreme values
  extreme_pit <- c(rep(0, 10), rep(1, 10), runif(80))
  expect_gg(p3 <- ppc_loo_pit_ecdf(pit = extreme_pit, method = "correlated"))
  
  # Test with single value (edge case)
  single_pit <- 0.5
  expect_gg(p4 <- ppc_loo_pit_ecdf(pit = single_pit, method = "correlated"))
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
  skip_if(packageVersion("rstantools") <= "2.4.0")

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
  skip_if(packageVersion("rstantools") <= "2.4.0")

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
