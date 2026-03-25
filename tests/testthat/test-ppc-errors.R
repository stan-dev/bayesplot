source(test_path("data-for-ppc-tests.R"))

test_that("ppc_error_hist and ppc_error_scatter return ggplot object", {
  skip_if_not_installed("rstantools")
  expect_gg(ppc_error_hist(y, yrep[1:5, ], binwidth = 0.1))
  expect_gg(ppc_error_scatter(y, yrep[1:5, ]))

  expect_gg(ppc_error_hist(y, yrep[1,, drop = FALSE], binwidth = 0.1))
  expect_gg(ppc_error_scatter(y, yrep[1,, drop = FALSE]))

  expect_gg(ppc_error_hist(y2, yrep2, binwidth = 0.1))
  expect_gg(ppc_error_scatter(y2, yrep2))
})

test_that("ppc_error_hist_grouped returns ggplot object", {
  skip_if_not_installed("rstantools")
  expect_gg(ppc_error_hist_grouped(y, yrep[1:5, ], group, binwidth = 0.1))
  expect_gg(ppc_error_hist_grouped(y, yrep[1,, drop = FALSE], group,
                                   freq = FALSE, binwidth = 1))
})

test_that("ppc_error_scatter_avg returns ggplot2 object", {
  skip_if_not_installed("rstantools")
  expect_gg(ppc_error_scatter_avg(y, yrep))
  expect_gg(ppc_error_scatter_avg(y, yrep[1:5, ]))

  # when x is provided
  expect_gg(ppc_error_scatter_avg(y, yrep, x = rnorm(length(y))))
  expect_gg(ppc_error_scatter_avg(y, yrep[1:5, ], x = rnorm(length(y))))
})

test_that("ppc_error_scatter_avg same as ppc_error_scatter if nrow(yrep) = 1", {
  skip_if_not_installed("rstantools")
  p1 <- ppc_error_scatter_avg(y2, yrep2)
  p2 <- ppc_error_scatter(y2, yrep2)
  d1 <- p1$data
  d2 <- p2$data

  # really only a few columns are _exactly_ the same
  cols <- c("y_id", "y_obs", "value")
  expect_equal(d1[, cols], d2[, cols])
})

test_that("ppc_error_scatter_avg_vs_x returns ggplot2 object", {
  skip_if_not_installed("rstantools")

  # expect warning
  expect_warning(expect_gg(ppc_error_scatter_avg_vs_x(y, yrep, x = rnorm(length(y)))),
                 "'ppc_error_scatter_avg_vs_x' is deprecated.")
  expect_warning(expect_gg(ppc_error_scatter_avg_vs_x(y, yrep[1:5, ], x = rnorm(length(y)))),
                 "'ppc_error_scatter_avg_vs_x' is deprecated.")
})

test_that("ppc_error_binned returns ggplot object", {
  skip_if_not_installed("rstantools")
  load(test_path("data-for-binomial.rda"))
  expect_gg(ppc_error_binned(y, Ey))
  expect_gg(ppc_error_binned(y[1:5], Ey[, 1:5]))
  expect_gg(ppc_error_binned(rep(y, 2), cbind(Ey, Ey)))
  expect_gg(ppc_error_binned(y, Ey, x = x))
  expect_gg(ppc_error_binned(rep(y, 2), cbind(Ey, Ey), x = rep(x, 2)))
})

test_that("bin_errors works for edge cases", {
  ans <-
    data.frame(
      ey_bar = c(1, NaN),
      err_bar = c(0, NaN),
      se2 = c(0, NaN),
      bin = c(1, 2)
    )
  val <- bin_errors(rep(1, 10), rep(0, 10), bins = 1)
  expect_equal(ans, val)
})


# Visual tests -----------------------------------------------------------------

test_that("ppc_error_hist renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_error_hist(vdiff_y, vdiff_yrep[1:3, ])
  vdiffr::expect_doppelganger("ppc_error_hist (default)", p_base)
})

test_that("ppc_error_hist_grouped renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_error_hist_grouped(vdiff_y, vdiff_yrep[1:3, ], vdiff_group)
  vdiffr::expect_doppelganger("ppc_error_hist_grouped (default)", p_base)
})

test_that("ppc_error_scatter renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_error_scatter(vdiff_y, vdiff_yrep[1:3, ])
  vdiffr::expect_doppelganger("ppc_error_scatter (default)", p_base)
})

test_that("ppc_error_scatter_avg renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_error_scatter_avg(vdiff_y, vdiff_yrep)
  vdiffr::expect_doppelganger("ppc_error_scatter_avg (default)", p_base)

  p_base_x <- ppc_error_scatter_avg(vdiff_y, vdiff_yrep, x = seq_along(vdiff_y))
  vdiffr::expect_doppelganger("ppc_error_scatter_avg (with x)", p_base_x)
})

test_that("ppc_error_scatter_avg_grouped renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_error_scatter_avg_grouped(vdiff_y, vdiff_yrep, vdiff_group)
  vdiffr::expect_doppelganger("ppc_error_scatter_avg_grouped (default)", p_base)
})

test_that("ppc_error_scatter_avg_vs_x renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  # expect warning
  expect_warning(
    p_base <- ppc_error_scatter_avg_vs_x(vdiff_y, vdiff_yrep, x = seq_along(vdiff_y)),
    "'ppc_error_scatter_avg_vs_x' is deprecated."
  )
  vdiffr::expect_doppelganger("ppc_error_scatter_avg_vs_x (default)", p_base)

})

test_that("ppc_error_binned renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  rbeta2 <- function(n, mu, phi) {
    a <- mu * phi
    b <- (1 - mu) * phi
    rbeta(n, a, b)
  }

  set.seed(100)
  y <- rbeta(50, shape1 = 1, shape2 = 10)

  four_draws <- structure(
    c(-2.118, -2.061, -2.069, -2.011, 7.604, 9.720, 9.7186, 10.1888),
    .Dim = c(4L, 2L),
    .Dimnames = list(
      iterations = NULL,
      parameters = c("(Intercept)", "(phi)")
    )
  )

  y_rep <- t(apply(four_draws, 1, function(x) rbeta2(50, plogis(x[1]), x[2])))

  p_base <- ppc_error_binned(y, y_rep)
  vdiffr::expect_doppelganger("ppc_error_binned (default)", p_base)

  x <- rnorm(length(y), mean = 5)
  p_base_x <- ppc_error_binned(y, y_rep, x = x)
  vdiffr::expect_doppelganger("ppc_error_binned (with x)", p_base_x)
})


# ppc_error_data tests -----------------------------------------------------

source(test_path("data-for-ppc-tests.R"))

test_that("ppc_error_data returns correct structure", {
  skip_if_not_installed("rstantools")
  d <- ppc_error_data(y, yrep)
  expect_s3_class(d, "data.frame")
  expect_true(all(c("y_id", "y_obs", "rep_id", "rep_label", "value") %in% names(d)))
})

test_that("ppc_error_data computes errors correctly", {
  skip_if_not_installed("rstantools")
  d <- ppc_error_data(y, yrep)
  # errors should be y - yrep
  first_rep <- d[d$rep_id == 1, ]
  expected_errors <- y - yrep[1, ]
  expect_equal(first_rep$value, expected_errors)
  expect_equal(first_rep$y_obs, y)
})

test_that("ppc_error_data returns correct number of rows", {
  skip_if_not_installed("rstantools")
  d <- ppc_error_data(y, yrep)
  expect_equal(nrow(d), length(y) * nrow(yrep))
})

test_that("ppc_error_data with group adds group column", {
  skip_if_not_installed("rstantools")
  d <- ppc_error_data(y, yrep, group = group)
  expect_true("group" %in% names(d))
  expect_equal(nlevels(factor(d$group)), nlevels(group))
})

test_that("ppc_error_data works with single replicate", {
  skip_if_not_installed("rstantools")
  d <- ppc_error_data(y, yrep[1, , drop = FALSE])
  expect_equal(nrow(d), length(y))
})
