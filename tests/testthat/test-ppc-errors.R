library(bayesplot)
context("PPC: predictive errors")

source(test_path("data-for-ppc-tests.R"))

test_that("ppc_error_hist and ppc_error_scatter return ggplot object", {
  expect_gg(ppc_error_hist(y, yrep[1:5, ]))
  expect_gg(ppc_error_scatter(y, yrep[1:5, ]))

  expect_gg(ppc_error_hist(y, yrep[1,, drop = FALSE]))
  expect_gg(ppc_error_scatter(y, yrep[1,, drop = FALSE]))

  expect_gg(ppc_error_hist(y2, yrep2))
  expect_gg(ppc_error_scatter(y2, yrep2))
})

test_that("ppc_error_hist_grouped returns ggplot object", {
  expect_gg(ppc_error_hist_grouped(y, yrep[1:5, ], group))
  expect_gg(ppc_error_hist_grouped(y, yrep[1,, drop = FALSE], group,
                                   freq = FALSE, binwidth = 1))
})

test_that("ppc_error_scatter_avg returns ggplot2 object", {
  expect_gg(ppc_error_scatter_avg(y, yrep))
  expect_gg(ppc_error_scatter_avg(y, yrep[1:5, ]))
})

test_that("ppc_error_scatter_avg same as ppc_error_scatter if nrow(yrep) = 1", {
  expect_equal(
    ppc_error_scatter_avg(y2, yrep2),
    ppc_error_scatter(y2, yrep2),
    check.environment = FALSE
  )
  expect_equal(
    ppc_error_scatter_avg(y, yrep[1,, drop=FALSE]),
    ppc_error_scatter(y, yrep[1,, drop = FALSE]),
    check.environment = FALSE
  )
})

test_that("ppc_error_scatter_avg_vs_x returns ggplot2 object", {
  expect_gg(ppc_error_scatter_avg_vs_x(y, yrep, x = rnorm(length(y))))
  expect_gg(ppc_error_scatter_avg_vs_x(y, yrep[1:5, ], x = rnorm(length(y))))
})

test_that("ppc_error_binned returns ggplot object", {
  load(test_path("data-for-binomial.rda"))
  expect_gg(ppc_error_binned(y, Ey))
  expect_gg(ppc_error_binned(y[1:5], Ey[, 1:5]))
  expect_gg(ppc_error_binned(rep(y, 2), cbind(Ey, Ey)))
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

test_that("ppc_error_binned renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

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

  vdiffr::expect_doppelganger(
    title = "ppc_error_binned (default)",
    fig = p_base
  )

})
