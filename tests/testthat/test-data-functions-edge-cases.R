source(test_path("data-for-ppc-tests.R"))
load(test_path("data-for-ordinal.rda"))

# ppc_bars_data ------------------------------------------------------------

test_that("ppc_bars_data handles single observation and single draw", {
  y1 <- 2L
  yrep1 <- matrix(c(1L, 2L, 3L, 2L, 2L), ncol = 1)
  d <- ppc_bars_data(y1, yrep1)
  expect_s3_class(d, "data.frame")
  expect_equal(d$y_obs[d$x == 2], 1)

  # single draw: interval collapses to a point
  y_s <- c(1L, 2L, 3L, 2L)
  yrep_s <- matrix(c(1L, 2L, 2L, 3L), nrow = 1)
  d2 <- ppc_bars_data(y_s, yrep_s)
  expect_equal(d2$l, d2$m, ignore_attr = TRUE)
  expect_equal(d2$m, d2$h, ignore_attr = TRUE)
})

test_that("ppc_bars_data prob = 0 collapses interval to median", {
  d <- ppc_bars_data(y_ord, yrep_ord, prob = 0)
  expect_equal(d$l, d$m, ignore_attr = TRUE)
  expect_equal(d$m, d$h, ignore_attr = TRUE)
})

test_that("ppc_bars_data errors on zero-length input", {
  expect_error(ppc_bars_data(integer(0), matrix(integer(0), nrow = 5, ncol = 0)))
})


# ppc_error_data -----------------------------------------------------------

test_that("ppc_error_data handles single observation", {
  skip_if_not_installed("rstantools")
  y1 <- 5
  yrep1 <- matrix(c(4, 6, 5), ncol = 1)
  d <- ppc_error_data(y1, yrep1)
  expect_equal(nrow(d), 3)
  expect_equal(d$value, y1 - yrep1[, 1])
  expect_true(all(d$y_obs == 5))
})

test_that("ppc_error_data returns zero-row data frame for zero-length input", {
  skip_if_not_installed("rstantools")
  d <- ppc_error_data(numeric(0), matrix(numeric(0), nrow = 1, ncol = 0))
  expect_equal(nrow(d), 0)
  expect_named(d, c("y_id", "y_name", "y_obs", "rep_id", "rep_label", "value"))
})


# ppc_scatter_data ---------------------------------------------------------

test_that("ppc_scatter_data handles single observation and single draw", {
  y1 <- 5
  yrep1 <- matrix(c(4, 6, 5), ncol = 1)
  d <- ppc_scatter_data(y1, yrep1)
  expect_equal(nrow(d), 3)
  expect_true(all(d$y_obs == 5))
  expect_equal(d$value, c(4, 6, 5))

  # single draw
  d2 <- ppc_scatter_data(y, yrep[1, , drop = FALSE])
  expect_equal(nrow(d2), length(y))
  expect_equal(d2$value, yrep[1, ])
  expect_equal(d2$y_obs, y)
})

test_that("ppc_scatter_data returns zero-row data frame for zero-length input", {
  d <- ppc_scatter_data(numeric(0), matrix(numeric(0), nrow = 1, ncol = 0))
  expect_equal(nrow(d), 0)
})


# ppc_scatter_avg_data -----------------------------------------------------

test_that("ppc_scatter_avg_data handles single observation", {
  y1 <- 5
  yrep1 <- matrix(c(4, 6, 5), ncol = 1)
  d <- ppc_scatter_avg_data(y1, yrep1)
  expect_equal(nrow(d), 1)
  expect_equal(d$value, mean(c(4, 6, 5)))
  expect_equal(d$y_obs, 5)
})

test_that("ppc_scatter_avg_data returns zero-row data frame for zero-length input", {
  d <- ppc_scatter_avg_data(numeric(0), matrix(numeric(0), nrow = 1, ncol = 0))
  expect_equal(nrow(d), 0)
})


# ppc_loo_pit_data ---------------------------------------------------------

test_that("ppc_loo_pit_data handles single pit value", {
  expect_message(
    d <- ppc_loo_pit_data(pit = 0.5, boundary_correction = FALSE, samples = 3),
    "pit"
  )
  y_rows <- d[d$is_y, ]
  expect_equal(nrow(y_rows), 1)
  expect_equal(y_rows$value, 0.5)
})

test_that("ppc_loo_pit_data works with custom bw parameter", {
  set.seed(42)
  pit_vals <- runif(50)
  expect_message(
    d <- ppc_loo_pit_data(
      pit = pit_vals,
      boundary_correction = TRUE,
      bw = "SJ",
      samples = 3,
      grid_len = 128
    ),
    "pit"
  )
  expect_true("x" %in% names(d))
})

test_that("ppc_loo_pit_data handles zero-length pit", {
  expect_message(
    d <- ppc_loo_pit_data(pit = numeric(0), boundary_correction = FALSE, samples = 2),
    "pit"
  )
  expect_equal(nrow(d), 0)
})

test_that("ppc_loo_pit_data is_y and is_y_label columns are consistent", {
  set.seed(42)
  pit_vals <- runif(10)
  expect_message(
    d <- ppc_loo_pit_data(pit = pit_vals, boundary_correction = FALSE, samples = 2),
    "pit"
  )
  expect_true(all(d$is_y_label[d$is_y] == levels(d$is_y_label)[1]))
  expect_true(all(d$is_y_label[!d$is_y] == levels(d$is_y_label)[2]))
})


# ppd_data -----------------------------------------------------------------

test_that("ppd_data handles single observation (single column)", {
  ypred <- matrix(c(1, 2, 3), ncol = 1)
  d <- ppd_data(ypred)
  expect_equal(nrow(d), 3)
  expect_true(all(d$y_id == 1))
  expect_equal(d$value, c(1, 2, 3))
})

test_that("ppd_data returns zero-row data frame for zero-length input", {
  d <- ppd_data(matrix(numeric(0), nrow = 1, ncol = 0))
  expect_equal(nrow(d), 0)
})


# ppd_stat_data ------------------------------------------------------------

test_that("ppd_stat_data handles single draw and single observation", {
  yrep_single <- matrix(rnorm(10), nrow = 1)
  d <- ppd_stat_data(yrep_single, stat = "mean")
  expect_equal(nrow(d), 1)

  yrep_1obs <- matrix(rnorm(5), ncol = 1)
  d2 <- ppd_stat_data(yrep_1obs, stat = "mean")
  expect_s3_class(d2, "data.frame")
})

test_that("ppd_stat_data errors on zero-length input", {
  expect_error(ppd_stat_data(matrix(numeric(0), nrow = 1, ncol = 0), stat = "mean"))
})


# ppd_intervals_data -------------------------------------------------------

test_that("ppd_intervals_data handles single observation and single draw", {
  yrep_1obs <- matrix(rnorm(25), ncol = 1)
  d <- ppd_intervals_data(yrep_1obs)
  expect_equal(nrow(d), 1)
  expect_true(d$ll <= d$l && d$l <= d$m && d$m <= d$h && d$h <= d$hh)

  # single draw: all quantiles collapse to the value
  yrep_1draw <- matrix(rnorm(10), nrow = 1)
  d2 <- ppd_intervals_data(yrep_1draw)
  expect_equal(d2$ll, d2$m)
  expect_equal(d2$hh, d2$m)
})

test_that("ppd_intervals_data uses custom x values", {
  x_vals <- seq(10, 100, length.out = ncol(yrep))
  d <- ppd_intervals_data(yrep, x = x_vals)
  expect_equal(d$x, x_vals)
})

test_that("ppd_intervals_data returns zero-row data frame for zero-length input", {
  d <- ppd_intervals_data(matrix(numeric(0), nrow = 1, ncol = 0))
  expect_equal(nrow(d), 0)
})
