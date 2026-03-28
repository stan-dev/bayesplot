source(test_path("data-for-ppc-tests.R"))
load(test_path("data-for-ordinal.rda"))

# ppc_bars_data edge cases -------------------------------------------------

test_that("ppc_bars_data returns correct structure and types", {
  d <- ppc_bars_data(y_ord, yrep_ord)
  expect_s3_class(d, "data.frame")
  expect_named(d, c("x", "y_obs", "l", "m", "h"))
  expect_type(d$x, "double")
  expect_type(d$y_obs, "integer")
  expect_type(d$l, "double")
  expect_type(d$m, "double")
  expect_type(d$h, "double")
})

test_that("ppc_bars_data works with single observation", {
  y1 <- 2L
  yrep1 <- matrix(c(1L, 2L, 3L, 2L, 2L), ncol = 1)
  d <- ppc_bars_data(y1, yrep1)
  expect_s3_class(d, "data.frame")
  expect_true(all(c("x", "y_obs") %in% names(d)))
  expect_equal(d$y_obs[d$x == 2], 1)
})

test_that("ppc_bars_data works with single draw", {
  y_single <- c(1L, 2L, 3L, 2L)
  yrep_single <- matrix(c(1L, 2L, 2L, 3L), nrow = 1)
  d <- ppc_bars_data(y_single, yrep_single)
  expect_s3_class(d, "data.frame")
  # with single draw, l == m == h
  expect_equal(d$l, d$m, ignore_attr = TRUE)
  expect_equal(d$m, d$h, ignore_attr = TRUE)
})

test_that("ppc_bars_data freq = FALSE returns proportions", {
  d <- ppc_bars_data(y_ord, yrep_ord, freq = FALSE)
  expect_true(all(d$y_obs <= 1))
  expect_true(all(d$y_obs >= 0))
})

test_that("ppc_bars_data with group adds group column", {
  d <- ppc_bars_data(y_ord, yrep_ord, group = group_ord)
  expect_true("group" %in% names(d))
  expect_s3_class(d$group, "factor")
})

test_that("ppc_bars_data prob = 0 collapses interval to median", {
  d <- ppc_bars_data(y_ord, yrep_ord, prob = 0)
  expect_equal(d$l, d$m, ignore_attr = TRUE)
  expect_equal(d$m, d$h, ignore_attr = TRUE)
})

test_that("ppc_bars_data errors on NA in y", {
  y_na <- y_ord
  y_na[1] <- NA
  expect_error(ppc_bars_data(y_na, yrep_ord))
})

test_that("ppc_bars_data errors on NA in yrep", {
  yrep_na <- yrep_ord
  yrep_na[1, 1] <- NA
  expect_error(ppc_bars_data(y_ord, yrep_na))
})

test_that("ppc_bars_data errors on non-discrete inputs", {
  expect_error(ppc_bars_data(y_ord + 0.5, yrep_ord),
               "ppc_bars expects 'y' to be discrete")
  expect_error(ppc_bars_data(y_ord, yrep_ord + 0.5),
               "ppc_bars expects 'yrep' to be discrete")
})

test_that("ppc_bars_data errors on zero-length input", {
  expect_error(ppc_bars_data(integer(0), matrix(integer(0), nrow = 5, ncol = 0)))
})


# ppc_error_data edge cases ------------------------------------------------

test_that("ppc_error_data works with single observation", {
  skip_if_not_installed("rstantools")
  y1 <- 5
  yrep1 <- matrix(c(4, 6, 5), ncol = 1)
  d <- ppc_error_data(y1, yrep1)
  expect_named(d, c("y_id", "y_name", "y_obs", "rep_id", "rep_label", "value"))
  expect_equal(nrow(d), 3)
  expect_equal(d$value, y1 - yrep1[, 1])
  expect_true(all(d$y_obs == 5))
})

test_that("ppc_error_data works with single draw", {
  skip_if_not_installed("rstantools")
  d <- ppc_error_data(y, yrep[1, , drop = FALSE])
  expect_equal(nrow(d), length(y))
  expect_true(all(d$rep_id == 1))
  expect_equal(d$value, y - yrep[1, ])
})

test_that("ppc_error_data preserves observation names", {
  skip_if_not_installed("rstantools")
  y_named <- c(a = 1, b = 2, c = 3)
  yrep_named <- matrix(c(1.1, 2.1, 3.1), ncol = 3)
  colnames(yrep_named) <- c("a", "b", "c")
  d <- ppc_error_data(y_named, yrep_named)
  expect_equal(as.character(d$y_name), c("a", "b", "c"))
})

test_that("ppc_error_data errors on NA in y", {
  skip_if_not_installed("rstantools")
  y_na <- y
  y_na[1] <- NA
  expect_error(ppc_error_data(y_na, yrep))
})

test_that("ppc_error_data errors on NA in yrep", {
  skip_if_not_installed("rstantools")
  yrep_na <- yrep
  yrep_na[1, 1] <- NA
  expect_error(ppc_error_data(y, yrep_na))
})

test_that("ppc_error_data errors on dimension mismatch", {
  skip_if_not_installed("rstantools")
  expect_error(ppc_error_data(y, yrep[, 1:5]),
               "ncol(yrep) must be equal to length(y)", fixed = TRUE)
})

test_that("ppc_error_data returns zero-row data frame for zero-length input", {
  skip_if_not_installed("rstantools")
  d <- ppc_error_data(numeric(0), matrix(numeric(0), nrow = 1, ncol = 0))
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 0)
  expect_named(d, c("y_id", "y_name", "y_obs", "rep_id", "rep_label", "value"))
})


# ppc_scatter_data edge cases ----------------------------------------------

test_that("ppc_scatter_data returns correct structure", {
  d <- ppc_scatter_data(y, yrep)
  expect_s3_class(d, "data.frame")
  expect_named(d, c("y_id", "y_name", "y_obs", "rep_id", "rep_label", "value"))
  expect_equal(nrow(d), length(y) * nrow(yrep))
})

test_that("ppc_scatter_data works with single observation", {
  y1 <- 5
  yrep1 <- matrix(c(4, 6, 5), ncol = 1)
  d <- ppc_scatter_data(y1, yrep1)
  expect_equal(nrow(d), 3)
  expect_true(all(d$y_obs == 5))
  expect_equal(d$value, c(4, 6, 5))
})

test_that("ppc_scatter_data works with single draw", {
  d <- ppc_scatter_data(y, yrep[1, , drop = FALSE])
  expect_equal(nrow(d), length(y))
  expect_true(all(d$rep_id == 1))
  expect_equal(d$value, yrep[1, ])
  expect_equal(d$y_obs, y)
})

test_that("ppc_scatter_data preserves observation names", {
  y_named <- c(a = 1, b = 2)
  yrep_named <- matrix(c(1.1, 2.1), ncol = 2)
  colnames(yrep_named) <- c("a", "b")
  d <- ppc_scatter_data(y_named, yrep_named)
  expect_equal(as.character(d$y_name), c("a", "b"))
})

test_that("ppc_scatter_data errors on NA in y", {
  y_na <- y
  y_na[1] <- NA
  expect_error(ppc_scatter_data(y_na, yrep))
})

test_that("ppc_scatter_data errors on NA in yrep", {
  yrep_na <- yrep
  yrep_na[1, 1] <- NA
  expect_error(ppc_scatter_data(y, yrep_na))
})

test_that("ppc_scatter_data returns zero-row data frame for zero-length input", {
  d <- ppc_scatter_data(numeric(0), matrix(numeric(0), nrow = 1, ncol = 0))
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 0)
  expect_named(d, c("y_id", "y_name", "y_obs", "rep_id", "rep_label", "value"))
})


# ppc_scatter_avg_data edge cases ------------------------------------------

test_that("ppc_scatter_avg_data returns correct structure", {
  d <- ppc_scatter_avg_data(y, yrep)
  expect_s3_class(d, "data.frame")
  expect_named(d, c("y_id", "y_name", "y_obs", "rep_id", "rep_label", "value"))
  expect_equal(nrow(d), length(y))
  expect_true(all(is.na(d$rep_id)))
})

test_that("ppc_scatter_avg_data with group adds group column", {
  d <- ppc_scatter_avg_data(y, yrep, group = group)
  expect_true("group" %in% names(d))
  expect_s3_class(d$group, "factor")
  expect_equal(nrow(d), length(y))
  expect_equal(as.character(d$group), as.character(group))
})

test_that("ppc_scatter_avg_data works with single observation", {
  y1 <- 5
  yrep1 <- matrix(c(4, 6, 5), ncol = 1)
  d <- ppc_scatter_avg_data(y1, yrep1)
  expect_equal(nrow(d), 1)
  expect_equal(d$value, mean(c(4, 6, 5)))
  expect_equal(d$y_obs, 5)
})

test_that("ppc_scatter_avg_data with custom stat function", {
  d <- ppc_scatter_avg_data(y, yrep, stat = "median")
  expected <- apply(yrep, 2, median)
  expect_equal(d$value, expected)
})

test_that("ppc_scatter_avg_data errors on NA in y", {
  y_na <- y
  y_na[1] <- NA
  expect_error(ppc_scatter_avg_data(y_na, yrep))
})

test_that("ppc_scatter_avg_data returns zero-row data frame for zero-length input", {
  d <- ppc_scatter_avg_data(numeric(0), matrix(numeric(0), nrow = 1, ncol = 0))
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 0)
})


# ppc_loo_pit_data edge cases ----------------------------------------------

test_that("ppc_loo_pit_data with pre-computed pit and boundary_correction = FALSE", {
  set.seed(42)
  pit_vals <- runif(30)
  expect_message(
    d <- ppc_loo_pit_data(pit = pit_vals, boundary_correction = FALSE, samples = 5),
    "pit"
  )
  expect_s3_class(d, "data.frame")
  y_rows <- d[d$is_y, ]
  yrep_rows <- d[!d$is_y, ]
  expect_equal(nrow(y_rows), length(pit_vals))
  expect_equal(nrow(yrep_rows), length(pit_vals) * 5)
  expect_equal(y_rows$value, pit_vals)
  expect_true(all(d$value >= 0 & d$value <= 1))
})

test_that("ppc_loo_pit_data with pre-computed pit and boundary_correction = TRUE", {
  set.seed(42)
  pit_vals <- runif(30)
  grid_len <- 128
  expect_message(
    d <- ppc_loo_pit_data(
      pit = pit_vals,
      boundary_correction = TRUE,
      samples = 5,
      grid_len = grid_len
    ),
    "pit"
  )
  expect_true("x" %in% names(d))
  y_rows <- d[d$is_y, ]
  yrep_rows <- d[!d$is_y, ]
  expect_equal(nrow(y_rows), grid_len)
  expect_equal(nrow(yrep_rows), grid_len * 5)
  expect_false(anyNA(d$x))
})

test_that("ppc_loo_pit_data with single pit value and no boundary correction", {
  expect_message(
    d <- ppc_loo_pit_data(pit = 0.5, boundary_correction = FALSE, samples = 3),
    "pit"
  )
  expect_s3_class(d, "data.frame")
  y_rows <- d[d$is_y, ]
  expect_equal(nrow(y_rows), 1)
  expect_equal(y_rows$value, 0.5)
})

test_that("ppc_loo_pit_data with custom bw parameter", {
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
  expect_s3_class(d, "data.frame")
  expect_true("x" %in% names(d))
})

test_that("ppc_loo_pit_data returns zero-row data frame for zero-length pit", {
  expect_message(
    d <- ppc_loo_pit_data(pit = numeric(0), boundary_correction = FALSE, samples = 2),
    "pit"
  )
  expect_s3_class(d, "data.frame")
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


# ppd_data edge cases ------------------------------------------------------

test_that("ppd_data works with single observation (single column)", {
  ypred <- matrix(c(1, 2, 3), ncol = 1)
  d <- ppd_data(ypred)
  expect_s3_class(d, "data.frame")
  expect_named(d, c("y_id", "y_name", "rep_id", "rep_label", "value"))
  expect_equal(nrow(d), 3)
  expect_true(all(d$y_id == 1))
  expect_equal(d$value, c(1, 2, 3))
})

test_that("ppd_data rep_label uses 'pred' not 'rep'", {
  ypred <- matrix(1:4, nrow = 2, ncol = 2)
  d <- ppd_data(ypred)
  expect_true(all(grepl("pred", levels(d$rep_label), fixed = TRUE)))
  expect_false(any(grepl("rep", levels(d$rep_label), fixed = TRUE)))
})

test_that("ppd_data errors on NA in ypred", {
  ypred_na <- matrix(c(1, NA, 3, 4), nrow = 2)
  expect_error(ppd_data(ypred_na))
})

test_that("ppd_data errors on non-matrix input", {
  expect_error(ppd_data(c(1, 2, 3)))
})

test_that("ppd_data with group errors on length mismatch", {
  ypred <- matrix(1:4, nrow = 2, ncol = 2)
  expect_error(ppd_data(ypred, group = factor(c("a", "b", "c"))))
})

test_that("ppd_data returns zero-row data frame for zero-length input", {
  d <- ppd_data(matrix(numeric(0), nrow = 1, ncol = 0))
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 0)
  expect_named(d, c("y_id", "y_name", "rep_id", "rep_label", "value"))
})


# ppd_stat_data edge cases -------------------------------------------------

test_that("ppd_stat_data returns correct structure with single stat", {
  d <- ppd_stat_data(yrep, stat = "mean")
  expect_s3_class(d, "data.frame")
  expect_true("value" %in% names(d))
  expect_true("variable" %in% names(d))
  expect_false("group" %in% names(d))
})

test_that("ppd_stat_data returns correct structure with two stats", {
  d <- ppd_stat_data(yrep, stat = c("mean", "sd"))
  expect_true("value" %in% names(d))
  expect_true("value2" %in% names(d))
})

test_that("ppd_stat_data with group adds group column", {
  d <- ppd_stat_data(yrep, group = group, stat = "mean")
  expect_true("group" %in% names(d))
  expect_s3_class(d$group, "factor")
})

test_that("ppd_stat_data works with single draw", {
  yrep_single <- matrix(rnorm(10), nrow = 1)
  d <- ppd_stat_data(yrep_single, stat = "mean")
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 1)
})

test_that("ppd_stat_data works with single observation", {
  yrep_1obs <- matrix(rnorm(5), ncol = 1)
  d <- ppd_stat_data(yrep_1obs, stat = "mean")
  expect_s3_class(d, "data.frame")
})

test_that("ppd_stat_data errors on stat length > 2", {
  expect_error(ppd_stat_data(yrep, stat = c("mean", "sd", "var")),
               "'stat' must have length 1 or 2")
})

test_that("ppd_stat_data errors on invalid stat function name", {
  expect_error(ppd_stat_data(yrep, stat = "not_a_known_function"))
})

test_that("ppd_stat_data errors on NA in ypred", {
  yrep_na <- yrep
  yrep_na[1, 1] <- NA
  expect_error(ppd_stat_data(yrep_na, stat = "mean"))
})

test_that("ppd_stat_data errors on zero-length input", {
  expect_error(ppd_stat_data(matrix(numeric(0), nrow = 1, ncol = 0), stat = "mean"))
})


# ppd_intervals_data edge cases --------------------------------------------

test_that("ppd_intervals_data returns correct structure", {
  d <- ppd_intervals_data(yrep)
  expect_s3_class(d, "data.frame")
  expected_cols <- c("y_id", "x", "inner_width", "outer_width",
                     "ll", "l", "m", "h", "hh")
  expect_true(all(expected_cols %in% names(d)))
  expect_equal(nrow(d), ncol(yrep))
})

test_that("ppd_intervals_data works with single observation", {
  yrep_1obs <- matrix(rnorm(25), ncol = 1)
  d <- ppd_intervals_data(yrep_1obs)
  expect_equal(nrow(d), 1)
  expect_true(d$ll <= d$l)
  expect_true(d$l <= d$m)
  expect_true(d$m <= d$h)
  expect_true(d$h <= d$hh)
})

test_that("ppd_intervals_data works with single draw", {
  yrep_1draw <- matrix(rnorm(10), nrow = 1)
  d <- ppd_intervals_data(yrep_1draw)
  expect_equal(nrow(d), 10)
  # single draw: all quantiles equal the value
  expect_equal(d$ll, d$m)
  expect_equal(d$hh, d$m)
})

test_that("ppd_intervals_data uses custom x values", {
  x_vals <- seq(10, 100, length.out = ncol(yrep))
  d <- ppd_intervals_data(yrep, x = x_vals)
  expect_equal(d$x, x_vals)
})

test_that("ppd_intervals_data with group adds group column", {
  d <- ppd_intervals_data(yrep, group = group)
  expect_true("group" %in% names(d))
  expect_s3_class(d$group, "factor")
})

test_that("ppd_intervals_data respects prob and prob_outer", {
  d <- ppd_intervals_data(yrep, prob = 0.5, prob_outer = 0.9)
  expect_equal(unique(d$inner_width), 0.5)
  expect_equal(unique(d$outer_width), 0.9)
})

test_that("ppd_intervals_data quantile ordering holds", {
  d <- ppd_intervals_data(yrep, prob = 0.3, prob_outer = 0.8)
  expect_true(all(d$ll <= d$l))
  expect_true(all(d$l <= d$m))
  expect_true(all(d$m <= d$h))
  expect_true(all(d$h <= d$hh))
})

test_that("ppd_intervals_data errors on NA in ypred", {
  yrep_na <- yrep
  yrep_na[1, 1] <- NA
  expect_error(ppd_intervals_data(yrep_na))
})

test_that("ppd_intervals_data errors on invalid prob_outer", {
  expect_error(ppd_intervals_data(yrep, prob_outer = 0))
  expect_error(ppd_intervals_data(yrep, prob_outer = 1.01))
})

test_that("ppd_intervals_data returns zero-row data frame for zero-length input", {
  d <- ppd_intervals_data(matrix(numeric(0), nrow = 1, ncol = 0))
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 0)
})


# ppd_ribbon_data edge cases -----------------------------------------------

test_that("ppd_ribbon_data is identical to ppd_intervals_data", {
  expect_identical(ppd_ribbon_data, ppd_intervals_data)
})

test_that("ppd_ribbon_data returns same result as ppd_intervals_data", {
  d1 <- ppd_ribbon_data(yrep, prob = 0.5, prob_outer = 0.9)
  d2 <- ppd_intervals_data(yrep, prob = 0.5, prob_outer = 0.9)
  expect_identical(d1, d2)
})
