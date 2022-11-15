library(bayesplot)
context("PPC: misc. functions")

source(test_path("data-for-ppc-tests.R"))
source(test_path("data-for-mcmc-tests.R"))

# melt_predictions ---------------------------------------------------------------
expect_molten_yrep <- function(yrep) {
  y <- rnorm(ncol(yrep))
  yrep <- validate_predictions(yrep, length(y))

  x <- melt_predictions(yrep)
  expect_equal(ncol(x), 5)
  expect_equal(nrow(x), prod(dim(yrep)))

  rep_nums <- rep(seq_len(nrow(yrep)), length(y))
  obs_nums <- sort(rep(seq_len(length(y)), nrow(yrep)))

  expect_identical(colnames(x), c("y_id", "y_name", "rep_id", "rep_label", "value"))
  expect_equal(x$y_id, obs_nums)
  expect_equal(x$rep_id, rep_nums)

  expect_s3_class(x, "data.frame")
  expect_s3_class(x$rep_label, "factor")
  expect_type(x$rep_id, "integer")
  expect_type(x$y_id, "integer")
  expect_type(x$value, "double")
}

test_that("melt_predictions returns correct structure", {
  expect_molten_yrep(yrep)
  expect_molten_yrep(yrep2)

  load(test_path("data-for-binomial.rda"))
  expect_molten_yrep(Ey)
  expect_molten_yrep(validate_predictions(yrep, length(y)))
})


# melt_and_stack ----------------------------------------------------------
test_that("melt_and_stack returns correct structure", {
  molten_yrep <- melt_predictions(yrep)
  d <- melt_and_stack(y, yrep)
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), nrow(molten_yrep) + length(y))

  sorted_names <- sort(c(colnames(molten_yrep), c("is_y", "is_y_label")))
  expect_equal(sort(colnames(d)), sorted_names)
})


# is_whole_number, all_counts --------------------------------------------
test_that("is_whole_number works correctly", {
  expect_equal(is_whole_number(c(1L, 2, 3/3, 4/5)),
               c(rep(TRUE, 3), FALSE))
  expect_true(!is_whole_number("1"))
})

test_that("all_counts works correctly", {
  expect_true(all_counts(1))
  expect_true(all_counts(0:5))
  expect_true(all_counts(matrix(rpois(10, 1), 2, 5)))
  expect_false(all_counts(rnorm(5)))
  expect_false(all_counts(c("1", "2")))
  expect_false(all_counts(c(1, 1.5)))
  expect_false(all_counts(c(-1, 2)))
})

# adjust_gamma

test_that("adjust_gamma works with different adjustment methods", {
  set.seed(8420)

  expect_equal(
    adjust_gamma(N = 100, K = 100, L = 1, prob = .99),
    adjust_gamma(N = 100, K = 100, L = 1, prob = .99, interpolate_adj = TRUE),
    tolerance = 1e-3
  )

  expect_equal(
    adjust_gamma(N = 100, K = 100, L = 4, prob = .99, M = 1000),
    adjust_gamma(N = 100, K = 100, L = 4, prob = .99, interpolate_adj = TRUE),
    tolerance = 1e-3
  )

  set.seed(NULL)
})


# get_interpolation_values ------------------------------------------------
test_that("get_interpolation_values catches impossible values", {
  expect_error(
    get_interpolation_values(1000, 1000, 0, .5),
    "No precomputed values to interpolate from for 'L' = 0."
  )
  expect_error(
    get_interpolation_values(1000, 1000, 4, 0),
    "No precomputed values to interpolate from for 'prob' = 0."
  )
  expect_error(
    get_interpolation_values(1000, 0, 4, .95),
    "No precomputed values available for interpolation for 'K' = 0."
  )
  expect_error(
    get_interpolation_values(0, 1000, 4, .95),
    "No precomputed values to interpolate from for sample length of 0."
  )
  expect_error(
    get_interpolation_values(1e5, 10, 4, .95),
    "No precomputed values to interpolate from for sample length of 1e+05",
    fixed = TRUE
  )
  expect_error(
    get_interpolation_values(100, 300, 4, .95),
    "No precomputed values available for interpolation for 'K' = 300"
  )
})

# ecdf_intervals ---------------------------------------------------------
test_that("ecdf_intervals returns right dimensions and values", {
  lims <- ecdf_intervals(.0001, N = 100, K = 100, L = 1)
  expect_named(lims, c("lower", "upper"))
  expect_length(lims$upper, 101)
  expect_length(lims$lower, 101)
  expect_equal(min(lims$upper), 0)
  expect_equal(max(lims$upper), 100)
  expect_equal(min(lims$lower), 0)
  expect_equal(max(lims$lower), 100)
})
