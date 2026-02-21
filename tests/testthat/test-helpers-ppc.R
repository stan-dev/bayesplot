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

# Test for computation of Shapley values -------------------------------------

test_that("compute_shapley_values handles empty vector and single element", {
  result_empty <- compute_shapley_values(numeric(0))
  result_single <- compute_shapley_values(5)

  expect_equal(result_empty, numeric(0))
  expect_equal(length(result_empty), 0)
  expect_equal(result_single, 0)
  expect_equal(length(result_single), 1)
})

test_that("compute_shapley_values for simple case", {
  x <- c(1, 2)
  result <- compute_shapley_values(x)
  
  # Manual calculation for n=2:
  # harmonic_number = 1 + 1/2 = 1.5
  # For i=1: mean_others = 2/1 = 2
  # shapley[1] = (1/2)*1 + ((1.5-1)/2)*(1-2) = 0.5 - 0.25 = 0.25
  # For i=2: mean_others = 1/1 = 1
  # shapley[2] = (1/2)*2 + ((1.5-1)/2)*(2-1) = 1 + 0.25 = 1.25
  
  expected <- c(0.25, 1.25)
  expect_equal(result, expected, tolerance = 1e-10)
  expect_equal(length(result), 2)
})

test_that("compute_shapley_values handles mixed input values", {
  x <- c(-0.2, 0, 2, 3.1, 4.2)
  result <- compute_shapley_values(x)
  expect_equal(length(result), 5)
  expect_true(all(is.finite(result)))
})