library(bayesplot)
context("PPC: input validation")

source(test_path("data-for-ppc-tests.R"))

# validating y ------------------------------------------------------------
test_that("validate_y works", {
  expect_identical(validate_y(y), y)
  expect_identical(validate_y(as.array(y)), y)
  expect_identical(validate_y(y2), y2)

  names(y) <- paste0("y", 1:length(y))
  expect_identical(validate_y(y), unname(y))
})
test_that("validate_y throws errors", {
  expect_error(validate_y(yrep), "vector")
  expect_error(validate_y(as.array(yrep)), "vector or 1D array")
  expect_error(validate_y(LETTERS), "numeric")
  expect_error(validate_y(c(y, NA)), "NAs not allowed")
})

# validating yrep ----------------------------------------------------------
test_that("validate_predictions works", {
  expect_identical(validate_predictions(yrep, length(y)), yrep)
  expect_equal(validate_predictions(yrep2, length(y2)), yrep2)

  colnames(yrep) <- paste0("yrep", 1:ncol(yrep))
  expect_identical(validate_predictions(yrep, length(y)), unname(yrep))
})
test_that("validate_predictions throws errors", {
  expect_error(validate_predictions(as.matrix(LETTERS), length(y)), "numeric")
  expect_error(validate_predictions(rbind(yrep, NA), length(y)), "NAs not allowed")
  expect_error(validate_predictions(y, length(y)), "matrix")
  expect_error(validate_predictions(yrep2, length(y)), "must be equal to")
  expect_error(validate_predictions(yrep, length(y2)), "must be equal to ")
})

# validating group --------------------------------------------------------
test_that("validate_group works", {
  expect_identical(validate_group(1:3, n_obs = 3), as.factor(1:3))
  expect_identical(validate_group(as.numeric(1:3), n_obs = 3), as.factor(1:3))
  expect_identical(validate_group(group, n_obs = length(y)), group)
  expect_identical(validate_group(letters[1:3], n_obs = 3), factor(letters[1:3]))
})
test_that("validate_group throws errors", {
  expect_error(validate_group(array(1:3), n_obs = 3), "vector")
  expect_error(validate_group(c(1,2,NA), n_obs = 3), "NAs not allowed")
  expect_error(validate_group(1:4, n_obs = 3),
               "must be equal to the number of observations")
  expect_error(validate_group(rep(1,3), n_obs = 3),
               "must have more than one unique value")
})


# validating x --------------------------------------------------------
test_that("validate_x works", {
  x <- rnorm(3)
  expect_identical(validate_x(x, y = 1:3), x)
  expect_identical(validate_x(array(x), y = rnorm(3)), x)
  expect_identical(validate_x(y = rnorm(3)), 1:3)

  names(x) <- letters[1:3]
  expect_identical(validate_x(x, y = 1:3), unname(x))
})
test_that("validate_x throws errors", {
  expect_error(validate_x(factor(1:3), y = 1:3), "numeric")
  expect_error(validate_x(c(1,2,NA), y = 1:3), "NAs not allowed")
  expect_error(validate_x(1:4, y = 1:3), "must be equal to")
})
