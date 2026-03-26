source(test_path("data-for-mcmc-tests.R"))

# param_range --------------------------------------------------------------

test_that("param_range returns empty integer when no matches", {
  all_pars <- c("alpha", "gamma[1]", "gamma[2]")
  result <- param_range("beta", 1:3, vars = all_pars)
  expect_identical(result, integer(0))
})

test_that("param_range drops non-matching indices silently", {
  all_pars <- c("alpha", "beta[1]", "beta[3]")
  # beta[2] does not exist, should be silently dropped
  result <- param_range("beta", 1:3, vars = all_pars)
  expect_equal(result, c(2L, 3L))
})

# param_glue ---------------------------------------------------------------

test_that("param_glue returns empty integer when no matches", {
  all_pars <- c("alpha", "sigma")
  result <- param_glue("beta[{i}]", i = 1:3, vars = all_pars)
  expect_identical(result, integer(0))
})

test_that("param_glue drops non-matching names silently", {
  all_pars <- c("b[X:1]", "b[Y:2]", "sigma")
  # b[X:2] and b[Y:1] don't exist
  result <- param_glue("b[{var}:{lev}]", var = c("X", "Y"), lev = c(1, 2),
                       vars = all_pars)
  expect_equal(result, c(1L, 2L))
})

# tidyselect_parameters ----------------------------------------------------

test_that("tidyselect_parameters works with negation", {
  all_pars <- c("alpha", "beta[1]", "beta[2]", "sigma")
  selected <- tidyselect_parameters(all_pars, vars(-alpha))
  expect_equal(selected, c("beta[1]", "beta[2]", "sigma"))
})
