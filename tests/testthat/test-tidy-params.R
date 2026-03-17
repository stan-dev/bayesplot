source(test_path("data-for-mcmc-tests.R"))

# param_range --------------------------------------------------------------

test_that("param_range selects correct parameters by index", {
  all_pars <- c("alpha", "beta[1]", "beta[2]", "beta[3]", "sigma")
  result <- param_range("beta", c(1, 3), vars = all_pars)
  expect_equal(result, c(2L, 4L))
})

test_that("param_range returns empty integer when no matches", {
  all_pars <- c("alpha", "gamma[1]", "gamma[2]")
  result <- param_range("beta", 1:3, vars = all_pars)
  expect_identical(result, integer(0))
})

test_that("param_range handles single index", {
  all_pars <- c("alpha", "beta[1]", "beta[2]", "sigma")
  result <- param_range("beta", 2, vars = all_pars)
  expect_equal(result, 3L)
})

test_that("param_range drops non-matching indices silently", {
  all_pars <- c("alpha", "beta[1]", "beta[3]")
  # beta[2] does not exist, should be silently dropped
  result <- param_range("beta", 1:3, vars = all_pars)
  expect_equal(result, c(2L, 3L))
})

test_that("param_range errors when vars is not NULL or character", {
  expect_error(
    param_range("beta", 1:3, vars = list("a", "b")),
    "'vars' must be NULL or a character vector."
  )
  expect_error(
    param_range("beta", 1:3, vars = 1:5),
    "'vars' must be NULL or a character vector."
  )
})

# param_glue ---------------------------------------------------------------

test_that("param_glue selects correct parameters with one expression", {
  all_pars <- c("alpha[1]", "alpha[2]", "alpha[3]", "sigma")
  result <- param_glue("alpha[{i}]", i = c(1, 3), vars = all_pars)
  expect_equal(result, c(1L, 3L))
})

test_that("param_glue selects correct parameters with multiple expressions", {
  all_pars <- c(
    "b[X:1]", "b[X:2]", "b[Y:1]", "b[Y:2]", "sigma"
  )
  result <- param_glue("b[{var}:{lev}]", var = c("X", "Y"), lev = c(1, 2),
                       vars = all_pars)
  expect_equal(result, c(1L, 3L, 2L, 4L))
})

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

test_that("param_glue errors when vars is not NULL or character", {
  expect_error(
    param_glue("alpha[{i}]", i = 1:3, vars = list("a")),
    "'vars' must be NULL or a character vector."
  )
  expect_error(
    param_glue("alpha[{i}]", i = 1:3, vars = 42),
    "'vars' must be NULL or a character vector."
  )
})

# tidyselect_parameters ----------------------------------------------------

test_that("tidyselect_parameters selects by name", {
  all_pars <- c("alpha", "beta[1]", "beta[2]", "sigma")
  selected <- tidyselect_parameters(all_pars, vars(alpha, sigma))
  expect_equal(selected, c("alpha", "sigma"))
})

test_that("tidyselect_parameters works with tidyselect helpers", {
  all_pars <- c("alpha", "beta[1]", "beta[2]", "sigma")
  selected <- tidyselect_parameters(all_pars, vars(starts_with("beta")))
  expect_equal(selected, c("beta[1]", "beta[2]"))
})

test_that("tidyselect_parameters works with negation", {
  all_pars <- c("alpha", "beta[1]", "beta[2]", "sigma")
  selected <- tidyselect_parameters(all_pars, vars(-alpha))
  expect_equal(selected, c("beta[1]", "beta[2]", "sigma"))
})

test_that("tidyselect_parameters errors when nothing matches", {
  all_pars <- c("alpha", "beta[1]", "sigma")
  expect_error(
    tidyselect_parameters(all_pars, vars(starts_with("zzz"))),
    "No parameters were found matching those names."
  )
})

test_that("tidyselect_parameters works with contains()", {
  all_pars <- c("b[(Intercept) X:1]", "b[(Intercept) X:2]", "sigma")
  selected <- tidyselect_parameters(all_pars, vars(contains("Intercept")))
  expect_equal(selected, c("b[(Intercept) X:1]", "b[(Intercept) X:2]"))
})

# Integration with vars() --------------------------------------------------

test_that("param_range works inside vars() via prepare_mcmc_array", {
  result <- prepare_mcmc_array(mat, pars = vars(param_range("beta", 1:2)))
  pars <- dimnames(result)[[3]]
  expect_equal(pars, c("beta[1]", "beta[2]"))
})

test_that("param_glue works inside vars() via prepare_mcmc_array", {
  result <- prepare_mcmc_array(
    mat,
    pars = vars(param_glue("b[(Intercept) x:{i}]", i = c(1, 3)))
  )
  pars <- dimnames(result)[[3]]
  expect_equal(pars, c("b[(Intercept) x:1]", "b[(Intercept) x:3]"))
})
