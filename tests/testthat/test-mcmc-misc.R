library(bayesplot)
context("MCMC: misc. functions")

source("data-for-mcmc-tests.R")


# validate_mcmc_x ----------------------------------------------------------
test_that("validate_mcmc_x works", {
  expect_identical(validate_mcmc_x(mat), mat)
  expect_identical(validate_mcmc_x(mat1), mat1)
  expect_identical(validate_mcmc_x(arr), arr)
  expect_identical(validate_mcmc_x(arr1), arr1)
  expect_identical(validate_mcmc_x(arr1chain), arr1chain)

  # error if df_with_chain
  expect_error(validate_mcmc_x(dframe_multiple_chains), "is_df_with_chain")

  # converts regular df to matrix
  expect_identical(validate_mcmc_x(dframe), as.matrix(dframe))

  # NAs
  mat[1, 2] <- NA
  arr[1, 2, 3] <- NA
  expect_error(validate_mcmc_x(mat), "NAs not allowed")
  expect_error(validate_mcmc_x(arr), "NAs not allowed")
})


# 3-D array helpers --------------------------------------------------------
test_that("is_mcmc_array works", {
  expect_false(is_mcmc_array(mat))
  expect_false(is_mcmc_array(dframe))
  expect_false(is_mcmc_array(dframe_multiple_chains))
  expect_false(is_mcmc_array(arr))
  arr2 <- set_mcmc_dimnames(arr, parnames = dimnames(arr)[[3]])
  expect_mcmc_array(arr2)
})

test_that("has_multiple_chains works", {
  expect_error(has_multiple_chains(mat), "is_mcmc_array")
  expect_error(has_multiple_chains(dframe_multiple_chains), "is_mcmc_array")
  expect_error(has_multiple_chains(arr), "is_mcmc_array")

  arr2 <- set_mcmc_dimnames(arr, parnames = dimnames(arr)[[3]])
  expect_true(has_multiple_chains(arr2))

  arr1chain2 <- set_mcmc_dimnames(arr1chain, parnames = dimnames(arr1chain)[[3]])
  expect_false(has_multiple_chains(arr1chain2))
})

test_that("has_multiple_params works", {
  expect_error(has_multiple_params(mat), "is_mcmc_array")
  expect_error(has_multiple_params(dframe_multiple_chains), "is_mcmc_array")
  expect_error(has_multiple_params(arr), "is_mcmc_array")

  arr2 <- set_mcmc_dimnames(arr, parnames = dimnames(arr)[[3]])
  expect_true(has_multiple_params(arr2))

  arr2 <- arr2[, , 3, drop = FALSE]
  expect_false(has_multiple_params(arr2))
})

# data frame with ‘chain’ variable ----------------------------------------
test_that("is_df_with_chain works", {
  expect_false(is_df_with_chain(arr))
  expect_false(is_df_with_chain(mat))
  expect_false(is_df_with_chain(dframe))
  expect_true(is_df_with_chain(dframe_multiple_chains))

  mat2 <- cbind(mat, chain = dframe_multiple_chains$chain)
  expect_false(is_df_with_chain(mat2))

  dframe_multiple_chains2 <-
    cbind(dframe_multiple_chains, Chain = dframe_multiple_chains$chain)
  dframe_multiple_chains2$chain <- NULL
  expect_true(is_df_with_chain(dframe_multiple_chains2))
})

test_that("validate_df_with_chain works", {
  expect_error(validate_df_with_chain(mat), "is.data.frame")

  dframe_multiple_chains2 <-
    cbind(dframe_multiple_chains, Chain = dframe_multiple_chains$chain)
  dframe_multiple_chains2$chain <- NULL

  expect_identical(validate_df_with_chain(dframe_multiple_chains),
                   dframe_multiple_chains2)

  dframe_multiple_chains2$Chain <-
    factor(dframe_multiple_chains2$Chain, labels = letters[1:4])
  a <- validate_df_with_chain(dframe_multiple_chains2)
  expect_type(a$Chain, "integer")
})

test_that("df_with_chain2array works", {
  a <- validate_df_with_chain(dframe_multiple_chains)
  expect_mcmc_array(df_with_chain2array(a))

  expect_error(df_with_chain2array(dframe), "is_df_with_chain")
})
