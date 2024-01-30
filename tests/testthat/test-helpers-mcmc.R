library(bayesplot)
context("MCMC: misc. functions")

source(test_path("data-for-mcmc-tests.R"))



# melt_mcmc ----------------------------------------------------------------
test_that("melt_mcmc does not convert integer parameter names to integers #162", {
  mat2 <- mat[, 1:2]
  colnames(mat2) <- c("1", "2")
  long_mat <- melt_mcmc(mat2)
  expect_s3_class(long_mat$Parameter, "factor")

  arr2 <- arr[, , 1:2]
  dimnames(arr2)[[3]] <- c("1", "2")
  long_arr <- melt_mcmc(prepare_mcmc_array(arr2))
  expect_s3_class(long_arr$Parameter, "factor")

  dframe2 <- dframe[, 1:2]
  colnames(dframe2) <- c("1", "2")
  long_df <- melt_mcmc(as.matrix(dframe2))
  expect_s3_class(long_df$Parameter, "factor")
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

test_that("parameter_names works", {
  x <- example_mcmc_draws()
  expect_identical(parameter_names(x), dimnames(x)[[3]])

  dimnames(x) <- list(a = NULL, b = NULL, c = letters[1:dim(x)[3]])
  expect_identical(parameter_names(x), dimnames(x)[[3]])

  dimnames(x) <- NULL
  expect_error(parameter_names(x), "No parameter names found")
  expect_error(parameter_names(x[, 1, ]), "No parameter names found")
})

test_that("has_multiple_chains works", {
  expect_error(has_multiple_chains(mat), "is_3d_array")
  expect_error(has_multiple_chains(dframe_multiple_chains), "is_3d_array")
  expect_error(has_multiple_chains(chainlist), "is_3d_array")

  expect_true(has_multiple_chains(arr))

  arr2 <- set_mcmc_dimnames(arr, parnames = dimnames(arr)[[3]])
  expect_true(has_multiple_chains(arr2))

  arr1chain2 <- set_mcmc_dimnames(arr1chain, parnames = dimnames(arr1chain)[[3]])
  expect_false(has_multiple_chains(arr1chain2))
})

test_that("has_multiple_params works", {
  expect_error(has_multiple_params(mat), "is_3d_array")
  expect_error(has_multiple_params(dframe_multiple_chains), "is_3d_array")

  expect_true(has_multiple_params(arr), "is_3d_array")

  arr2 <- set_mcmc_dimnames(arr, parnames = dimnames(arr)[[3]])
  expect_true(has_multiple_params(arr2))

  arr2 <- arr2[, , 3, drop = FALSE]
  expect_false(has_multiple_params(arr2))
})



# data frame with ‘chain’ variable ----------------------------------------
test_that("is_df_with_chain works", {
  expect_false(is_df_with_chain(arr))
  expect_false(is_df_with_chain(mat))
  expect_false(is_df_with_chain(chainlist))
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
  expect_error(validate_df_with_chain(mat), "is_df_with_chain")

  dframe_multiple_chains2 <-
    cbind(dframe_multiple_chains, Chain = dframe_multiple_chains$chain)
  dframe_multiple_chains2$chain <- NULL

  expect_identical(validate_df_with_chain(dframe_multiple_chains),
                   dframe_multiple_chains2)

  dframe_multiple_chains2$Chain <-
    factor(dframe_multiple_chains2$Chain, labels = letters[1:4])
  a <- validate_df_with_chain(dframe_multiple_chains2)
  expect_type(a$Chain, "integer")

  # no warning raised when using tibbles (#160)
  tbl <- tibble::tibble(parameter=rnorm(n=40), Chain=rep(1:4, each=10))
  a <- validate_df_with_chain(tbl)
  expect_type(a$Chain, "integer")
})

test_that("df_with_chain2array works", {
  a <- df_with_chain2array(dframe_multiple_chains)
  expect_mcmc_array(a)

  expect_error(df_with_chain2array(dframe), "is_df_with_chain")
})



# list of chains ----------------------------------------------------------
test_that("is_chain_list works", {
  expect_false(is_chain_list(arr))
  expect_false(is_chain_list(mat))
  expect_false(is_chain_list(dframe))
  expect_false(is_chain_list(dframe_multiple_chains))
  expect_true(is_chain_list(chainlist))
  expect_true(is_chain_list(chainlist1))
  expect_true(is_chain_list(chainlist1chain))
})

test_that("validate_chain_list works", {
  expect_identical(validate_chain_list(chainlist), chainlist)
  expect_identical(validate_chain_list(chainlist1), chainlist1)
  expect_identical(validate_chain_list(chainlist1chain), chainlist1chain)

  chainlist2 <- chainlist
  colnames(chainlist2[[1]]) <- colnames(chainlist[[1]])
  colnames(chainlist2[[1]])[1] <- "AAA"
  expect_error(validate_chain_list(chainlist2), "parameters for each chain")

  chainlist3 <- chainlist
  colnames(chainlist3[[1]]) <- c("", colnames(chainlist[[1]])[-1])
  expect_error(validate_chain_list(chainlist3), "Some parameters are missing names")

  chainlist[[1]] <- chainlist[[1]][-1, ]
  expect_error(validate_chain_list(chainlist),
               "Each chain should have the same number of iterations")
})

test_that("chain_list2array works", {
  expect_mcmc_array(chain_list2array(chainlist))
  expect_mcmc_array(chain_list2array(chainlist1))
  expect_mcmc_array(chain_list2array(chainlist1chain))
})



# transformations ---------------------------------------------------------
test_that("validate_transformations throws correct works", {
  trans <- list(x = "exp", 'beta[1]' = function(x) x^2, sigma = log)
  expect_silent(
    validate_transformations(trans, pars = c("x", "beta[1]", "sigma"))
  )

  trans2 <- trans
  trans2[[1]] <- match.fun(trans[[1]])
  expect_equal(
    validate_transformations(trans, pars = c("x", "beta[1]", "sigma")),
    trans2
  )
})
test_that("validate_transformations throws correct errors", {
  expect_error(
    validate_transformations(list("log", exp)),
    "must be a _named_ list"
  )
  expect_error(
    validate_transformations(list(x = "log", function(x) x^2)),
    "Each element of 'transformations' must have a name"
  )
  expect_error(
    validate_transformations(list(x = "log", 'beta[2]' = exp),
                             pars = c("x", "beta[1]")),
    regexp = "don't match parameter names: beta[2]", fixed = TRUE
  )
})

test_that("apply_transformations works", {
  trans <- list('beta[1]' = "exp", sigma = function(x) x^2)

  arr_trans <- apply_transformations(arr, transformations = trans)
  expect_equal(arr_trans[,, "sigma"], arr[,, "sigma"]^2)
  expect_equal(arr_trans[,, "beta[1]"], exp(arr[,, "beta[1]"]))

  mat_trans <- apply_transformations(mat, transformations = trans)
  expect_equal(mat_trans[, "sigma"], mat[, "sigma"]^2)
  expect_equal(mat_trans[, "beta[1]"], exp(mat[, "beta[1]"]))
})

test_that("transformations recycled properly if not a named list", {
  # if transformations is a single string naming a function
  x <- prepare_mcmc_array(arr, regex_pars = "beta", transformations = "exp")
  expect_identical(parameter_names(x), c("exp(beta[1])", "exp(beta[2])"))

  # if transformations is a single function
  x <- prepare_mcmc_array(arr, pars = c("beta[1]", "sigma"), transformations = exp)
  expect_identical(parameter_names(x), c("t(beta[1])", "t(sigma)"))
})


# prepare_mcmc_array ------------------------------------------------------
test_that("prepare_mcmc_array errors if NAs", {
  arr[1,1,1] <- NA
  expect_error(prepare_mcmc_array(arr), "NAs not allowed")
})
test_that("prepare_mcmc_array processes non-array input types correctly", {
  # errors are mostly covered by tests of the many internal functions above

  # data frame with no Chain column (treat as 1 chain or merged chains)
  a1 <- prepare_mcmc_array(dframe)
  expect_s3_class(a1, "mcmc_array")
  expect_equal(dim(a1), c(nrow(dframe), 1, ncol(dframe)))
  expect_equal(parameter_names(a1), colnames(dframe))

  # data frame with Chain column
  a2 <- prepare_mcmc_array(dframe_multiple_chains)
  expect_s3_class(a2, "mcmc_array")
  n_chain <- max(dframe_multiple_chains$chain)
  expect_equal(dim(a2), c(nrow(dframe) / n_chain, n_chain, ncol(dframe)))
  expect_equal(parameter_names(a2), colnames(dframe))

  # list of matrices with multiple chains
  a3 <- prepare_mcmc_array(chainlist)
  expect_s3_class(a3, "mcmc_array")
  expect_equal(dim(a3), c(nrow(chainlist[[1]]), length(chainlist), ncol(chainlist[[1]])))
  expect_equal(parameter_names(a3), colnames(chainlist[[1]]))

  # object with acceptable as.array method
  if (requireNamespace("rstanarm", quietly = TRUE)) {
    fit <- suppressWarnings(rstanarm::stan_glm(mpg ~ wt, data = mtcars, chains = 2, iter = 500, refresh = 0))
    a4 <- prepare_mcmc_array(fit)
    expect_s3_class(a4, "mcmc_array")
    expect_equal(a4, prepare_mcmc_array(as.array(fit)))
    expect_equal(dim(a4), c(250, 2, 3))
    expect_equal(parameter_names(a4), c("(Intercept)", "wt", "sigma"))
  }

  # object with unacceptable as.array method
  fit2 <- lm(mpg ~ wt, data = mtcars)
  expect_error(prepare_mcmc_array(fit2), "Arrays should have 2 or 3 dimensions.")
})


test_that("prepare_mcmc_array tidy parameter selection is same as traditional selection", {
  pars_all <- c(
    "(Intercept)", "beta[1]", "beta[2]", "sigma",
    "b[(Intercept) XX:1]", "b[(Intercept) XX:2]", "b[(Intercept) XX:3]",
    "b[(Intercept) ZZ:1]", "b[(Intercept) ZZ:2]", "b[(Intercept) ZZ:3]"
  )
  colnames(mat) <- pars_all

  # check easier parameters
  pars_char_1 <- c("(Intercept)", "beta[1]", "beta[2]", "sigma")
  pars_tidy_1a <- vars(`(Intercept)`, `beta[1]`, `beta[2]`, sigma)
  pars_tidy_1b <- vars(`(Intercept)`, contains("beta"), sigma)
  pars_tidy_1c <- vars("(Intercept)", param_range("beta", 1:2), "sigma")
  expect_identical(prepare_mcmc_array(mat, pars = pars_tidy_1a),
                   prepare_mcmc_array(mat, pars = pars_char_1))
  expect_identical(prepare_mcmc_array(mat, pars = pars_tidy_1b),
                   prepare_mcmc_array(mat, pars = pars_char_1))
  expect_identical(prepare_mcmc_array(mat, pars = pars_tidy_1c),
                   prepare_mcmc_array(mat, pars = pars_char_1))


  # check multilevel parameters
  pars_char_2 <- c("b[(Intercept) XX:1]", "b[(Intercept) ZZ:1]",
                   "b[(Intercept) XX:3]", "b[(Intercept) ZZ:3]")
  pars_tidy_2a <- vars(param_glue("b[(Intercept) {var}:{lev}]",
                                  var = c("XX", "ZZ"), lev = c(1, 3)))
  expect_identical(prepare_mcmc_array(mat, pars = pars_tidy_2a),
                   prepare_mcmc_array(mat, pars = pars_char_2))
})

test_that("tidy parameter selection throws correct errors", {
  expect_error(mcmc_hist(mat, pars = vars(contains("nonsense"))),
               "No parameters were found matching those names")

  expect_error(param_range("alpha", 1:3, vars = list("a", "b", "c")),
               "'vars' must be NULL or a character vector.")
  expect_error(param_glue("alpha[{lev}]", lev = 1:3, vars = 1:3,
               "'vars' must be NULL or a character vector."))
})

# rhat and neff helpers ---------------------------------------------------
test_that("diagnostic_factor.rhat works", {
  rhats <- new_rhat(c(low = 0.99, low = 1, low = 1.01,
                      ok = 1.06, ok = 1.09, ok = 1.1,
                      high = 1.2, high = 1.7))

  r <- diagnostic_factor(unname(rhats))
  expect_equivalent(r, as.factor(names(rhats)))
  expect_identical(levels(r), c("low", "ok", "high"))
})
test_that("diagnostic_factor.neff_ratio works", {
  ratios <- new_neff_ratio(c(low = 0.05, low = 0.01,
                             ok = 0.2, ok = 0.49,
                             high = 0.51, high = 0.99, high = 1))

  r <- diagnostic_factor(unname(ratios))
  expect_equivalent(r, as.factor(names(ratios)))
  expect_identical(levels(r), c("low", "ok", "high"))
})

