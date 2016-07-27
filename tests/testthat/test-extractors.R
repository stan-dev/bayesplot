library(bayesplot)
suppressPackageStartupMessages(library(rstanarm))
context("Extractors")

ITER <- 1000
CHAINS <- 3
fit <- stan_glm(mpg ~ wt + am, data = mtcars,
                iter = ITER, chains = CHAINS, refresh = 0)

# nuts_params and log_posterior methods -----------------------------------
test_that("nuts_params.stanreg returns correct structure", {
  np <- nuts_params(fit)
  expect_identical(colnames(np), c("Iteration", "Parameter", "Value", "Chain"))

  np_names <- paste0(c("accept_stat", "stepsize", "treedepth", "n_leapfrog",
                       "divergent", "energy"), "__")
  expect_identical(levels(np$Parameter), np_names)

  expect_equal(length(unique(np$Iteration)), floor(ITER / 2))
  expect_equal(length(unique(np$Chain)), CHAINS)
})

test_that("log_posterior.stanreg returns correct structure", {
  lp <- log_posterior(fit)
  expect_identical(colnames(lp), c("Iteration", "Value", "Chain"))
  expect_equal(length(unique(lp$Iteration)), floor(ITER / 2))
  expect_equal(length(unique(lp$Chain)), CHAINS)
})

test_that("rhat.stanreg returns correct structure", {
  r <- rhat(fit)
  expect_named(r)
  expect_equal(r, summary(fit)[1:length(r), "Rhat"])
})

test_that("neff_ratio.stanreg returns correct structure", {
  ratio <- neff_ratio(fit)
  expect_named(ratio)
  ans <- summary(fit)[1:length(ratio), "n_eff"] / (floor(ITER / 2) * CHAINS)
  expect_equal(ratio, ans, tol = 0.001)
})

test_that("rhat.stanfit returns correct structure", {
  r <- rhat(fit$stanfit)
  expect_named(r)
  expect_equal(r, summary(fit)[, "Rhat"])
})

test_that("neff_ratio.stanreg returns correct structure", {
  ratio <- neff_ratio(fit$stanfit)
  expect_named(ratio)
  ans <- summary(fit)[, "n_eff"] / (floor(ITER / 2) * CHAINS)
  expect_equal(ratio, ans, tol = 0.001)
})
