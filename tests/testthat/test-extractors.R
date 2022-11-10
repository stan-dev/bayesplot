library(bayesplot)
context("Extractors")

if (requireNamespace("rstanarm", quietly = TRUE)) {
  ITER <- 1000
  CHAINS <- 3
  fit <- rstanarm::stan_glm(mpg ~ wt + am, data = mtcars,
                            iter = ITER, chains = CHAINS,
                            refresh = 0)
}
x <- list(cbind(a = 1:3, b = rnorm(3)), cbind(a = 1:3, b = rnorm(3)))

# nuts_params and log_posterior methods -----------------------------------
test_that("nuts_params.list throws errors", {
  x[[3]] <- c(a = 1:3, b = rnorm(3))
  expect_error(nuts_params.list(x), "list elements should be matrices")

  x[[3]] <- cbind(a = 1:3, d = rnorm(3))
  expect_error(nuts_params.list(x), "same column names")

  x[[3]] <- cbind(a = 1:4, b = rnorm(4))
  expect_error(nuts_params.list(x), "same dimensions")
})

test_that("nuts_params.list parameter selection ok", {
  expect_error(nuts_params.list(x, pars = "apple"), "subscript out of bounds")

  np <- nuts_params.list(x, pars = "b")
  expect_true(all(np$Parameter == "b"))
})

test_that("all nuts_params methods identical", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("rstan")
  expect_identical(
    nuts_params(fit),
    nuts_params(fit$stanfit)
  )
  expect_identical(
    nuts_params(fit),
    nuts_params(rstan::get_sampler_params(fit$stanfit, inc_warmup = FALSE))
  )
})

test_that("nuts_params.stanreg returns correct structure", {
  skip_if_not_installed("rstanarm")

  np <- nuts_params(fit)
  expect_identical(colnames(np), c("Chain", "Iteration", "Parameter", "Value"))

  np_names <- paste0(c("accept_stat", "stepsize", "treedepth", "n_leapfrog",
                       "divergent", "energy"), "__")
  expect_identical(levels(np$Parameter), np_names)

  expect_equal(length(unique(np$Iteration)), floor(ITER / 2))
  expect_equal(length(unique(np$Chain)), CHAINS)
})

test_that("log_posterior.stanreg returns correct structure", {
  skip_if_not_installed("rstanarm")

  lp <- log_posterior(fit)
  expect_identical(colnames(lp), c("Chain", "Iteration", "Value"))
  expect_equal(length(unique(lp$Iteration)), floor(ITER / 2))
  expect_equal(length(unique(lp$Chain)), CHAINS)
})

test_that("rhat.stanreg returns correct structure", {
  skip_if_not_installed("rstanarm")

  r <- rhat(fit)
  expect_named(r)
  expect_equal(r, summary(fit)[1:length(r), "Rhat"])

  expect_identical(names(rhat(fit, regex_pars = c("wt", "am"))),
                   c("wt", "am"))
})

test_that("neff_ratio.stanreg returns correct structure", {
  skip_if_not_installed("rstanarm")

  expect_named(neff_ratio(fit, pars = c("wt", "am")), c("wt", "am"))

  ratio <- neff_ratio(fit)
  expect_named(ratio)
  ans <- summary(fit)[1:length(ratio), "n_eff"] / (floor(ITER / 2) * CHAINS)
  expect_equal(ratio, ans, tol = 0.001)
})

test_that("rhat.stanfit returns correct structure", {
  skip_if_not_installed("rstanarm")

  r <- rhat(fit$stanfit)
  expect_named(r)
  expect_equal(r, summary(fit)[, "Rhat"])

  r2 <- rhat(fit$stanfit, pars = c("wt", "sigma"))
  expect_named(r2)
  expect_equal(r2, summary(fit, pars = c("wt", "sigma"))[, "Rhat"])
})

test_that("neff_ratio.stanreg returns correct structure", {
  skip_if_not_installed("rstanarm")

  denom <- floor(ITER / 2) * CHAINS

  ratio <- neff_ratio(fit$stanfit)
  expect_named(ratio)
  ans <- summary(fit)[, "n_eff"] / denom
  expect_equal(ratio, ans, tol = 0.001)

  ratio2 <- neff_ratio(fit$stanfit, pars = c("wt", "sigma"))
  expect_named(ratio2)
  ans2 <- summary(fit, pars = c("wt", "sigma"))[, "n_eff"] / denom
  expect_equal(ratio2, ans2, tol = 0.001)
})

test_that("cmdstanr methods work", {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")

  fit <- cmdstanr::cmdstanr_example("logistic", iter_sampling = 500, chains = 2)
  np <- nuts_params(fit)
  np_names <- paste0(c("treedepth", "divergent", "energy", "accept_stat", "stepsize", "n_leapfrog"), "__")
  expect_identical(levels(np$Parameter), np_names)
  expect_equal(range(np$Iteration), c(1, 500))
  expect_equal(range(np$Chain), c(1, 2))
  expect_true(all(np$Value[np$Parameter == "divergent__"] == 0))

  lp <- log_posterior(fit)
  expect_named(lp, c("Chain", "Iteration", "Value"))
  expect_equal(range(np$Chain), c(1, 2))
  expect_equal(range(np$Iteration), c(1, 500))

  r <- rhat(fit)
  expect_named(head(r, 4), c("alpha", "beta[1]", "beta[2]", "beta[3]"))
  expect_true(all(round(r) == 1))

  ratio <- neff_ratio(fit)
  expect_named(head(ratio, 4), c("alpha", "beta[1]", "beta[2]", "beta[3]"))
  expect_true(all(ratio > 0))
})
