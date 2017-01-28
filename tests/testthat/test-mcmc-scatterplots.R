library(bayesplot)
suppressPackageStartupMessages(library(rstanarm))
context("MCMC: scatterplots")

source("data-for-mcmc-tests.R")

# also fit an rstanarm model to use with mcmc_pairs
capture.output(
  fit <- stan_glm(mpg ~ wt + am, data = mtcars, iter = 1000, chains = 2, refresh = 0)
)
post <- as.array(fit)
lp <- log_posterior(fit)
np <- nuts_params(fit)
divs <- sample(c(0,1), size = 1000, prob = c(0.25, 0.75), replace = TRUE)
np$Value[np$Parameter=="divergent__"] <- divs # fake divergences


test_that("mcmc_scatter returns a ggplot object", {
  expect_gg(mcmc_scatter(arr, pars = c("beta[1]", "beta[2]")))
  expect_gg(mcmc_scatter(arr1chain, regex_pars = "beta", size = 3, alpha = 0.5))
  expect_gg(mcmc_scatter(mat, pars = c("sigma", "(Intercept)")))
  expect_gg(mcmc_scatter(dframe, regex_pars = "x:[2,4]"))
  expect_gg(mcmc_scatter(dframe_multiple_chains,
                         pars = c("sigma", "(Intercept)")))
})

test_that("mcmc_hex returns a ggplot object", {
  expect_gg(mcmc_hex(arr, pars = c("beta[1]", "beta[2]")))
  expect_gg(mcmc_hex(arr1chain, regex_pars = "beta", binwidth = c(.5,.5)))
})

test_that("mcmc_scatter & mcmc_hex throw error if only 1 parameter", {
  expect_error(mcmc_scatter(arr, pars = "sigma"), "exactly 2 parameters")
  expect_error(mcmc_scatter(arr1), "exactly 2 parameters")
  expect_error(mcmc_scatter(mat1), "exactly 2 parameters")
  expect_error(mcmc_hex(dframe1), "exactly 2 parameters")
  expect_error(mcmc_hex(chainlist1), "exactly 2 parameters")
})


test_that("mcmc_pairs returns a bayesplot_grid object", {
  expect_bayesplot_grid(mcmc_pairs(arr, pars = c("(Intercept)", "sigma")))
  expect_bayesplot_grid(mcmc_pairs(arr, pars = "sigma", regex_pars = "beta"))
  expect_bayesplot_grid(mcmc_pairs(arr, regex_pars = "x:[1-3]",
                                   transformations = "exp",
                                   diag_fun = "dens", off_diag_fun = "hex",
                                   diag_args = list(trim = FALSE),
                                   off_diag_args = list(binwidth = c(0.5, 0.5))))

  expect_bayesplot_grid(suppressWarnings(mcmc_pairs(arr1chain, regex_pars = "beta")))
  expect_bayesplot_grid(suppressWarnings(mcmc_pairs(mat, pars = c("(Intercept)", "sigma"))))
  expect_bayesplot_grid(suppressWarnings(mcmc_pairs(dframe, pars = c("(Intercept)", "sigma"))))
  expect_bayesplot_grid(mcmc_pairs(dframe_multiple_chains, regex_pars = "beta"))
})

test_that("mcmc_pairs works with NUTS info", {
  expect_bayesplot_grid(mcmc_pairs(post, pars = c("wt", "am", "sigma"), np = np))
  expect_bayesplot_grid(mcmc_pairs(post, pars = c("wt", "am"),
                                   condition = "energy__", np = np))
  expect_bayesplot_grid(mcmc_pairs(post, pars = c("wt", "am"),
                                   condition = "lp__", lp=lp, np = np,
                                   max_treedepth = 2))

  p <- mcmc_pairs(
    post,
    pars = c("wt", "am"),
    off_diag_fun = "hex",
    condition = "lp__",
    lp = lp,
    np = np,
    np_style = list(color = c("firebrick", "dodgerblue"), size = c(2,2)),
    max_treedepth = with(np, max(Value[Parameter == "treedepth__"]) - 1)
  )
  expect_bayesplot_grid(p)
})


test_that("mcmc_pairs throws correct warnings and errors", {
  expect_warning(mcmc_pairs(arr1chain, regex_pars = "beta"),
                 "This plot is more useful with multiple chains")
  expect_error(mcmc_pairs(arr, pars = "sigma"),
               "requires at least two parameters")
  expect_error(
    mcmc_pairs(arr, condition = 2.5),
    "If numeric, 'condition' must be an integer (vector) or a number between 0 and 1",
    fixed = TRUE
  )
  expect_error(
    mcmc_pairs(arr, condition = TRUE),
    "length(condition) == (n_iter * n_chain) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    mcmc_pairs(arr, condition = list(1)),
    "If a list, 'condition' must be of length 2",
    fixed = TRUE
  )
  expect_error(mcmc_pairs(arr, condition = "accept_stat__"),
               "the 'np' argument must also be specified")
  expect_error(mcmc_pairs(arr, condition = "lp__"),
               "the 'lp' argument must also be specified")

  expect_error(
    mcmc_pairs(post, pars = c("wt", "am"), max_treedepth = 2, np = np,
               np_style = list(color = "green")),
    "All specified elements of 'np_style' must have length 2"
  )
})
