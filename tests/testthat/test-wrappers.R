source(test_path("data-for-ppc-tests.R"))
source(test_path("data-for-mcmc-tests.R"))

# PPC wrappers -------------------------------------------------------------

test_that("ppc_error dispatches correctly", {
  skip_if_not_installed("rstantools")
  expect_gg(ppc_error(y = y, yrep = yrep[1:3, ], type = "hist"))
  expect_gg(ppc_error(y = y, yrep = yrep[1, , drop = FALSE], type = "scatter"))
  expect_gg(ppc_error(y = y, yrep = yrep, type = "scatter_avg"))
  expect_gg(ppc_error(y = y, yrep = yrep, type = "binned"))
  expect_gg(ppc_error(y = y, yrep = yrep[1:3, ], type = "hist",
                      grouped = TRUE, group = group))
  expect_error(ppc_error(y = y, yrep = yrep, type = "binned", grouped = TRUE),
               "No grouped variant")
})

test_that("ppc_dist dispatches correctly", {
  expect_gg(ppc_dist(y = y, yrep = yrep[1:3, ], type = "hist"))
  expect_gg(ppc_dist(y = y, yrep = yrep[1:3, ], type = "dens"))
  expect_gg(ppc_dist(y = y, yrep = yrep, type = "dens_overlay"))
  expect_gg(ppc_dist(y = y, yrep = yrep, type = "ecdf_overlay"))
  expect_gg(ppc_dist(y = y, yrep = yrep[1:3, ], type = "freqpoly"))
  expect_gg(ppc_dist(y = y, yrep = yrep[1:3, ], type = "boxplot"))
  expect_gg(ppc_dist(y = y, yrep = yrep, type = "dens_overlay",
                     grouped = TRUE, group = group))
  expect_error(ppc_dist(y = y, yrep = yrep, type = "violin"),
               "grouped variant")
  expect_gg(ppc_dist(y = y, yrep = yrep, type = "violin",
                     grouped = TRUE, group = group))
})

test_that("ppc_discrete dispatches correctly", {
  expect_gg(ppc_discrete(y = y2, yrep = yrep2, type = "bars"))
  expect_gg(ppc_discrete(y = y2, yrep = yrep2, type = "bars",
                         grouped = TRUE, group = group2))
  expect_error(ppc_discrete(y = y2, yrep = yrep2, type = "rootogram",
                            grouped = TRUE),
               "No grouped variant")
})

test_that("ppc_loo dispatches correctly", {
  skip_if_not_installed("loo")
  skip_if_not_installed("rstantools")
  skip_if(packageVersion("rstantools") <= "2.4.0")

  expect_gg(suppressMessages(
    ppc_loo(y = vdiff_loo_y, yrep = vdiff_loo_yrep, lw = vdiff_loo_lw,
            type = "pit_overlay")
  ))
  expect_gg(suppressWarnings(
    ppc_loo(y = vdiff_loo_y, yrep = vdiff_loo_yrep, lw = vdiff_loo_lw,
            type = "pit_qq")
  ))
})

test_that("ppc_error partial matching works", {
  skip_if_not_installed("rstantools")
  expect_gg(ppc_error(y = y, yrep = yrep[1:3, ], type = "hi"))
})

# PPD wrappers -------------------------------------------------------------

test_that("ppd_dist dispatches correctly", {
  expect_gg(ppd_dist(ypred = yrep[1:3, ], type = "hist"))
  expect_gg(ppd_dist(ypred = yrep, type = "dens_overlay"))
  expect_gg(ppd_dist(ypred = yrep[1:3, ], type = "boxplot"))
  expect_gg(ppd_dist(ypred = yrep[1:3, ], type = "freqpoly",
                     grouped = TRUE, group = group))
})

# MCMC wrappers ------------------------------------------------------------

test_that("mcmc_dist dispatches correctly", {
  expect_gg(mcmc_dist(x = dframe, type = "hist"))
  expect_gg(mcmc_dist(x = dframe, type = "dens"))
  expect_gg(mcmc_dist(x = arr, type = "dens_overlay"))
})

test_that("mcmc_trace_w dispatches correctly", {
  expect_gg(mcmc_trace_w(x = arr, type = "trace", pars = "sigma"))
  expect_gg(mcmc_trace_w(x = arr, type = "rank_overlay", pars = "sigma"))
})

test_that("mcmc_diag dispatches correctly", {
  rhat_val <- runif(10, 1, 1.5)
  expect_gg(mcmc_diag(rhat = rhat_val, type = "rhat"))
  expect_gg(mcmc_diag(rhat = rhat_val, type = "rhat_hist"))

  neff_val <- runif(10, 0, 1)
  expect_gg(mcmc_diag(ratio = neff_val, type = "neff"))
})

test_that("mcmc_nuts dispatches correctly", {
  skip_if_not_installed("gridExtra")
  np <- data.frame(
    Parameter = rep(c("accept_stat__", "stepsize__", "treedepth__",
                      "n_leapfrog__", "divergent__", "energy__"), each = 100),
    Value = rnorm(600),
    Chain = rep(1, 600),
    Iteration = rep(1:100, 6)
  )
  lp <- data.frame(
    Parameter = rep("lp__", 100),
    Value = rnorm(100),
    Chain = rep(1, 100),
    Iteration = 1:100
  )
  expect_gg(mcmc_nuts(x = np, lp = lp, type = "acceptance"))
  expect_gg(mcmc_nuts(x = np, lp = lp, type = "divergence"))
  expect_gg(mcmc_nuts(x = np, type = "energy"))
})

test_that("mcmc_recover dispatches correctly", {
  true_vals <- colMeans(mat)
  expect_gg(mcmc_recover(x = mat, true = true_vals, type = "intervals"))
  expect_gg(mcmc_recover(x = mat, true = true_vals, type = "scatter"))
  expect_gg(mcmc_recover(x = mat, true = true_vals, type = "hist"))
})

test_that("invalid type errors helpfully", {
  expect_error(ppc_error(y = y, yrep = yrep, type = "nonexistent"))
})
