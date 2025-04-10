library(bayesplot)
context("MCMC: nuts")

if (requireNamespace("rstanarm", quietly = TRUE)) {
  ITER <- 1000
  CHAINS <- 3
  fit <- rstanarm::stan_glm(mpg ~ wt + am, data = mtcars,
                            iter = ITER, chains = CHAINS,
                            refresh = 0)
  np <- nuts_params(fit)
  lp <- log_posterior(fit)
}

test_that("all mcmc_nuts_* (except energy) return gtable objects", {
  skip_if_not_installed("rstanarm")
  expect_gtable(mcmc_nuts_acceptance(np, lp))
  expect_gtable(mcmc_nuts_acceptance(np, lp, chain = CHAINS))

  expect_gtable(mcmc_nuts_treedepth(np, lp))
  expect_gtable(mcmc_nuts_treedepth(np, lp, chain = CHAINS))

  expect_gtable(mcmc_nuts_stepsize(np, lp))
  expect_gtable(mcmc_nuts_stepsize(np, lp, chain = CHAINS))

  np <- ensure_divergences(np)
  expect_gtable(mcmc_nuts_divergence(np, lp))
  expect_gtable(mcmc_nuts_divergence(np, lp, chain = CHAINS))
})

test_that("all mcmc_nuts_* (except energy) error if chain argument is bad", {
  skip_if_not_installed("rstanarm")
  funs <- c("acceptance", "divergence", "treedepth", "stepsize")
  for (f in paste0("mcmc_nuts_", funs)) {
    expect_error(do.call(f, list(x=np, lp=lp, chain = CHAINS + 1)),
                 regexp = paste("only", CHAINS, "chains found"),
                 info = f)
    expect_error(do.call(f, list(x=np, lp=lp, chain = 0)),
                 regexp = "chain >= 1",
                 info = f)
  }
})

test_that("mcmc_nuts_energy returns a ggplot object", {
  skip_if_not_installed("rstanarm")

  p <- mcmc_nuts_energy(np)
  expect_gg(p)
  expect_s3_class(p$facet, "FacetWrap")
  expect_equal(names(p$facet$params$facets), "Chain")

  p <- mcmc_nuts_energy(np, merge_chains = TRUE)
  expect_gg(p)
  expect_s3_class(p$facet, "FacetNull")
})

test_that("mcmc_nuts_energy throws correct warnings", {
  skip_if_not_installed("rstanarm")
  expect_warning(mcmc_nuts_energy(np, chain = 1), "ignored: chain")
})


test_that("validate_nuts_data_frame throws errors", {
  skip_if_not_installed("rstanarm")
  expect_error(
    validate_nuts_data_frame(list(Iteration = 1, Chain = 1)),
    "NUTS parameters should be in a data frame"
  )
  expect_error(
    validate_nuts_data_frame(data.frame(Iteration = 1, apple = 2)),
    "NUTS parameter data frame must have columns: Chain, Iteration, Parameter, Value"
  )
  expect_error(
    validate_nuts_data_frame(np, as.matrix(lp)),
    "lp should be in a data frame"
  )

  lp2 <- lp
  colnames(lp2)[3] <- "Chains"
  expect_error(
    validate_nuts_data_frame(np, lp2),
    "lp data frame must have columns: Chain, Iteration, Value"
  )

  lp2 <- subset(lp, Chain %in% 1:2)
  expect_error(
    validate_nuts_data_frame(np, lp2),
    "Number of chains"
  )
})



# Visual tests -----------------------------------------------------------------

source(test_path("data-for-mcmc-tests.R"))

test_that("mcmc_nuts_acceptance renders correctly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_nuts_acceptance(vdiff_dframe_chains_np, vdiff_dframe_chains_lp)
  vdiffr::expect_doppelganger("mcmc_nuts_acceptance (default)", p_base)

  p_chain <- mcmc_nuts_acceptance(vdiff_dframe_chains_np, vdiff_dframe_chains_lp, chain = 1)
  vdiffr::expect_doppelganger("mcmc_nuts_acceptance (chain)", p_chain)
})

test_that("mcmc_nuts_divergence renders correctly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_nuts_divergence(vdiff_dframe_chains_np, vdiff_dframe_chains_lp)
  vdiffr::expect_doppelganger("mcmc_nuts_divergence (default)", p_base)

  p_chain <- mcmc_nuts_divergence(vdiff_dframe_chains_np, vdiff_dframe_chains_lp, chain = 1)
  vdiffr::expect_doppelganger("mcmc_nuts_divergence (chain)", p_chain)
})

test_that("mcmc_nuts_treedepth renders correctly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_nuts_treedepth(vdiff_dframe_chains_np, vdiff_dframe_chains_lp)
  vdiffr::expect_doppelganger("mcmc_nuts_treedepth (default)", p_base)

  p_chain <- mcmc_nuts_treedepth(vdiff_dframe_chains_np, vdiff_dframe_chains_lp, chain = 1)
  vdiffr::expect_doppelganger("mcmc_nuts_treedepth (chain)", p_chain)
})

test_that("mcmc_nuts_stepsize renders correctly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_nuts_stepsize(vdiff_dframe_chains_np, vdiff_dframe_chains_lp)
  vdiffr::expect_doppelganger("mcmc_nuts_stepsize (default)", p_base)

  p_chain <- mcmc_nuts_stepsize(vdiff_dframe_chains_np, vdiff_dframe_chains_lp, chain = 1)
  vdiffr::expect_doppelganger("mcmc_nuts_stepsize (chain)", p_chain)
})

test_that("mcmc_nuts_energy renders correctly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_nuts_energy(vdiff_dframe_chains_np, vdiff_dframe_chains_lp, binwidth = 10)
  vdiffr::expect_doppelganger("mcmc_nuts_energy (default)", p_base)

  p_merged <- mcmc_nuts_energy(vdiff_dframe_chains_np, vdiff_dframe_chains_lp, binwidth = 10, merge_chains = TRUE)
  vdiffr::expect_doppelganger("mcmc_nuts_energy (merged)", p_merged)
})

