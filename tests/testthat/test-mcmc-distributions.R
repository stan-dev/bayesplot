library(bayesplot)
context("MCMC: distributions")

source(test_path("data-for-mcmc-tests.R"))

get_palette <- function(ggplot, n) {
  scale <- ggplot$scales$get_scales("colour")
  scale$palette(n)
}

test_that("mcmc_hist returns a ggplot object", {
  expect_gg(mcmc_hist(arr, pars = "beta[1]", regex_pars = "x\\:", binwidth = 0.1))
  expect_gg(mcmc_hist(arr1chain, regex_pars = "beta", binwidth = 0.1))
  expect_gg(mcmc_hist(drawsarr, pars = "theta[1]", binwidth = 0.1))
  expect_gg(mcmc_hist(drawsarr1chain, regex_pars = "theta", binwidth = 0.1))
  expect_gg(mcmc_hist(mat, binwidth = 0.1))
  expect_gg(mcmc_hist(dframe, binwidth = 0.1))
  expect_gg(mcmc_hist(dframe, bins = 10))
  expect_gg(mcmc_hist(dframe_multiple_chains, binwidth = 0.1))

  expect_gg(mcmc_hist(arr1, binwidth = 0.1))
  expect_gg(mcmc_hist(drawsarr1, binwidth = 0.1))
  expect_gg(mcmc_hist(mat1, binwidth = 0.1))
  expect_gg(mcmc_hist(dframe1, binwidth = 0.1))
})

test_that("mcmc_dens returns a ggplot object", {
  expect_gg(mcmc_dens(arr, pars = "beta[2]", regex_pars = "x\\:"))
  expect_gg(mcmc_dens(arr1chain, regex_pars = "beta"))
  expect_gg(mcmc_dens(drawsarr, pars = "theta[1]"))
  expect_gg(mcmc_dens(drawsarr1chain, regex_pars = "theta"))
  expect_gg(mcmc_dens(mat))

  expect_gg(mcmc_dens(dframe, transformations = list(sigma = function(x) x^2)))
  expect_gg(mcmc_dens(
    dframe_multiple_chains,
    transformations =
      list(sigma = function(x) x ^ 2, 'beta[1]' = "exp")
  ))

  expect_gg(mcmc_dens(arr1))
  expect_gg(mcmc_dens(drawsarr1))
  expect_gg(mcmc_dens(mat1))
  expect_gg(mcmc_dens(dframe1))
})


# functions that require multiple chains ----------------------------------
test_that("mcmc_hist_by_chain returns a ggplot object", {
  expect_gg(mcmc_hist_by_chain(arr, pars = "beta[1]", regex_pars = "x\\:", binwidth = 0.1))
  expect_gg(mcmc_hist_by_chain(dframe_multiple_chains,
                               regex_pars = c("(Intercept)", "beta"), binwidth = 0.1))
})

test_that("mcmc_dens_overlay returns a ggplot object", {
  expect_gg(mcmc_dens_overlay(arr, pars = "beta[1]", regex_pars = "x\\:"))
  expect_gg(mcmc_dens_overlay(dframe_multiple_chains,
                              pars = c("(Intercept)", "beta[2]")))
})

test_that("mcmc_dens_chains returns a ggplot object", {
  p <- mcmc_dens_chains(arr, pars = "beta[1]", regex_pars = "x\\:",
                        color_chains = FALSE)
  expect_gg(p)

  p2 <- mcmc_dens_overlay(dframe_multiple_chains,
                          pars = c("(Intercept)", "beta[2]"),
                          color_chains = TRUE)
  expect_gg(p2)
})

test_that("mcmc_dens_chains/mcmc_dens_overlay color chains", {
  p1 <- mcmc_dens_chains(arr, pars = "beta[1]", regex_pars = "x\\:",
                         color_chains = FALSE)
  p2 <- mcmc_dens_overlay(arr, pars = "beta[1]", regex_pars = "x\\:",
                          color_chains = FALSE)

  # Only one color when set not to color chains
  expect_equal(length(unique(get_palette(p1, 4))), 1)
  expect_equal(length(unique(get_palette(p2, 4))), 1)

  p3 <- mcmc_dens_chains(arr, pars = "beta[1]", regex_pars = "x\\:",
                         color_chains = TRUE)
  p4 <- mcmc_dens_overlay(arr, pars = "beta[1]", regex_pars = "x\\:",
                          color_chains = TRUE)

  # Chain coloring works
  expect_equal(get_palette(p3, 4), chain_colors(4))
  expect_equal(get_palette(p4, 4), chain_colors(4))
})

test_that("mcmc_violin returns a ggplot object", {
  expect_gg(mcmc_violin(arr, pars = "beta[2]", regex_pars = "x\\:"))
  expect_gg(mcmc_violin(dframe_multiple_chains,
                        regex_pars = c("\\(Intercept\\)$", "beta")))
})

test_that("mcmc_* throws error if 1 chain but multiple chains required", {
  expect_error(mcmc_hist_by_chain(mat), "requires multiple chains")
  expect_error(mcmc_hist_by_chain(dframe), "requires multiple chains")
  expect_error(mcmc_hist_by_chain(arr1chain), "requires multiple chains")
  expect_error(mcmc_hist_by_chain(drawsarr1chain), "requires multiple chains")

  expect_error(mcmc_dens_overlay(mat), "requires multiple chains")
  expect_error(mcmc_dens_overlay(dframe), "requires multiple chains")
  expect_error(mcmc_dens_overlay(arr1chain), "requires multiple chains")
  expect_error(mcmc_dens_overlay(drawsarr1chain), "requires multiple chains")

  expect_error(mcmc_dens_chains(mat), "requires multiple chains")
  expect_error(mcmc_dens_chains(dframe), "requires multiple chains")
  expect_error(mcmc_dens_chains(arr1chain), "requires multiple chains")
  expect_error(mcmc_dens_chains(drawsarr1chain), "requires multiple chains")

  expect_error(mcmc_violin(mat), "requires multiple chains")
  expect_error(mcmc_violin(dframe), "requires multiple chains")
  expect_error(mcmc_violin(arr1chain), "requires multiple chains")
  expect_error(mcmc_violin(drawsarr1chain), "requires multiple chains")
})



# Visual tests ------------------------------------------------------------

test_that("mcmc_hist renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_hist(vdiff_dframe, binwidth = 0.1)
  vdiffr::expect_doppelganger("mcmc_hist (default)", p_base)

  p_freq <- mcmc_hist(vdiff_dframe, freq = TRUE, binwidth = 0.1)
  vdiffr::expect_doppelganger("mcmc_hist (freq)", p_freq)

  p_alpha <- mcmc_hist(vdiff_dframe, alpha = 0, binwidth = 0.1)
  vdiffr::expect_doppelganger("mcmc_hist (alpha)", p_alpha)
})

test_that("mcmc_dens renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_dens(vdiff_dframe)
  vdiffr::expect_doppelganger("mcmc_dens (default)", p_base)

  p_alpha <- mcmc_dens(vdiff_dframe, alpha = 0)
  vdiffr::expect_doppelganger("mcmc_dens (alpha)", p_alpha)
})

test_that("mcmc_dens_overlay renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_dens_overlay(vdiff_dframe_chains)
  vdiffr::expect_doppelganger("mcmc_dens_overlay (default)", p_base)
})

test_that("mcmc_dens_chains renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_dens_chains(vdiff_dframe_chains)
  vdiffr::expect_doppelganger("mcmc_dens_chains (default)", p_base)
})

test_that("mcmc_hist_by_chain renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_hist_by_chain(vdiff_dframe_chains, binwidth = 0.5)
  vdiffr::expect_doppelganger("mcmc_hist_by_chain (default)", p_base)
})

test_that("mcmc_violin renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_violin(vdiff_dframe_chains)
  vdiffr::expect_doppelganger("mcmc_violin (default)", p_base)
})

