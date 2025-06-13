library(bayesplot)
context("MCMC: traces")

source(test_path("data-for-mcmc-tests.R"))

test_that("mcmc_trace returns a ggplot object", {
  expect_gg(mcmc_trace(arr, pars = "beta[1]", regex_pars = "x\\:"))
  expect_gg(mcmc_trace(arr1chain, pars = "beta[2]", regex_pars = "x\\:"))
  expect_gg(mcmc_trace(mat))
  expect_gg(mcmc_trace(dframe))
  expect_gg(mcmc_trace(dframe_multiple_chains))
  expect_gg(mcmc_trace(chainlist))

  expect_gg(mcmc_trace(arr1))
  expect_gg(mcmc_trace(mat1))
  expect_gg(mcmc_trace(dframe1))
  expect_gg(mcmc_trace(chainlist1))
})

# functions that require multiple chains ----------------------------------
test_that("mcmc_trace_highlight returns a ggplot object", {
  expect_gg(mcmc_trace_highlight(arr, regex_pars = c("beta", "x\\:")))
  expect_gg(mcmc_trace_highlight(dframe_multiple_chains, highlight = 2))
})

test_that("mcmc_trace_highlight throws error if 1 chain but multiple chains required", {
  expect_error(mcmc_trace_highlight(mat), "requires multiple")
  expect_error(mcmc_trace_highlight(dframe, highlight = 1), "requires multiple chains")
  expect_error(mcmc_trace_highlight(arr1chain, highlight = 1), "requires multiple chains")
})

test_that("mcmc_trace_highlight throws error if highlight > number of chains", {
  expect_error(mcmc_trace_highlight(arr, pars = "sigma", highlight = 7), "'highlight' is 7")
})

test_that("mcmc_rank_ecdf returns a ggplot object", {
  expect_gg(mcmc_rank_ecdf(arr, regex_pars = c("beta", "x\\:")))
  expect_gg(mcmc_rank_ecdf(dframe_multiple_chains, interpolate_adj = FALSE))
})

test_that("mcmc_rank_ecdf throws error if 1 chain but multiple chains required", {
  expect_error(mcmc_rank_ecdf(mat), "requires multiple chains")
  expect_error(mcmc_rank_ecdf(dframe), "requires multiple chains")
  expect_error(mcmc_rank_ecdf(arr1chain), "requires multiple chains")
})

# options -----------------------------------------------------------------
test_that("mcmc_trace options work", {
  expect_gg(g1 <- mcmc_trace(arr, regex_pars = "beta", window = c(5, 10)))
  coord <- g1$coordinates
  expect_equal(g1$coordinates$limits$x, c(5, 10))
  expect_error(mcmc_trace(arr, iter1 = -1))
  expect_error(mcmc_trace(arr, n_warmup = 50, iter1 = 20))
})

test_that("mcmc_rank_ecdf options work", {
  expect_error(
    mcmc_rank_ecdf(dframe_multiple_chains, interpolate_adj = TRUE),
    "No precomputed values"
  )
})

# displaying divergences in traceplot -------------------------------------
test_that("mcmc_trace 'np' argument works", {
  skip_if_not_installed("rstanarm")
  suppressPackageStartupMessages(library(rstanarm))
  fit <- stan_glm(mpg ~ wt + am, data = mtcars, iter = 1000, chains = 2, refresh = 0)
  draws <- as.array(fit)

  # divergences via nuts_params
  divs1 <- ensure_divergences(nuts_params(fit, pars = "divergent__"))
  g <- mcmc_trace(draws, pars = "sigma", np = divs1)
  expect_gg(g)
  l2_data <- g$layers[[2]]$data
  expect_equal(names(l2_data), "Divergent")

  # divergences as vector via 'divergences' arg should throw deprecation warning
  divs2 <- rep_len(c(0,1), length.out = nrow(draws))
  expect_warning(
    g2 <- mcmc_trace(draws, pars = "sigma", divergences = divs2),
    regexp = "deprecated"
  )
  expect_gg(g2)

  expect_error(
    mcmc_trace(draws, pars = "sigma", np = divs1, divergences = divs2),
    "can't both be specified"
  )

  # check errors & messages
  expect_error(mcmc_trace(draws, pars = "sigma", np = 1),
               "length(divergences) == n_iter is not TRUE",
               fixed = TRUE)
  expect_error(mcmc_trace(draws[,1,], pars = "sigma", np = divs1),
               "num_chains(np) == n_chain is not TRUE",
               fixed = TRUE)
  expect_error(mcmc_trace(draws, pars = "sigma", np = divs1[1:10, ]),
               "num_iters(np) == n_iter is not TRUE",
               fixed = TRUE)

  divs1$Value[divs1$Parameter == "divergent__"] <- 0
  expect_message(mcmc_trace(draws, pars = "sigma", np = divs1),
                 "No divergences to plot.")
})




# Visual tests -----------------------------------------------------------------

test_that("mcmc_trace renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_trace(vdiff_dframe_chains, pars = c("V1", "V2"))
  p_one_param <- mcmc_trace(vdiff_dframe_chains, pars = "V1")

  p_warmup <- mcmc_trace(
    vdiff_dframe_chains, pars = c("V1", "V2"),
    n_warmup = 200
  )

  p_iter1 <- mcmc_trace(
    vdiff_dframe_chains, pars = c("V1", "V2"), iter1 = 200
  )

  vdiffr::expect_doppelganger("mcmc_trace (default)", p_base)
  vdiffr::expect_doppelganger("mcmc_trace (one parameter)", p_one_param)
  vdiffr::expect_doppelganger("mcmc_trace (warmup window)", p_warmup)
  vdiffr::expect_doppelganger("mcmc_trace (iter1 offset)", p_iter1)
})

test_that("mcmc_rank_overlay renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_rank_overlay(vdiff_dframe_chains, pars = c("V1", "V2"))
  p_base_ref <- mcmc_rank_overlay(
    vdiff_dframe_chains,
    pars = c("V1", "V2"),
    ref_line = TRUE
  )
  p_one_param <- mcmc_rank_overlay(vdiff_dframe_chains, pars = "V1")
  p_one_param_wide_bins <- mcmc_rank_overlay(
    vdiff_dframe_chains,
    pars = "V1",
    n_bins = 4
  )

  # https://github.com/stan-dev/bayesplot/issues/331
  p_not_all_bins_exist <- mcmc_rank_overlay(vdiff_dframe_rank_overlay_bins_test)

  vdiffr::expect_doppelganger("mcmc_rank_overlay (default)", p_base)
  vdiffr::expect_doppelganger(
    "mcmc_rank_overlay (reference line)",
    p_base_ref
  )
  vdiffr::expect_doppelganger("mcmc_rank_overlay (one parameter)", p_one_param)
  vdiffr::expect_doppelganger(
    "mcmc_rank_overlay (wide bins)",
    p_one_param_wide_bins
  )

  # https://github.com/stan-dev/bayesplot/issues/331
  vdiffr::expect_doppelganger("mcmc_rank_overlay (not all bins)", p_not_all_bins_exist)
})

test_that("mcmc_rank_hist renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_rank_hist(vdiff_dframe_chains, pars = c("V1", "V2"))
  p_base_ref <- mcmc_rank_hist(
    vdiff_dframe_chains,
    pars = c("V1", "V2"),
    ref_line = TRUE
  )
  p_one_param <- mcmc_rank_hist(vdiff_dframe_chains, pars = "V1")
  p_one_param_wide_bins <- mcmc_rank_hist(
    vdiff_dframe_chains,
    pars = "V1",
    n_bins = 4
  )

  vdiffr::expect_doppelganger("mcmc_rank_hist (default)", p_base)
  vdiffr::expect_doppelganger(
    "mcmc_rank_hist (reference line)",
    p_base_ref
  )
  vdiffr::expect_doppelganger(
    "mcmc_rank_hist (one parameter)",
    p_one_param
  )
  vdiffr::expect_doppelganger(
    "mcmc_rank_hist (wide bins)",
    p_one_param_wide_bins
  )
})

test_that("mcmc_trace_highlight renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_trace_highlight(
    vdiff_dframe_chains,
    pars = "V1",
    highlight = 1
  )

  p_2 <- mcmc_trace_highlight(
    vdiff_dframe_chains,
    pars = "V1",
    highlight = 2
  )

  p_alpha <- mcmc_trace_highlight(
    vdiff_dframe_chains,
    pars = "V1",
    highlight = 1,
    alpha = .1
  )

  vdiffr::expect_doppelganger("mcmc_trace_highlight (default)", p_base)
  vdiffr::expect_doppelganger("mcmc_trace_highlight (other chain)", p_2)
  vdiffr::expect_doppelganger("mcmc_trace_highlight (alpha)", p_alpha)
})

test_that("mcmc_rank_ecdf renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_rank_ecdf(vdiff_dframe_chains, pars = c("V1", "V2"))
  p_one_param <- mcmc_rank_ecdf(vdiff_dframe_chains, pars = "V1")

  p_diff <- mcmc_rank_ecdf(
    vdiff_dframe_chains,
    pars = c("V1", "V2"),
    plot_diff = TRUE
  )

  p_diff_one_param <- mcmc_rank_ecdf(
    vdiff_dframe_chains,
    pars = "V1",
    plot_diff = TRUE
  )

  vdiffr::expect_doppelganger("mcmc_rank_ecdf (default)", p_base)
  vdiffr::expect_doppelganger("mcmc_rank_ecdf (one parameter)", p_one_param)
  vdiffr::expect_doppelganger("mcmc_rank_ecdf (diff)", p_diff)
  vdiffr::expect_doppelganger(
    "mcmc_rank_ecdf (one param, diff)",
    p_diff_one_param
  )
})

test_that("mcmc_trace with 'np' renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- mcmc_trace(
    vdiff_dframe_chains,
    pars = "V1",
    np = vdiff_dframe_chains_divergences
  )

  new_style <- trace_style_np(div_color = "black")

  p_np_style <- mcmc_trace(
    vdiff_dframe_chains,
    pars = "V1",
    np = vdiff_dframe_chains_divergences,
    np_style = new_style
  )

  vdiffr::expect_doppelganger("mcmc_trace divergences (default)", p_base)
  vdiffr::expect_doppelganger("mcmc_trace divergences (custom)",  p_np_style)
})
