source(test_path("data-for-ppc-tests.R"))

prop0 <- function(x) mean(x == 0)

# ppd-test-statistics with show_marginal -----------------------------------

test_that("ppd_stat_data returns PPD data", {
  d <- ppd_stat_data(yrep, group, stat = var, show_marginal = FALSE)
  expect_false("PPD" %in% d$variable)

  d <- ppd_stat_data(yrep, group, stat = var, show_marginal = TRUE)
  expect_true("PPD" %in% d$variable)

  d_no <- ppd_stat_data(yrep, group, stat = var, show_marginal = FALSE)
  expect_false("PPD" %in% d_no$variable)
})


# Visual tests -------------------------------------------------------------

test_that("ppd-dist with show_marginal render correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p <- ppd_dens_overlay(vdiff_yrep, show_marginal = TRUE)
  vdiffr::expect_doppelganger("ppd_dens_overlay (marginal)", p)

  p <- ppd_ecdf_overlay(vdiff_yrep, show_marginal = TRUE)
  vdiffr::expect_doppelganger("ppd_ecdf_overlay (marginal)", p)

  p <- ppd_dens(vdiff_yrep[1:8, ], show_marginal = TRUE)
  vdiffr::expect_doppelganger("ppd_dens (marginal)", p)

  p <- ppd_hist(vdiff_yrep[1:8, ], show_marginal = TRUE)
  vdiffr::expect_doppelganger("ppd_hist (marginal)", p)

  p <- ppd_freqpoly(vdiff_yrep[1:8, ], show_marginal = TRUE)
  vdiffr::expect_doppelganger("ppd_freqpoly (marginal)", p)

  p <- ppd_freqpoly_grouped(
    vdiff_yrep[1:3, ],
    vdiff_group,
    show_marginal = TRUE
  )
  vdiffr::expect_doppelganger("ppd_freqpoly_grouped (marginal)", p)

  p <- ppd_boxplot(vdiff_yrep[1:8, ], show_marginal = TRUE)
  vdiffr::expect_doppelganger("ppd_boxplot (marginal)", p)

  testthat::skip_if_not_installed("ggdist")
  p <- ppd_dots(vdiff_yrep[1:8, ], show_marginal = TRUE)
  vdiffr::expect_doppelganger("ppd_dots (marginal)", p)
})

test_that("ppd-stat with show_marginal render correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p <- ppd_stat(vdiff_yrep, show_marginal = TRUE, binwidth = 0.05)
  vdiffr::expect_doppelganger("ppd_stat (marginal)", p)

  set.seed(42)
  ypred_bin <- matrix(rbinom(100 * 20, 1, 0.2), nrow = 100)
  p <- ppd_stat(ypred_bin, stat = prop0, discrete = TRUE, show_marginal = TRUE)
  vdiffr::expect_doppelganger("ppd_stat (marginal, discrete)", p)
  set.seed(seed = NULL)

  p <- ppd_stat_grouped(
    vdiff_yrep,
    vdiff_group,
    show_marginal = TRUE,
    binwidth = 0.05
  )
  vdiffr::expect_doppelganger("ppd_stat_grouped (marginal)", p)

  p <- ppd_stat_freqpoly(vdiff_yrep, show_marginal = TRUE, binwidth = 0.05)
  vdiffr::expect_doppelganger("ppd_stat_freqpoly (marginal)", p)

  p <- ppd_stat_freqpoly_grouped(
    vdiff_yrep,
    vdiff_group,
    show_marginal = TRUE,
    binwidth = 0.05
  )
  vdiffr::expect_doppelganger("ppd_stat_freqpoly_grouped (marginal)", p)

  p <- ppd_stat_2d(vdiff_yrep, show_marginal = TRUE)
  vdiffr::expect_doppelganger("ppd_stat_2d (marginal)", p)
})
