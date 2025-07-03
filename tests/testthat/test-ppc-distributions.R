library(bayesplot)
context("PPC: distributions")

source(test_path("data-for-ppc-tests.R"))

test_that("ppc_dens_overlay returns a ggplot object", {
  expect_gg(ppc_dens_overlay(y, yrep))
  expect_gg(ppc_dens_overlay(y2, yrep2, size = 0.5, alpha = 0.2))

  # ppd versions
  expect_gg(ppd_dens_overlay(yrep))
  expect_gg(ppd_dens_overlay(yrep2, size = 0.5, alpha = 0.2))
})

test_that("ppc_ecdf_overlay returns a ggplot object", {
  expect_gg(ppc_ecdf_overlay(y, yrep, size = 0.5, alpha = 0.2))
  expect_gg(ppc_ecdf_overlay(y2, yrep2))

  # ppd versions
  expect_gg(ppd_ecdf_overlay(yrep, size = 0.5, alpha = 0.2))
  expect_gg(ppd_ecdf_overlay(yrep2))
})

test_that("ppc_dens,pp_hist,ppc_freqpoly,ppc_boxplot return ggplot objects", {
  expect_gg(ppc_hist(y, yrep[1,, drop = FALSE], binwidth = 0.1))
  expect_gg(ppc_hist(y, yrep[1:8, ], binwidth = 0.1))
  expect_gg(ppc_hist(y2, yrep2, binwidth = 0.1))

  expect_gg(ppc_dens(y, yrep[1:8, ]))
  expect_gg(ppc_dens(y2, yrep2))

  expect_gg(ppc_freqpoly(y, yrep[1:8, ], binwidth = 2, size = 2, alpha = 0.1))
  expect_gg(ppc_freqpoly(y2, yrep2, binwidth = 0.1))

  expect_gg(ppc_boxplot(y, yrep[1,, drop = FALSE]))
  expect_gg(ppc_boxplot(y, yrep[1:8, ]))
  expect_gg(ppc_boxplot(y2, yrep2, notch = FALSE))

  expect_gg(p <- ppc_hist(y, yrep[1:8, ], binwidth = 3))
  if (utils::packageVersion("ggplot2") >= "3.0.0") {
    facet_var <- vars(rep_label)
    expect_equal(p$facet$params$facets[[1]], facet_var[[1]])
  }

  # ppd versions
  expect_gg(ppd_hist(yrep[1,, drop = FALSE], binwidth = 0.1))
  expect_gg(ppd_hist(yrep[1:8, ], binwidth = 0.1))
  expect_gg(ppd_hist(yrep2, binwidth = 0.1))

  expect_gg(ppc_dens(y, yrep[1:8, ]))
  expect_gg(ppc_dens(y2, yrep2))

  expect_gg(ppd_freqpoly(yrep[1:8, ], binwidth = 2, size = 2, alpha = 0.1))
  expect_gg(ppd_freqpoly(yrep2, binwidth = 0.1))

  expect_gg(ppd_boxplot(yrep[1,, drop = FALSE]))
  expect_gg(ppd_boxplot(yrep[1:8, ]))
  expect_gg(ppd_boxplot(yrep2, notch = FALSE))
})

test_that("ppc_dots returns a ggplot object", {
  testthat::skip_if_not_installed("ggdist")

  expect_gg(ppc_dots(y, yrep[1:8, ]))
  expect_gg(ppc_dots(y, yrep[1,, drop = FALSE], quantiles = 25))
  expect_gg(ppc_dots(y, yrep[1:8, ], binwidth = 0.1))
  expect_gg(ppc_dots(y2, yrep2, binwidth = 0.1, quantiles = 25))

  # ppd versions
  expect_gg(ppd_dots(yrep[1:8, ]))
  expect_gg(ppd_dots(yrep[1,, drop = FALSE], quantiles = 25))
  expect_gg(ppd_dots(yrep[1:8, ], binwidth = 0.1))
  expect_gg(ppd_dots(yrep2, binwidth = 0.1, quantiles = 25))

})

test_that("ppc_pit_ecdf, ppc_pit_ecdf_grouped returns a ggplot object", {
  expect_gg(ppc_pit_ecdf(y, yrep, interpolate_adj = FALSE))
  expect_gg(ppc_pit_ecdf_grouped(y, yrep, group = group, interpolate_adj = FALSE))
  expect_message(ppc_pit_ecdf(pit = runif(100)), "'pit' specified")
  expect_message(
    ppc_pit_ecdf_grouped(pit = runif(length(group)), group = group, interpolate_adj = FALSE),
    "'pit' specified"
  )
})

test_that("ppc_freqpoly_grouped returns a ggplot object", {
  expect_gg(ppc_freqpoly_grouped(y, yrep[1:4, ], group))
  expect_gg(ppc_freqpoly_grouped(y, yrep[1:4, ], group,
                                 freq = TRUE, alpha = 0.5))

  # ppd versions
  expect_gg(ppd_freqpoly_grouped(yrep[1:4, ], group))
})

test_that("ppc_violin_grouped returns a ggplot object", {
  expect_gg(ppc_violin_grouped(y, yrep, group))
  expect_gg(ppc_violin_grouped(y, yrep, as.numeric(group)))
  expect_gg(ppc_violin_grouped(y, yrep, as.integer(group)))
  expect_gg(ppc_violin_grouped(y, yrep, group, y_draw = "both", y_jitter = 0.3))
})



# Visual tests -----------------------------------------------------------------

test_that("ppc_hist renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_hist(vdiff_y, vdiff_yrep[1:8, ])
  vdiffr::expect_doppelganger("ppc_hist (default)", p_base)

  p_binwidth <- ppc_hist(vdiff_y, vdiff_yrep[1:8, ], binwidth = 3)
  vdiffr::expect_doppelganger("ppc_hist (binwidth)", p_binwidth)

  # ppd versions
  p_base <- ppd_hist(vdiff_yrep[1:8, ])
  vdiffr::expect_doppelganger("ppd_hist (default)", p_base)

  p_binwidth <- ppd_hist(vdiff_yrep[1:8, ], binwidth = 3)
  vdiffr::expect_doppelganger("ppd_hist (binwidth)", p_binwidth)
})

test_that("ppc_freqpoly renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_freqpoly(vdiff_y, vdiff_yrep[1:8, ])
  vdiffr::expect_doppelganger("ppc_freqpoly (default)", p_base)

  p_custom <- ppc_freqpoly(
    y = vdiff_y,
    yrep = vdiff_yrep[1:8, ],
    binwidth = 2,
    size = 2,
    alpha = 0.1)
  vdiffr::expect_doppelganger(
    title = "ppc_freqpoly (alpha, binwidth, size)",
    fig = p_custom)

  # ppd versions
  p_base <- ppd_freqpoly(vdiff_yrep[1:8, ])
  vdiffr::expect_doppelganger("ppd_freqpoly (default)", p_base)

  p_custom <- ppd_freqpoly(
    ypred = vdiff_yrep[1:8, ],
    binwidth = 2,
    size = 2,
    alpha = 0.1)
  vdiffr::expect_doppelganger(
    title = "ppd_freqpoly (alpha, binwidth, size)",
    fig = p_custom)
})

test_that("ppc_freqpoly_grouped renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_freqpoly_grouped(vdiff_y, vdiff_yrep[1:3, ], vdiff_group)
  vdiffr::expect_doppelganger("ppc_freqpoly_grouped (default)", p_base)

  # ppd versions
  p_base <- ppd_freqpoly_grouped(vdiff_yrep[1:3, ], vdiff_group)
  vdiffr::expect_doppelganger("ppd_freqpoly_grouped (default)", p_base)
})

test_that("ppc_boxplot renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_boxplot(vdiff_y, vdiff_yrep[1:8, ])
  vdiffr::expect_doppelganger("ppc_boxplot (default)", p_base)

  p_no_notch <- ppc_boxplot(vdiff_y, vdiff_yrep[1:8, ], notch = FALSE)
  vdiffr::expect_doppelganger("ppc_boxplot (no notch)", p_no_notch)

  p_custom <- ppc_boxplot(vdiff_y, vdiff_yrep[1:8, ], size = 1.5, alpha = .5)
  vdiffr::expect_doppelganger("ppc_boxplot (alpha, size)", p_custom)

  # ppd versions
  p_base <- ppd_boxplot(vdiff_yrep[1:8, ])
  vdiffr::expect_doppelganger("ppd_boxplot (default)", p_base)

  p_no_notch <- ppd_boxplot(vdiff_yrep[1:8, ], notch = FALSE)
  vdiffr::expect_doppelganger("ppd_boxplot (no notch)", p_no_notch)

  p_custom <- ppd_boxplot(vdiff_yrep[1:8, ], size = 1.5, alpha = .5)
  vdiffr::expect_doppelganger("ppd_boxplot (alpha, size)", p_custom)
})

test_that("ppc_dots renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not_installed("ggdist")
  skip_on_r_oldrel()

  p_base <- ppc_dots(vdiff_y, vdiff_yrep[1:8, ])
  vdiffr::expect_doppelganger("ppc_dots (default)", p_base)

  p_binwidth <- ppc_dots(vdiff_y, vdiff_yrep[1:8, ], binwidth = 3)
  expect_warning(vdiffr::expect_doppelganger("ppc_dots (binwidth)", p_binwidth),
                 "The provided binwidth will cause dots to overflow the boundaries")

  p_quantile <- ppc_dots(vdiff_y, vdiff_yrep[1:8, ], quantiles = 50)
  vdiffr::expect_doppelganger("ppc_dots (quantile)", p_quantile)

  p_quantile_binwidth <- ppc_dots(vdiff_y, vdiff_yrep[1:8, ], binwidth = 3, quantiles = 50)
  expect_warning(vdiffr::expect_doppelganger("ppc_dots (quantile-binwidth)", p_quantile_binwidth),
                 "The provided binwidth will cause dots to overflow the boundaries")

  # ppd versions
  p_base <- ppd_dots(vdiff_yrep[1:8, ])
  vdiffr::expect_doppelganger("ppd_dots (default)", p_base)

  p_binwidth <- ppd_dots(vdiff_yrep[1:8, ], binwidth = 3)
  expect_warning(vdiffr::expect_doppelganger("ppd_dots (binwidth)", p_binwidth),
                 "The provided binwidth will cause dots to overflow the boundaries")

  p_quantile <- ppd_dots(vdiff_yrep[1:8, ], quantiles = 50)
  vdiffr::expect_doppelganger("ppd_dots (quantile)", p_quantile)

  p_quantile_binwidth <- ppd_dots(vdiff_yrep[1:8, ], binwidth = 3, quantiles = 50)
  expect_warning(vdiffr::expect_doppelganger("ppd_dots (quantile-binwidth)", p_quantile_binwidth),
                 "The provided binwidth will cause dots to overflow the boundaries")
})

test_that("ppc_ecdf_overlay renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_ecdf_overlay(vdiff_y2, vdiff_yrep2)
  vdiffr::expect_doppelganger("ppc_ecdf_overlay (default)", p_base)

  p_custom <- ppc_ecdf_overlay(
    vdiff_y2,
    vdiff_yrep2,
    discrete = TRUE,
    size = 2,
    alpha = .2
  )

  vdiffr::expect_doppelganger(
    "ppc_ecdf_overlay (discrete, size, alpha)",
    p_custom
  )
})

test_that("ppc_ecdf_overlay_grouped renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_ecdf_overlay_grouped(vdiff_y2, vdiff_yrep2, vdiff_group2)
  vdiffr::expect_doppelganger("ppc_ecdf_overlay_grouped (default)", p_base)

  p_custom <- ppc_ecdf_overlay_grouped(
    vdiff_y2,
    vdiff_yrep2,
    vdiff_group2,
    discrete = TRUE,
    size = 2,
    alpha = .2
  )

  vdiffr::expect_doppelganger(
    "ppc_ecdf_overlay_grouped (discrete, size, alpha)",
    p_custom
  )
})

test_that("ppc_dens renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_dens(vdiff_y, vdiff_yrep[1:8, ])
  vdiffr::expect_doppelganger("ppc_dens (default)", p_base)

  # ppd versions
  p_base <- ppd_dens(vdiff_yrep[1:8, ])
  vdiffr::expect_doppelganger("ppd_dens (default)", p_base)
})

test_that("ppc_dens_overlay renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_dens_overlay(vdiff_y, vdiff_yrep)
  vdiffr::expect_doppelganger("ppc_dens_overlay (default)", p_base)

  p_custom <- ppc_dens_overlay(vdiff_y, vdiff_yrep, size = 1, alpha = 0.2)
  vdiffr::expect_doppelganger("ppc_dens_overlay (alpha, size)", p_custom)

  # ppd versions
  p_base <- ppd_dens_overlay(vdiff_yrep)
  vdiffr::expect_doppelganger("ppd_dens_overlay (default)", p_base)

  p_custom <- ppd_dens_overlay(vdiff_yrep, size = 1, alpha = 0.2)
  vdiffr::expect_doppelganger("ppd_dens_overlay (alpha, size)", p_custom)
})

test_that("ppc_dens_overlay_grouped renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_dens_overlay_grouped(vdiff_y, vdiff_yrep, vdiff_group)
  vdiffr::expect_doppelganger("ppc_dens_overlay_grouped (default)", p_base)

  p_custom <- ppc_dens_overlay_grouped(
    vdiff_y,
    vdiff_yrep,
    vdiff_group,
    size = 1,
    alpha = 0.2
  )

  vdiffr::expect_doppelganger(
    "ppc_dens_overlay_grouped (alpha, size)",
    p_custom
  )
})

test_that("ppc_violin_grouped renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not(getRversion() >= "3.6.0")
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_violin_grouped(vdiff_y, vdiff_yrep, vdiff_group)
  vdiffr::expect_doppelganger("ppc_violin_grouped (default)", p_base)

  # lock in jitter
  set.seed(100)
  p_dots <- ppc_violin_grouped(
    y = vdiff_y,
    yrep = vdiff_yrep,
    group = vdiff_group,
    y_draw = "both")

  vdiffr::expect_doppelganger("ppc_violin_grouped (with points)", p_dots)

  p_dots_jitter <- ppc_violin_grouped(
    y = vdiff_y,
    yrep = vdiff_yrep,
    group = vdiff_group,
    y_draw = "points",
    y_jitter = 0.01)

  vdiffr::expect_doppelganger(
    "ppc_violin_grouped (points, low jitter)",
    p_dots_jitter)

  set.seed(seed = NULL)
})

test_that("ppc_pit_ecdf, ppc_pit_ecdf_grouped renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_pit_ecdf(y, yrep, interpolate_adj = FALSE)
  g_base <- ppc_pit_ecdf_grouped(y, yrep, group = group, interpolate_adj = FALSE)
  p_diff <- ppc_pit_ecdf(y, yrep, plot_diff = TRUE, interpolate_adj = FALSE)
  g_diff <- ppc_pit_ecdf_grouped(y, yrep, plot_diff = TRUE, group = group, interpolate_adj = FALSE)

  vdiffr::expect_doppelganger("ppc_pit_ecdf (default)", p_base)
  vdiffr::expect_doppelganger("ppc_pit_ecdf_grouped (default)", g_base)
  vdiffr::expect_doppelganger("ppc_pit_ecdf (diff)", p_diff)
  vdiffr::expect_doppelganger("ppc_pit_ecdf_grouped (diff)", g_diff)
})
