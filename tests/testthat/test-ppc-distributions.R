source(test_path("data-for-ppc-tests.R"))

test_that("ppc_dens_overlay returns a ggplot object", {
  expect_gg(ppc_dens_overlay(y, yrep))
  expect_gg(ppc_dens_overlay(y2, yrep2, size = 0.5, alpha = 0.2))

  # ppd versions
  expect_gg(ppd_dens_overlay(yrep))
  expect_gg(ppd_dens_overlay(yrep2, size = 0.5, alpha = 0.2))
})

test_that("density PPC/PPD plots accept bounds", {
  suppressWarnings(expect_gg(ppc_dens(y, yrep[1:8, ], bounds = c(0, Inf))))
  suppressWarnings(expect_gg(ppc_dens_overlay(y, yrep, bounds = c(0, Inf))))
  suppressWarnings(expect_gg(ppc_dens_overlay_grouped(y, yrep, group = group, bounds = c(0, Inf))))
  suppressWarnings(expect_gg(ppd_dens(yrep[1:8, ], bounds = c(0, Inf))))
  suppressWarnings(expect_gg(ppd_dens_overlay(yrep, bounds = c(0, Inf))))
})

test_that("density PPC/PPD plots reject invalid bounds", {
  # non-numeric bounds
  expect_error(ppc_dens_overlay(y, yrep, bounds = c("a", "b")),
               "`bounds` must be a numeric vector of length 2")

  # bounds with length != 2
  expect_error(ppc_dens_overlay(y, yrep, bounds = c(0, 1, 2)),
               "`bounds` must be a numeric vector of length 2")
  expect_error(ppc_dens_overlay(y, yrep, bounds = 1),
               "`bounds` must be a numeric vector of length 2")

  # bounds with NA values
  expect_error(ppc_dens_overlay(y, yrep, bounds = c(0, NA)),
               "`bounds` must be a numeric vector of length 2")
  expect_error(ppc_dens_overlay(y, yrep, bounds = c(NA, 1)),
               "`bounds` must be a numeric vector of length 2")

  # bounds where bounds[1] >= bounds[2]
  expect_error(ppc_dens_overlay(y, yrep, bounds = c(1, 0)),
               "`bounds` must satisfy bounds\\[1\\] < bounds\\[2\\]")
  expect_error(ppc_dens_overlay(y, yrep, bounds = c(1, 1)),
               "`bounds` must satisfy bounds\\[1\\] < bounds\\[2\\]")
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
    expect_equal(p$facet$params$facets[[1]], facet_var[[1]], ignore_function_env = TRUE, ignore_formula_env = TRUE)
  }

  # ppd versions
  expect_gg(ppd_hist(yrep[1,, drop = FALSE], binwidth = 0.1))
  expect_gg(ppd_hist(yrep[1:8, ], binwidth = 0.1))
  expect_gg(ppd_hist(yrep2, binwidth = 0.1))

  expect_gg(ppd_dens(yrep[1:8, ]))
  expect_gg(ppd_dens(yrep2))

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
  # Independent method (default)
  expect_gg(ppc_pit_ecdf(y, yrep, interpolate_adj = FALSE))
  expect_gg(ppc_pit_ecdf(y, yrep, method = "independent", interpolate_adj = FALSE))
  expect_gg(ppc_pit_ecdf_grouped(y, yrep, group = group, interpolate_adj = FALSE))

  # Correlated method
  expect_gg(ppc_pit_ecdf(y, yrep, method = "correlated"))
  expect_gg(ppc_pit_ecdf(y, yrep, method = "correlated", plot_diff = TRUE))
  expect_gg(ppc_pit_ecdf(y, yrep, method = "correlated", test = "PRIT"))
  expect_gg(ppc_pit_ecdf(y, yrep, method = "correlated", test = "PIET"))
  expect_gg(ppc_pit_ecdf_grouped(y, yrep, group = group, method = "correlated"))
  expect_gg(ppc_pit_ecdf_grouped(y, yrep, group = group, method = "correlated", plot_diff = TRUE))
  expect_gg(ppc_pit_ecdf_grouped(y, yrep, group = group, method = "correlated", test = "PRIT"))
  expect_gg(ppc_pit_ecdf_grouped(y, yrep, group = group, method = "correlated", test = "PIET"))

  # Specify 'pit' directly (with y/yrep still supplied)
  expect_message(
    ppc_pit_ecdf_grouped(
      y = y, yrep = yrep, pit = runif(length(group)),
      group = group, interpolate_adj = FALSE
    ),
    "'pit' specified"
  )

  # No y/yrep provided with pit -> no ignored-input message
  expect_no_message(
    ppc_pit_ecdf_grouped(pit = runif(length(group)), group = group, interpolate_adj = FALSE)
  )
})

test_that("ppc_pit_ecdf method validation and ignored-argument warnings", {
  # Invalid method
  expect_error(ppc_pit_ecdf(y, yrep, method = "bogus"))

  # method = "correlated" warns about interpolate_adj
  expect_message(
    ppc_pit_ecdf(y, yrep, method = "correlated", interpolate_adj = TRUE),
    "ignoring.*interpolate_adj"
  )

  # method = "independent" warns about test and gamma
  expect_message(
    ppc_pit_ecdf(y, yrep, method = "independent", test = "POT",
                 interpolate_adj = FALSE),
    "ignoring.*test"
  )
  expect_message(
    ppc_pit_ecdf(y, yrep, method = "independent", test = "POT", gamma = 0.5,
                 interpolate_adj = FALSE),
    "ignoring.*test, gamma"
  )

  # Invalid test type for correlated
  expect_error(
    ppc_pit_ecdf(y, yrep, method = "correlated", test = "INVALID")
  )
})

test_that("ppc_pit_ecdf correlated method validates gamma", {
  expect_error(
    ppc_pit_ecdf(y, yrep, method = "correlated", gamma = -1),
    regexp = "gamma must be in"
  )
})

test_that("ppc_pit_ecdf_grouped method validation and ignored-argument warnings", {
  # Invalid method
  expect_error(ppc_pit_ecdf_grouped(y, yrep, group = group, method = "bogus"))

  # method = "correlated" warns about interpolate_adj
  expect_message(
    ppc_pit_ecdf_grouped(
      y, yrep, group = group, method = "correlated", interpolate_adj = TRUE
    ),
    "ignoring.*interpolate_adj"
  )

  # method = "independent" warns about correlated-only args
  expect_message(
    ppc_pit_ecdf_grouped(
      y, yrep, group = group, method = "independent",
      test = "POT", interpolate_adj = FALSE
    ),
    "ignoring.*test"
  )
  expect_message(
    ppc_pit_ecdf_grouped(
      y, yrep, group = group, method = "independent",
      test = "POT", gamma = 0.5, interpolate_adj = FALSE
    ),
    "ignoring.*test, gamma"
  )

  # Invalid test type for correlated
  expect_error(
    ppc_pit_ecdf_grouped(y, yrep, group = group, method = "correlated", test = "INVALID")
  )
})

test_that("ppc_pit_ecdf_grouped correlated method validates gamma", {
  expect_error(
    ppc_pit_ecdf_grouped(y, yrep, group = group, method = "correlated", gamma = -1),
    regexp = "gamma must be in"
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

# ppc_data / ppd_data tests -----------------------------------------------

test_that("ppc_data returns the correct structure", {
  y_small <- c(10, 20)
  yrep_small <- rbind(c(11, 21), c(12, 22))

  d <- ppc_data(y_small, yrep_small)

  expect_s3_class(d, "data.frame")
  expect_named(d, c("y_id", "y_name", "rep_id", "rep_label",
                    "is_y", "is_y_label", "value"))
  expect_equal(nrow(d), length(y_small) * (nrow(yrep_small) + 1))
  expect_equal(d$y_id, c(1L, 1L, 2L, 2L, 1L, 2L))
  expect_equal(as.character(d$y_name), c("1", "1", "2", "2", "1", "2"))
  expect_equal(d$rep_id, c(1L, 2L, 1L, 2L, NA, NA))
  expect_equal(d$is_y, c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(d$value, c(11, 12, 21, 22, 10, 20))
  expect_equal(d$value[d$is_y], y_small)

  first_level <- levels(d$rep_label)[1]
  expect_true(all(as.character(d$rep_label[d$is_y]) == first_level))
  expect_true(all(as.character(d$rep_label[!d$is_y]) != first_level))
})

test_that("ppc_data carries group through correctly", {
  y_small <- c(10, 20)
  yrep_small <- rbind(c(11, 21), c(12, 22))
  group_small <- factor(c("a", "b"))

  d <- ppc_data(y_small, yrep_small, group = group_small)

  expect_named(d, c("group", "y_id", "y_name", "rep_id", "rep_label",
                    "is_y", "is_y_label", "value"))
  expect_equal(as.character(d$group), c("a", "a", "b", "b", "a", "b"))
  expect_equal(as.character(d$group[d$is_y]), as.character(group_small))
})

test_that("ppc_data handles a single replicate matrix", {
  y_small <- c(10, 20)
  yrep_small <- matrix(c(11, 21), nrow = 1)

  d <- ppc_data(y_small, yrep_small)

  expect_equal(sum(!d$is_y), length(y_small))
  expect_equal(d$rep_id[!d$is_y], c(1L, 1L))
  expect_equal(d$value[!d$is_y], c(11, 21))
})

test_that("ppd_data returns the correct structure", {
  yrep_small <- rbind(c(11, 21), c(12, 22))

  d <- ppd_data(yrep_small)

  expect_s3_class(d, "data.frame")
  expect_named(d, c("y_id", "y_name", "rep_id", "rep_label", "value"))
  expect_equal(nrow(d), nrow(yrep_small) * ncol(yrep_small))
  expect_equal(d$y_id, c(1L, 1L, 2L, 2L))
  expect_equal(as.character(d$y_name), c("1", "1", "2", "2"))
  expect_equal(d$rep_id, c(1L, 2L, 1L, 2L))
  expect_equal(d$value, c(11, 12, 21, 22))
  expect_true(all(grepl("pred", levels(d$rep_label), fixed = TRUE)))
})

test_that("ppd_data carries group through correctly", {
  yrep_small <- rbind(c(11, 21), c(12, 22))
  group_small <- factor(c("a", "b"))

  d <- ppd_data(yrep_small, group = group_small)

  expect_named(d, c("group", "y_id", "y_name", "rep_id", "rep_label", "value"))
  expect_equal(as.character(d$group), c("a", "a", "b", "b"))
})

test_that("ppd_data carries observation names through to y_name", {
  yrep_named <- rbind(c(11, 21), c(12, 22))
  colnames(yrep_named) <- c("obs_a", "obs_b")

  d <- ppd_data(yrep_named)

  expect_equal(as.character(d$y_name), c("obs_a", "obs_a", "obs_b", "obs_b"))
})

test_that("ppd_data handles a single replicate matrix", {
  yrep_small <- matrix(c(11, 21), nrow = 1)

  d <- ppd_data(yrep_small)

  expect_equal(nrow(d), ncol(yrep_small))
  expect_equal(d$rep_id, c(1L, 1L))
  expect_equal(d$value, c(11, 21))
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
  suppressWarnings(expect_warning(vdiffr::expect_doppelganger("ppc_dots (binwidth)", p_binwidth),
                 "The provided binwidth will cause dots to overflow the boundaries"))

  p_quantile <- ppc_dots(vdiff_y, vdiff_yrep[1:8, ], quantiles = 50)
  vdiffr::expect_doppelganger("ppc_dots (quantile)", p_quantile)

  p_quantile_binwidth <- ppc_dots(vdiff_y, vdiff_yrep[1:8, ], binwidth = 3, quantiles = 50)
  suppressWarnings(expect_warning(vdiffr::expect_doppelganger("ppc_dots (quantile-binwidth)", p_quantile_binwidth),
                 "The provided binwidth will cause dots to overflow the boundaries"))

  # ppd versions
  p_base <- ppd_dots(vdiff_yrep[1:8, ])
  vdiffr::expect_doppelganger("ppd_dots (default)", p_base)

  p_binwidth <- suppressWarnings(ppd_dots(vdiff_yrep[1:8, ], binwidth = 3))
  suppressWarnings(expect_warning(vdiffr::expect_doppelganger("ppd_dots (binwidth)", p_binwidth),
                 "The provided binwidth will cause dots to overflow the boundaries"))

  p_quantile <- ppd_dots(vdiff_yrep[1:8, ], quantiles = 50)
  vdiffr::expect_doppelganger("ppd_dots (quantile)", p_quantile)

  p_quantile_binwidth <- suppressWarnings(ppd_dots(vdiff_yrep[1:8, ], binwidth = 3, quantiles = 50))
  suppressWarnings(expect_warning(vdiffr::expect_doppelganger("ppd_dots (quantile-binwidth)", p_quantile_binwidth),
                 "The provided binwidth will cause dots to overflow the boundaries"))
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

  p_bounds <- suppressWarnings(ppc_dens_overlay(vdiff_y, vdiff_yrep, bounds = c(0, Inf)))
  suppressWarnings(vdiffr::expect_doppelganger("ppc_dens_overlay (bounds)", p_bounds))

  # ppd versions
  p_base <- ppd_dens_overlay(vdiff_yrep)
  vdiffr::expect_doppelganger("ppd_dens_overlay (default)", p_base)

  p_custom <- ppd_dens_overlay(vdiff_yrep, size = 1, alpha = 0.2)
  vdiffr::expect_doppelganger("ppd_dens_overlay (alpha, size)", p_custom)

  p_bounds <- suppressWarnings(ppd_dens_overlay(vdiff_yrep, bounds = c(0, Inf)))
  suppressWarnings(vdiffr::expect_doppelganger("ppd_dens_overlay (bounds)", p_bounds))
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

  p_probs <- ppc_violin_grouped(
    y = vdiff_y,
    yrep = vdiff_yrep,
    group = vdiff_group,
    y_draw = "points",
    probs = c(0.1, 0.9)
  )
  vdiffr::expect_doppelganger(
    "ppc_violin_grouped (points, probs)",
    p_probs)

  set.seed(seed = NULL)
})

test_that("ppc_pit_ecdf, ppc_pit_ecdf_grouped renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  # Independent method
  p_base <- ppc_pit_ecdf(y, yrep, interpolate_adj = FALSE)
  g_base <- ppc_pit_ecdf_grouped(y, yrep, group = group, interpolate_adj = FALSE)
  p_diff <- ppc_pit_ecdf(y, yrep, plot_diff = TRUE, interpolate_adj = FALSE)
  g_diff <- ppc_pit_ecdf_grouped(y, yrep, plot_diff = TRUE, group = group, interpolate_adj = FALSE)

  vdiffr::expect_doppelganger("ppc_pit_ecdf (default)", p_base)
  vdiffr::expect_doppelganger("ppc_pit_ecdf_grouped (default)", g_base)
  vdiffr::expect_doppelganger("ppc_pit_ecdf (diff)", p_diff)
  vdiffr::expect_doppelganger("ppc_pit_ecdf_grouped (diff)", g_diff)

  # Correlated method
  p_corr <- ppc_pit_ecdf(y, yrep, method = "correlated")
  vdiffr::expect_doppelganger("ppc_pit_ecdf (correlated)", p_corr)

  p_corr_diff <- ppc_pit_ecdf(y, yrep, method = "correlated", plot_diff = TRUE)
  vdiffr::expect_doppelganger("ppc_pit_ecdf (correlated diff)", p_corr_diff)

  p_corr_prit <- ppc_pit_ecdf(y, yrep, method = "correlated", test = "PRIT")
  vdiffr::expect_doppelganger("ppc_pit_ecdf (correlated PRIT)", p_corr_prit)

  p_corr_piet <- ppc_pit_ecdf(y, yrep, method = "correlated", test = "PIET")
  vdiffr::expect_doppelganger("ppc_pit_ecdf (correlated PIET)", p_corr_piet)

  g_corr <- ppc_pit_ecdf_grouped(y, yrep, group = group, method = "correlated")
  vdiffr::expect_doppelganger("ppc_pit_ecdf_grouped (correlated)", g_corr)

  g_corr_diff <- ppc_pit_ecdf_grouped(
    y, yrep, group = group, method = "correlated", plot_diff = TRUE
  )
  vdiffr::expect_doppelganger("ppc_pit_ecdf_grouped (correlated diff)", g_corr_diff)
})

test_that("ppc_pit_ecdf with method correlated renders different tests correctly", {
  set.seed(2025)
  pit <- 1 - (1 - runif(300))^(1.2)
  
  p_cor_pot <- ppc_pit_ecdf(
    pit = pit, 
    method = "correlated"
  )
  vdiffr::expect_doppelganger("ppc_pit_ecdf (correlated pot)", p_cor_pot)

  p_cor_prit <- ppc_pit_ecdf(
    pit = pit, 
    method = "correlated", 
    test = "PRIT"
  )
  vdiffr::expect_doppelganger("ppc_pit_ecdf (correlated prit 2)", p_cor_prit)

  p_cor_piet <- ppc_pit_ecdf(
    pit = pit, 
    method = "correlated", 
    test = "PIET"
  )
  vdiffr::expect_doppelganger("ppc_pit_ecdf (correlated piet 2)", p_cor_piet)
})

test_that("ppc_pit_ecdf with plot_diff=TRUE and method correlated renders different tests correctly", {
  set.seed(2025)
  pit <- 1 - (1 - runif(300))^(1.2)
  
  p_cor_pot <- ppc_pit_ecdf(
    pit = pit, 
    method = "correlated",
    plot_diff = TRUE
  )
  vdiffr::expect_doppelganger("ppc_pit_ecdf (diff, correlated pot)", p_cor_pot)

  p_cor_prit <- ppc_pit_ecdf(
    pit = pit, 
    method = "correlated", 
    test = "PRIT",
    plot_diff = TRUE
  )
  vdiffr::expect_doppelganger("ppc_pit_ecdf (diff, correlated prit)", p_cor_prit)

  p_cor_piet <- ppc_pit_ecdf(
    pit = pit, 
    method = "correlated", 
    test = "PIET",
    plot_diff = TRUE
  )
  vdiffr::expect_doppelganger("ppc_pit_ecdf (diff, correlated piet)", p_cor_piet)
})

test_that("ppc_pit_ecdf renders different linewidths and colors correctly", {
  set.seed(2025)
  pit <- 1 - (1 - runif(300))^(1.2)
  
  p_cor_lw1 <- ppc_pit_ecdf(
    pit = pit, 
    method = "correlated",
    linewidth = 1.
  )
  vdiffr::expect_doppelganger("ppc_pit_ecdf (linewidth = 1)", p_cor_lw1)

  p_cor_lw2 <- ppc_pit_ecdf(
    pit = pit, 
    method = "correlated",
    linewidth = 2.
  )
  vdiffr::expect_doppelganger("ppc_pit_ecdf (linewidth = 2)", p_cor_lw2)

  p_cor_col <- ppc_pit_ecdf(
    pit = pit, 
    method = "correlated",
    color = c(ecdf = "darkblue", highlight = "red")
  )
  vdiffr::expect_doppelganger("ppc_pit_ecdf (color change)", p_cor_col)
})


# Test PIT computation branches ------------------------------------------------
# use monkey-patching to test whether the correct branch of the 
# PIT computation is taken 

testthat::test_that("ppc_pit_ecdf takes correct PIT computation branch", {
  skip_on_cran()
  skip_if_not_installed("loo")
  skip_on_r_oldrel()
  skip_if(packageVersion("rstantools") <= "2.4.0")

  ppc_pit_ecdf_patched <- ppc_pit_ecdf

  body(ppc_pit_ecdf_patched)[[
    # Replace the PIT computation block (the large if/else if/else)
    # with a version that emits diagnostics
    which(sapply(as.list(body(ppc_pit_ecdf)), function(e) {
      is.call(e) && deparse(e[[1]]) == "if" &&
        grepl("pareto_pit", deparse(e[[2]]))
    }))
  ]] <- quote({

    if (isTRUE(pareto_pit) && is.null(pit)) {
      message("[PIT BRANCH] Pareto-smoothed LOO PIT")
      suggested_package("rstantools")
      y    <- validate_y(y)
      yrep <- validate_predictions(yrep, length(y))

      pit  <- posterior::pareto_pit(x = yrep, y = y, weights = NULL, log = TRUE)
      K    <- K %||% length(pit)

    } else if (!is.null(pit)) {
      message("[PIT BRANCH] Pre-supplied PIT")
      pit <- validate_pit(pit)
      K   <- K %||% length(pit)
      
      ignored <- c(
        if (!missing(y)    && !is.null(y))    "y",
        if (!missing(yrep) && !is.null(yrep)) "yrep"
      )
      if (length(ignored) > 0) {
        inform(paste0("As 'pit' specified; ignoring: ",
                      paste(ignored, collapse = ", "), "."))
      }

    } else {
      message("[PIT BRANCH] Empirical PIT")
      pit <- ppc_data(y, yrep) %>%
        group_by(.data$y_id) %>%
        dplyr::group_map(
          ~ mean(.x$value[.x$is_y] > .x$value[!.x$is_y]) +
          runif(1, max = mean(.x$value[.x$is_y] == .x$value[!.x$is_y]))
        ) %>%
        unlist()
      K <- K %||% min(nrow(yrep) + 1, 1000)
    }
  })

  # | yrep | y | pit | method      | test | pareto_pit | approach           |
  # |------|---|-----|-------------|------|------------|--------------------|
  # | x    | x |     | independent | NULL | FALSE      | empirical pit      |
  # |      |   | x   | independent | NULL | FALSE      |                    |
  # | x    | x |     | independent | NULL | TRUE       | compute pareto-pit |
  # | x    | x |     | correlated  | POT  | TRUE       | compute pareto-pit |
  # |      |   | x   | correlated  | POT  | FALSE      |                    |
  # | x    | x |     | correlated  | PIET | TRUE       | compute pareto-pit |
  # |      |   | x   | correlated  | PIET | FALSE      |                    |
  # | x    | x |     | correlated  | PRIT | FALSE      | empirical pit      |
  # |      |   | x   | correlated  | PRIT | FALSE      |                    |

  pits <- rstantools::loo_pit(vdiff_loo_yrep, vdiff_loo_y, vdiff_loo_lw)

  # method = independent ------------------------------------------
  expect_message(
    ppc_pit_ecdf_patched(
      vdiff_loo_y,
      vdiff_loo_yrep,
      method = "independent"
    ),
    regexp = "\\[PIT BRANCH\\] Empirical PIT"
  )

  expect_message(
    ppc_pit_ecdf_patched(
      vdiff_loo_y,
      vdiff_loo_yrep,
      method = "independent",
      pareto_pit = TRUE
    ),
    regexp = "\\[PIT BRANCH\\] Pareto-smoothed LOO PIT"
  )

  expect_message(
    ppc_pit_ecdf_patched(
      method = "independent",
      pit = pits,
    ),
    regexp = "\\[PIT BRANCH\\] Pre-supplied PIT"
  )

  # method = correlated + POT test -------------------------------
  expect_message(
    ppc_pit_ecdf_patched(
      vdiff_loo_y,
      vdiff_loo_yrep,
      method = "correlated"
    ),
    regexp = "\\[PIT BRANCH\\] Pareto-smoothed LOO PIT"
  )

  expect_message(
    ppc_pit_ecdf_patched(
      vdiff_loo_y,
      vdiff_loo_yrep,
      method = "correlated",
      pareto_pit = FALSE
    ),
    regexp = "\\[PIT BRANCH\\] Empirical PIT"
  )

  expect_message(
    ppc_pit_ecdf_patched(
      method = "correlated",
      pit = pits,
    ),
    regexp = "\\[PIT BRANCH\\] Pre-supplied PIT"
  )

  # method = correlated + PIET test -------------------------------
  expect_message(
    ppc_pit_ecdf_patched(
      vdiff_loo_y,
      vdiff_loo_yrep,
      method = "correlated",
      test = "PIET"
    ),
    regexp = "\\[PIT BRANCH\\] Pareto-smoothed LOO PIT"
  )

  expect_message(
    ppc_pit_ecdf_patched(
      method = "correlated",
      test = "PIET",
      pit = pits,
    ),
    regexp = "\\[PIT BRANCH\\] Pre-supplied PIT"
  )

  # method = correlated + PRIT test -------------------------------
  expect_message(
    ppc_pit_ecdf_patched(
      vdiff_loo_y,
      vdiff_loo_yrep,
      method = "correlated",
      test = "PRIT"
    ),
    regexp = "\\[PIT BRANCH\\] Empirical PIT"
  )

  expect_message(
    ppc_pit_ecdf_patched(
      method = "correlated",
      test = "PRIT",
      pit = pits,
    ),
    regexp = "\\[PIT BRANCH\\] Pre-supplied PIT"
  )
})

test_that("ppc_pit_ecdf works with pareto_pit method", {
  skip_if_not_installed("brms")
  skip_if_not_installed("rstanarm")

  data("roaches", package = "rstanarm")
  roaches$sqrt_roach1 <- sqrt(roaches$roach1)

  fit_p <- brms::brm(y ~ sqrt_roach1 + treatment + senior + offset(log(exposure2)),
             data = roaches,
             family = poisson,
             prior = brms::prior(normal(0, 1), class = b),
             refresh = 0)
  
  fit_p <- brms::add_criterion(fit_p, criterion = "loo")
  fit_p <- brms::add_criterion(fit_p, criterion = "loo", moment_match = TRUE, overwrite = TRUE)
  fit_nb <- update(fit_p, family = brms::negbinomial)

  expect_gg(brms::pp_check(fit_nb, type = "pit_ecdf"))

  draws <- brms::posterior_predict(fit_nb)
  y <- roaches$y

  expect_gg(ppc_pit_ecdf(
    y = y, yrep = draws, method = "correlated"
  ))

  expect_gg(brms::pp_check(fit_nb, type = "pit_ecdf", method = "correlated"))
})