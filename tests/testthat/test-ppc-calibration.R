library(bayesplot)
context("PPC: calibration")

test_that("calibration interval helper matches prep confidence algorithm", {
  y <- c(0, 1, 0, 1)
  prep <- rbind(
    c(0.1, 0.8, 0.2, 0.9),
    c(0.3, 0.7, 0.4, 0.6)
  )

  m1 <- stats::isoreg(y[order(prep[1, ])])$yf
  m2 <- stats::isoreg(y[order(prep[2, ])])$yf
  m <- rbind(m1, m2)

  d <- bayesplot:::ppc_calibration_interval_data(
    y = y,
    prep = prep,
    prob = 0.8,
    interval = "confidence"
  )

  expect_equal(d$cep, colMeans(m))
  expect_equal(
    d$lb,
    unname(apply(m, 2, stats::quantile, probs = 0.1))
  )
  expect_equal(
    d$ub,
    unname(apply(m, 2, stats::quantile, probs = 0.9))
  )
})

test_that("calibration interval helper matches yrep consistency algorithm", {
  y <- c(0, 1, 0, 1)
  yrep <- rbind(
    c(0, 1, 0, 1),
    c(0, 1, 1, 1),
    c(0, 0, 0, 1)
  )

  p <- colMeans(yrep)
  ord <- order(p)
  m_obs <- stats::isoreg(y[ord])$yf
  m_rep <- t(apply(yrep[, ord, drop = FALSE], 1, function(z) stats::isoreg(z)$yf))

  d <- bayesplot:::ppc_calibration_interval_data(
    y = y,
    yrep = yrep,
    prob = 0.8,
    interval = "consistency"
  )

  expect_equal(d$value, p[ord])
  expect_equal(d$cep, m_obs)
  expect_equal(
    d$lb,
    unname(apply(m_rep, 2, stats::quantile, probs = 0.1))
  )
  expect_equal(
    d$ub,
    unname(apply(m_rep, 2, stats::quantile, probs = 0.9))
  )
})

test_that("ppc_calibration uses requested axis labels", {
  y <- c(0, 1, 0, 1)
  yrep <- rbind(
    c(0, 1, 0, 1),
    c(0, 1, 1, 1),
    c(0, 0, 0, 1)
  )

  p <- ppc_calibration(y = y, yrep = yrep, interval = "consistency")
  expect_gg(p)

  if ("get_labs" %in% getNamespaceExports("ggplot2")) {
    labs <- ggplot2::get_labs(p)
  } else {
    labs <- p$labels
  }
  expect_equal(labs$x, "predicted probability")
  expect_equal(labs$y, "conditional event probability")
})

test_that("ppc_calibration_data returns sorted values and isotonic cep", {
  y <- c(0, 0, 1, 1)
  prep <- rbind(
    c(0.4, 0.1, 0.3, 0.2),
    c(0.2, 0.3, 0.4, 0.1)
  )

  d <- ppc_calibration_data(y = y, prep = prep)

  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), length(y) * nrow(prep))
  expect_true(all(d$group == 1))

  for (s in seq_len(nrow(prep))) {
    ord <- order(prep[s, ])
    expected_value <- prep[s, ord]
    expected_cep <- stats::isoreg(y[ord])$yf
    d_s <- d[d$rep_id == s, , drop = FALSE]

    expect_equal(d_s$value, expected_value)
    expect_equal(d_s$cep, expected_cep)
  }
})
if (!exists("expect_gg")) expect_gg <- bayesplot:::expect_gg

# Create binary test data for calibration plots
set.seed(1234)
n_obs <- 1000
n_draws <- 1000
p_true <- runif(n_obs)
calib_y <- rbinom(n_obs, 1, p_true)
calib_prep <- matrix(pmin(1, pmax(0, p_true + rnorm(n_obs * n_draws, 0, .1))), nrow = n_draws, ncol = n_obs)
calib_yrep <- t(apply(calib_prep, 1, rbinom, n = n_obs, size = 1))

calib_group <- gl(2, n_obs / 2, labels = c("A", "B"))
calib_lw <- matrix(runif(n_obs * n_draws), ncol = n_obs, nrow = n_draws)

test_that("ppc_calibration_overlay returns a ggplot object", {
  expect_gg(ppc_calibration_overlay(calib_y, calib_prep))
  expect_gg(ppc_calibration_overlay(calib_y, calib_prep[1:5, ], 
                                   linewidth = 0.5, alpha = 0.3))
  expect_gg(ppc_calibration_overlay(calib_y, calib_prep, x_range = "data"))
})

test_that("ppc_calibration_overlay_grouped returns a ggplot object", {
  expect_gg(ppc_calibration_overlay_grouped(calib_y, calib_prep, calib_group))
  expect_gg(ppc_calibration_overlay_grouped(calib_y, calib_prep[1:3, ], calib_group,
                                           linewidth = 0.5, alpha = 0.3))
  expect_gg(ppc_calibration_overlay_grouped(
    calib_y,
    calib_prep,
    calib_group,
    x_range = "data"
  ))
})

test_that("ppc_calibration overlay functions support x_range", {
  y <- c(0, 1, 0, 1)
  prep <- rbind(
    c(0.20, 0.30, 0.40, 0.50),
    c(0.25, 0.35, 0.45, 0.55)
  )
  group <- c("A", "A", "B", "B")

  p_full <- ppc_calibration_overlay(y, prep, x_range = "full")
  p_data <- ppc_calibration_overlay(y, prep, x_range = "data")
  xr_full <- ggplot2::ggplot_build(p_full)$layout$panel_params[[1]]$x.range
  xr_data <- ggplot2::ggplot_build(p_data)$layout$panel_params[[1]]$x.range

  expect_equal(xr_full, c(0, 1))
  expect_equal(xr_data, range(prep))

  p_grouped_full <- ppc_calibration_overlay_grouped(y, prep, group, x_range = "full")
  p_grouped_data <- ppc_calibration_overlay_grouped(y, prep, group, x_range = "data")
  xr_grouped_full <- ggplot2::ggplot_build(p_grouped_full)$layout$panel_params[[1]]$x.range
  xr_grouped_data <- ggplot2::ggplot_build(p_grouped_data)$layout$panel_params[[1]]$x.range

  expect_equal(xr_grouped_full, c(0, 1))
  expect_equal(xr_grouped_data, range(prep))
})

test_that("ppc_calibration returns a ggplot object", {
  expect_gg(ppc_calibration(calib_y, prep=calib_prep))
  expect_gg(ppc_calibration(calib_y, yrep=calib_yrep))
  expect_gg(ppc_calibration(calib_y, calib_prep, prob = 0.9,
                           linewidth = 0.8, alpha = 0.5))
  expect_gg(ppc_calibration(calib_y, calib_prep))
  # Test new interval_type parameter
  expect_gg(ppc_calibration(y=calib_y, prep=calib_prep, 
    interval_type = "confidence"))
  expect_gg(ppc_calibration(y=calib_y, prep=calib_prep, 
    interval_type = "consistency"))
})

test_that("ppc_calibration adds quantile dot layer when requested", {
  testthat::skip_if_not_installed("ggdist")

  p_no_qdots <- ppc_calibration(
    y = calib_y,
    prep = calib_prep,
    show_qdots = FALSE
  )
  p <- ppc_calibration(
    calib_y,
    calib_prep,
    show_qdots = TRUE
  )

  expect_gg(p_no_qdots)
  expect_gg(p)
  expect_equal(length(p[["layers"]]), length(p_no_qdots[["layers"]]) + 1)
  y_min <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y.range[1]
  y_min_no_qdots <- ggplot2::ggplot_build(p_no_qdots)$layout$panel_params[[1]]$y.range[1]
  expect_lte(y_min, y_min_no_qdots)
})

test_that("ppc_calibration_grouped returns a ggplot object", {
  expect_gg(ppc_calibration_grouped(y = calib_y, prep = calib_prep,
    group = calib_group))
  expect_gg(ppc_calibration_grouped(y = calib_y, yrep = calib_yrep,
    group = calib_group))
  expect_gg(ppc_calibration_grouped(
    y = calib_y,
    prep = calib_prep[1:5, ],
    group = calib_group,
    prob = 0.9,
    linewidth = 0.8,
    alpha = 0.5
  ))
  # Test new interval_type parameter
  expect_gg(ppc_calibration_grouped(y = calib_y, prep = calib_prep,
    group = calib_group, interval_type = "confidence"))
  expect_gg(ppc_calibration_grouped(y = calib_y, prep = calib_prep,
    group = calib_group, interval_type = "consistency"))
})

test_that("ppc_loo_calibration returns a ggplot object", {
  # Note: This function now returns interval plots instead of overlay plots
  # Testing basic functionality for now
  expect_gg(ppc_loo_calibration(calib_y, calib_yrep, calib_prep))
  expect_gg(ppc_loo_calibration(calib_y, calib_yrep[1:3, ], calib_prep[1:3, ],
                               prob = 0.9, linewidth = 0.8, alpha = 0.5))
})

test_that("ppc_loo_calibration_grouped returns a ggplot object", {
  # Note: This function now returns interval plots instead of overlay plots
  # Testing basic functionality for now
  expect_gg(ppc_loo_calibration_grouped(
    y=calib_y, yrep=calib_prep, group=calib_group, lw=calib_lw)
  )
  expect_gg(ppc_loo_calibration_grouped(
    y=calib_y, yrep=calib_prep[1:3, ], group=calib_group, 
    lw=calib_lw[1:3, ])
  )
})

test_that("calibration functions handle edge cases", {
  # Single observation
  expect_gg(ppc_calibration_overlay(1, matrix(0.5, nrow = 1, ncol = 1)))
  expect_gg(ppc_calibration(1, matrix(0.5, nrow = 1, ncol = 1)))
  
  # Single draw
  expect_gg(ppc_calibration_overlay(calib_y, calib_prep[1, , drop = FALSE]))
  expect_gg(ppc_calibration(calib_y, calib_prep[1, , drop = FALSE]))
  
  # All zeros or ones
  expect_gg(ppc_calibration_overlay(rep(0, 10), matrix(0.1, nrow = 5, ncol = 10)))
  expect_gg(ppc_calibration_overlay(rep(1, 10), matrix(0.9, nrow = 5, ncol = 10)))
})

test_that("calibration functions validate inputs correctly", {
  # Invalid probabilities (outside [0,1])
  expect_error(
    ppc_calibration_overlay(calib_y, matrix(1.5, nrow = 5, ncol = n_obs)),
    "Values of 'prep' should be predictive probabilities between 0 and 1.",
    fixed = TRUE
  )
  expect_error(
    ppc_calibration_overlay(calib_y, matrix(-0.1, nrow = 5, ncol = n_obs)),
    "Values of 'prep' should be predictive probabilities between 0 and 1.",
    fixed = TRUE
  )
  
  # Mismatched dimensions
  expect_error(ppc_calibration_overlay(calib_y, calib_prep[, 1:25]),
               "ncol(yrep) must be equal to length(y).", fixed=TRUE)
  
  # Invalid group
  expect_error(ppc_calibration_overlay_grouped(calib_y, calib_prep, calib_group[1:25]),
               "length(group) must be equal to the number of observations.",
              fixed=TRUE)
  
  # Invalid interval_type
  expect_error(ppc_calibration(calib_y, calib_prep, interval_type = "invalid"),
               "should be one of")
  expect_error(ppc_calibration(calib_y, calib_prep, show_qdots = NA),
               "'show_qdots' must be a single TRUE or FALSE.",
               fixed = TRUE)
  expect_error(ppc_calibration(calib_y, calib_prep, qdots_quantiles = 0),
               "'qdots_quantiles' must be a positive integer.",
               fixed = TRUE)
})

test_that("calibration functions work with different group types", {
  # Numeric groups
  expect_gg(ppc_calibration_overlay_grouped(calib_y, calib_prep, as.numeric(calib_group)))
  expect_gg(ppc_calibration_grouped(
    y = calib_y,
    prep = calib_prep,
    group = as.numeric(calib_group)
  ))
  
  # Integer groups
  expect_gg(ppc_calibration_overlay_grouped(calib_y, calib_prep, as.integer(calib_group)))
  expect_gg(ppc_calibration_grouped(
    y = calib_y,
    prep = calib_prep,
    group = as.integer(calib_group)
  ))
  
  # Character groups
  expect_gg(ppc_calibration_overlay_grouped(calib_y, calib_prep, as.character(calib_group)))
  expect_gg(ppc_calibration_grouped(
    y = calib_y,
    prep = calib_prep,
    group = as.character(calib_group)
  ))
})

# Visual tests -----------------------------------------------------------------

test_that("ppc_calibration_overlay renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_calibration_overlay(calib_y, calib_prep)
  vdiffr::expect_doppelganger("ppc_calibration_overlay (default)", p_base)

  p_custom <- ppc_calibration_overlay(
    calib_y,
    calib_prep,
    prob = .99,
    linewidth = 0.5,
    alpha = 0.3
  )
  vdiffr::expect_doppelganger("ppc_calibration_overlay (custom)", p_custom)
})

test_that("ppc_calibration_overlay_grouped renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_calibration_overlay_grouped(calib_y, calib_prep, calib_group)
  vdiffr::expect_doppelganger("ppc_calibration_overlay_grouped (default)", p_base)

  p_custom <- ppc_calibration_overlay_grouped(
    calib_y,
    calib_prep,
    calib_group,
    prob = .99,
    linewidth = 0.5,
    alpha = 0.3
  )
  vdiffr::expect_doppelganger("ppc_calibration_overlay_grouped (custom)", p_custom)
})

test_that("ppc_calibration renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_calibration(calib_y, calib_prep)
  vdiffr::expect_doppelganger("ppc_calibration (default)", p_base)

  p_custom <- ppc_calibration(
    calib_y,
    calib_prep,
    prob = 0.9,
    linewidth = 0.8,
    alpha = 0.5
  )
  vdiffr::expect_doppelganger("ppc_calibration (custom)", p_custom)
  
  # Test interval_type variants
  p_confidence <- ppc_calibration(
    calib_y,
    calib_prep,
    interval_type = "confidence"
  )
  vdiffr::expect_doppelganger("ppc_calibration (confidence)", p_confidence)
  
  p_consistency <- ppc_calibration(
    calib_y,
    calib_prep,
    interval_type = "consistency"
  )
  vdiffr::expect_doppelganger("ppc_calibration (consistency)", p_consistency)
})

test_that("ppc_calibration_grouped renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_calibration_grouped(y = calib_y, prep = calib_prep, group = calib_group)
  vdiffr::expect_doppelganger("ppc_calibration_grouped (default)", p_base)

  p_custom <- ppc_calibration_grouped(
    y = calib_y,
    prep = calib_prep,
    group = calib_group,
    prob = 0.9,
    linewidth = 0.8,
    alpha = 0.5
  )
  vdiffr::expect_doppelganger("ppc_calibration_grouped (custom)", p_custom)
  
  # Test interval_type variants
  p_confidence <- ppc_calibration_grouped(
    y = calib_y,
    prep = calib_prep,
    group = calib_group,
    interval_type = "confidence"
  )
  vdiffr::expect_doppelganger("ppc_calibration_grouped (confidence)", p_confidence)
  
  p_consistency <- ppc_calibration_grouped(
    y = calib_y,
    prep = calib_prep,
    group = calib_group,
    interval_type = "consistency"
  )
  vdiffr::expect_doppelganger("ppc_calibration_grouped (consistency)", p_consistency)
})

test_that("ppc_loo_calibration renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_loo_calibration(calib_y, calib_yrep, calib_lw)
  vdiffr::expect_doppelganger("ppc_loo_calibration (default)", p_base)

  p_custom <- ppc_loo_calibration(
    calib_y,
    calib_yrep,
    calib_lw,
    linewidth = 0.5,
    alpha = 0.3
  )
  vdiffr::expect_doppelganger("ppc_loo_calibration (custom)", p_custom)
})

test_that("ppc_loo_calibration_grouped renders correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  skip_on_r_oldrel()

  p_base <- ppc_loo_calibration_grouped(
    y = calib_y,
    yrep = calib_yrep,
    lw = calib_lw,
    group = calib_group
  )
  vdiffr::expect_doppelganger("ppc_loo_calibration_grouped (default)", p_base)

  p_custom <- ppc_loo_calibration_grouped(
    y = calib_y,
    yrep = calib_yrep,
    lw = calib_lw,
    group = calib_group,
    linewidth = 0.5,
    alpha = 0.3
  )
  vdiffr::expect_doppelganger("ppc_loo_calibration_grouped (custom)", p_custom)
})

test_that("ppc_calibration computes yrep confidence interval bands", {
  set.seed(1001)
  y <- c(0, 1, 0, 1, 0, 1)
  yrep <- rbind(
    c(0, 1, 0, 1, 0, 1),
    c(0, 1, 1, 1, 0, 1),
    c(0, 0, 0, 1, 0, 1),
    c(1, 1, 0, 1, 0, 1)
  )

  p <- ppc_calibration(
    y = y,
    yrep = yrep,
    interval = "confidence",
    B = 50
  )

  expect_gg(p)
  expect_named(p$data, c("y_id", "value", "cep", "lb", "ub"))
  expect_equal(nrow(p$data), length(y))
  expect_lte(max(p$data$ub), 1)
  expect_gte(min(p$data$lb), 0)
})

test_that("ppc_calibration recovers identity trend for calibrated data", {
  set.seed(20260408)
  n_obs <- 10000
  n_draws <- 1000
  p_true <- runif(n_obs)
  y <- stats::rbinom(n_obs, size = 1, prob = p_true)
  yrep <- t(vapply(
    seq_len(n_draws),
    function(i) stats::rbinom(n_obs, size = 1, prob = p_true),
    numeric(n_obs)
  ))

  vdiffr::expect_doppelganger("ppc_calibration consistent_model",
    ppc_calibration(
      y = y,
      yrep = yrep,
      interval = "consistency"
    )
  )
})


test_that("test-data-2 from paper", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed(c("vdiffr", "rstanarm"))

  library(rstanarm)
  SEED <- 236543
  set.seed(SEED)
  data(roaches)
  # Roach1 is very skewed and we take a square root
  roaches$sqrt_roach1 <- sqrt(roaches$roach1)
  n <- length(roaches$y)
  
  stan_glmnb <- stan_glm(
    y ~ sqrt_roach1 + treatment + senior,
    offset = log(exposure2),
    data = roaches,
    family = neg_binomial_2,
    prior = normal(0, 2.5),
    prior_intercept = normal(0, 5),
    chains = 4,
    cores = 1,
    seed = SEED,
    refresh = 0
  )
  pp <- pmin(posterior_predict(stan_glmnb),1)
  
  vdiffr::expect_doppelganger("ppc_calibration roaches negbinomial",
    ppc_calibration(
      y = pmin(roaches$y, 1),
      yrep = pp,
      interval_type = "consistency",
      prob = 0.95,
      x_range = "full"
    )
  )

  vdiffr::expect_doppelganger("ppc_calibration consistency x-range=data",
    ppc_calibration(
      y = pmin(roaches$y, 1),
      yrep = pp,
      interval_type = "confidence",
      prob = 0.95,
      x_range = "data"
    )
  )

  vdiffr::expect_doppelganger("ppc_calibration confidence x-range=data",
    ppc_calibration(
      y = pmin(roaches$y, 1),
      yrep = pp,
      interval_type = "confidence",
      prob = 0.95,
      x_range = "data"
    )
  )
  
  vdiffr::expect_doppelganger("ppc_calibration confidence x-range=full",
    ppc_calibration(
      y = pmin(roaches$y, 1),
      yrep = pp,
      interval_type = "confidence",
      prob = 0.95,
      x_range = "full"
    )
  )
})

test_that("test-data-2 from paper", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("brms")

  library(brms)
  SEED <- 236543
  set.seed(SEED)
  data(roaches)

  roaches$sqrt_roach1 <- sqrt(roaches$roach1)
  n <- length(roaches$y)

  brm_glmzinb <-
    brms::brm(bf(y ~ sqrt_roach1 + treatment + senior + offset(log(exposure2)),
          zi ~ sqrt_roach1 + treatment + senior + offset(log(exposure2))),
        family = zero_inflated_negbinomial(),
        data = roaches,
        prior = c(brms::prior(normal(0,3), class = 'b'),
                brms::prior(normal(0,3), class = 'b', dpar = 'zi'),
                brms::prior(normal(0,3), class = 'Intercept', dpar = 'zi')),
        seed = SEED,
        refresh = 0)
  
    vdiffr::expect_doppelganger("roaches zero-inflated negbinomial",
      ppc_calibration(
        y = pmin(roaches$y, 1),
        yrep = apply(posterior_predict(brm_glmzinb), 2, pmin, 1),
        interval_type = "consistency",
        prob = 0.95,
        x_range = "data"
      )
    )
})
