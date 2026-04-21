options(useFancyQuotes = FALSE)

if (requireNamespace("rstanarm", quietly = TRUE) &&
    requireNamespace("loo", quietly = TRUE)) {
  suppressPackageStartupMessages(library(rstanarm))
  suppressPackageStartupMessages(library(loo))
  ITER <- 1000
  CHAINS <- 3
  fit <- stan_glm(mpg ~ wt + am, data = mtcars,
                  iter = ITER, chains = CHAINS,
                  refresh = 0)
  y <- fit$y
  yrep <- posterior_predict(fit)
  suppressWarnings(
    psis1 <- psis(-log_lik(fit), cores = 2)
  )
  lw <- weights(psis1)
  suppressWarnings(
    pits <- rstantools::loo_pit(yrep, y, lw)
  )
}


test_that("ppc_loo_pit gives deprecation warning but still works", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_warning(p1 <- ppc_loo_pit(y, yrep, lw), "deprecated")
  expect_gg(p1)
})

test_that("ppc_loo_pit_overlay returns ggplot object", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_gg(ppc_loo_pit_overlay(y, yrep, lw, samples = 25))
  expect_gg(ppc_loo_pit_overlay(y, yrep, psis_object = psis1, samples = 25))
})

test_that("ppc_loo_pit_overlay warns about binary data", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_warning(
    ppc_loo_pit_overlay(rep(1, length(y)), yrep, lw),
    "not recommended for binary data"
  )
})

test_that("ppc_loo_pit_overlay works with boundary_correction=TRUE", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_message(p1 <- ppc_loo_pit_overlay(y, yrep, lw, boundary_correction = TRUE),
                 "continuous observations")
  expect_gg(p1)
})

test_that("ppc_loo_pit_overlay works with boundary_correction=FALSE", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  p1 <- ppc_loo_pit_overlay(y, yrep, lw, boundary_correction = FALSE)
  expect_gg(p1)
})

test_that("ppc_loo_pit_data validates user-provided pit values", {
  expect_error(
    ppc_loo_pit_data(pit = c(0.5, Inf)),
    "between 0 and 1"
  )
  expect_error(
    ppc_loo_pit_data(pit = c(-1, 0.5)),
    "between 0 and 1"
  )
  expect_error(
    ppc_loo_pit_data(pit = c(0.5, NA)),
    "NAs not allowed"
  )
  expect_error(
    ppc_loo_pit_data(pit = "not numeric"),
    "is.numeric"
  )
  expect_error(
    ppc_loo_pit_data(pit = c(Inf, -Inf, Inf)),
    "between 0 and 1"
  )
  expect_error(
    ppc_loo_pit_data(pit = 0.5, boundary_correction = TRUE),
    "At least 2 PIT values"
  )
})

test_that("ppc_loo_pit_qq validates user-provided pit values", {
  expect_error(
    ppc_loo_pit_qq(pit = c(0.5, Inf)),
    "between 0 and 1"
  )
  expect_error(
    ppc_loo_pit_qq(pit = c(-1, 0.5)),
    "between 0 and 1"
  )
  expect_error(
    ppc_loo_pit_qq(pit = c(0.5, NA)),
    "NAs not allowed"
  )
  expect_error(
    ppc_loo_pit_qq(pit = "not numeric"),
    "is.numeric"
  )
})

test_that("ppc_loo_pit_qq returns ggplot object", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_gg(p1 <- ppc_loo_pit_qq(y, yrep, lw))
  expect_gg(p2 <- ppc_loo_pit_qq(y, yrep, psis_object = psis1))
  if ("get_labs" %in% getNamespaceExports("ggplot2")) {
    ll1 <- ggplot2::get_labs(p1)
  } else {
    ll1 <- p1$labels
  }
  expect_equal(ll1$x, "Uniform")
  expect_equal(p1$data, p2$data)
  expect_gg(p3 <- ppc_loo_pit_qq(y, yrep, lw, compare = "normal"))
  if ("get_labs" %in% getNamespaceExports("ggplot2")) {
    ll3 <- ggplot2::get_labs(p3)
  } else {
    ll3 <- p3$labels
  }
  expect_equal(ll3$x, "Normal")
})

test_that("ppc_loo_pit_ecdf returns a ggplot object", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_gg(p0 <- ppc_loo_pit_ecdf(y, yrep, lw))
  expect_gg(p1 <- ppc_loo_pit_ecdf(y, yrep, lw))
  expect_gg(p2 <- ppc_loo_pit_ecdf(y, yrep, psis_object = psis1))
  if ("get_labs" %in% getNamespaceExports("ggplot2")) {
    ll1 <- ggplot2::get_labs(p1)
  } else {
    ll1 <- p1$labels
  }
  expect_equal(ll1$x, "LOO-PIT")
  expect_equal(ll1$y, "ECDF")
  expect_equal(p1$data, p2$data)
  expect_gg(p3 <- ppc_loo_pit_ecdf(y, yrep, lw, plot_diff = TRUE))
  if ("get_labs" %in% getNamespaceExports("ggplot2")) {
    ll3 <- ggplot2::get_labs(p3)
  } else {
    ll3 <- p3$labels
  }
  expect_equal(ll3$y, "ECDF difference")
})

test_that("ppc_loo_pit_ecdf with method='correlated' validates input correctly", {
  set.seed(2025)
  pit <- 1 - (1 - runif(300))^(1.2)
  y_mock <- 1:length(pit)

  expect_message(
    ppc_loo_pit_ecdf(pit = pit, method = "correlated", interpolate_adj = FALSE),
    "As method = 'correlated' specified; ignoring: interpolate_adj."
  )
  expect_message(
    ppc_loo_pit_ecdf(pit = pit, method = "independent", y = y_mock),
    "As 'pit' specified; ignoring: y."
  )
  expect_message(
    ppc_loo_pit_ecdf(pit = pit, method = "independent", gamma = 1.0),
    "As method = 'independent' specified; ignoring: gamma."
  )
  expect_message(
    ppc_loo_pit_ecdf(pit = pit, method = "independent", test = "POT"),
    "As method = 'independent' specified; ignoring: test."
  )
})

test_that("ppc_loo_pit_ecdf with method='correlated' returns ggplot object", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")

  # Test with POT-C (default)
  expect_gg(p1 <- ppc_loo_pit_ecdf(y, yrep, lw, method = "correlated"))

  # Test with PRIT-C
  expect_gg(p2 <- ppc_loo_pit_ecdf(y, yrep, lw, method = "correlated",
  test = "PRIT"))

  # Test with PIET-C
  expect_gg(p3 <- ppc_loo_pit_ecdf(y, yrep, lw, method = "correlated",
  test = "PIET"))

  # Test with plot_diff = TRUE
  expect_gg(p4 <- ppc_loo_pit_ecdf(y, yrep, lw, method = "correlated",
  plot_diff = TRUE))

  # Test with gamma specified
  expect_gg(p5 <- ppc_loo_pit_ecdf(y, yrep, lw, method = "correlated",
  gamma = 0.1))
})

test_that("error if 0,1 in PIT values and test POT or PIET", {
  expect_error(
      ppc_loo_pit_ecdf(pit = c(0, runif(3)), method = "correlated",
      test = "POT")
    )
  expect_error(
      ppc_loo_pit_ecdf(pit = c(0, runif(3)), method = "correlated",
      test = "PIET")
    )
  expect_no_error(
    ppc_loo_pit_ecdf(pit = c(0, runif(3)), method = "correlated",
    test = "PRIT")
  )
})

test_that("ppc_loo_pit_ecdf method argument works correctly", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")

  # Test default (should inform about upcoming change)
  expect_message(
    p1 <- ppc_loo_pit_ecdf(y, yrep, lw),
    "In the next major release"
  )
  expect_gg(p1)

  # Test explicit independent method (should inform about supersession)
  expect_message(
    p2 <- ppc_loo_pit_ecdf(y, yrep, lw, method = "independent"),
    "superseded by the 'correlated' method"
  )
  expect_gg(p2)

  # Test correlated method (no message expected)
  expect_gg(p3 <- ppc_loo_pit_ecdf(y, yrep, lw, method = "correlated"))

  # Test that independent and correlated produce different plots
  expect_true(!identical(p2$data, p3$data) || !identical(p2$layers, p3$layers))
})

test_that("ppc_loo_pit_ecdf correlated method handles edge cases", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")

  set.seed(2026)

  # Test with small sample
  small_pit <- runif(10)
  expect_gg(p1 <- ppc_loo_pit_ecdf(pit = small_pit, method = "correlated"))

  # Test with perfect uniform
  uniform_pit <- seq(0.0001, 0.9999, length.out = 100)
  expect_gg(p2 <- ppc_loo_pit_ecdf(pit = uniform_pit, method = "correlated"))

  # Test with extreme values
  extreme_pit <- c(rep(0.0001, 10), rep(0.99999, 10), runif(80))
  expect_gg(p3 <- ppc_loo_pit_ecdf(pit = extreme_pit, method = "correlated"))

  # Test with single value (edge case)
  single_pit <- 0.5
  expect_error(ppc_loo_pit_ecdf(pit = single_pit, method = "correlated"))
  expect_gg(p5 <- ppc_loo_pit_ecdf(pit = single_pit, method = "correlated",
  test = "PIET"))
})

test_that("ppc_loo_pit functions work when pit specified instead of y, yrep, and lw", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_gg(ppc_loo_pit_qq(pit = pits))
  # ppc_loo_pit_qq
  expect_message(
    p1 <- ppc_loo_pit_qq(y = y, yrep = yrep, lw = lw, pit = pits),
    "'pit' specified so ignoring 'y','yrep','lw' if specified"
  )
  expect_message(
    p2 <- ppc_loo_pit_qq(pit = pits)
  )
  expect_equal(p1$data, p2$data)

  # ppc_loo_pit_ecdf
  expect_gg(ppc_loo_pit_ecdf(pit = rep(pits, 4)))
  expect_message(
    p1 <- ppc_loo_pit_ecdf(y = y, yrep = yrep, lw = lw, pit = rep(pits, 4)),
    "As 'pit' specified; ignoring: y, yrep, lw."
  )
  expect_message(
    p2 <- ppc_loo_pit_ecdf(pit = rep(pits, 4))
  )
  expect_equal(p1$data, p2$data)

  # ppc_loo_pit_overlay
  expect_gg(p1 <- ppc_loo_pit_overlay(pit = pits))
  expect_message(
    ppc_loo_pit_overlay(y = y, yrep = yrep, lw = lw, pit = pits),
    "'pit' specified so ignoring 'y','yrep','lw' if specified"
  )
})

test_that("ppc_loo_intervals returns ggplot object", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_gg(ppc_loo_intervals(y, yrep, psis_object = psis1))
  expect_gg(g <- ppc_loo_intervals(y, yrep, psis_object = psis1, order = "median"))
  expect_s3_class(g$data$x, "factor")
  expect_equal(nlevels(g$data$x), length(g$data$x))

  # subset argument
  expect_gg(g <- ppc_loo_intervals(y, yrep, psis_object = psis1, subset = 1:25))
  expect_equal(nrow(g$data), 25)
})

test_that("ppc_loo_ribbon returns ggplot object", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_gg(ppc_loo_ribbon(y, yrep, psis_object = psis1, prob = 0.7, alpha = 0.1))
  expect_gg(g <- ppc_loo_ribbon(y, yrep, psis_object = psis1, subset = 1:25))
  expect_equal(nrow(g$data), 25)
})

test_that("ppc_loo_intervals/ribbon work when 'intervals' specified", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  intervals <- t(apply(yrep, 2, quantile, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)))
  expect_gg(ppc_loo_intervals(y, intervals = intervals))
  expect_gg(ppc_loo_ribbon(y, intervals = intervals))
  expect_message(ppc_loo_ribbon(y, intervals = intervals),
                 "'intervals' specified so ignoring 'yrep', 'psis_object', 'subset', if specified")
  expect_message(ppc_loo_intervals(y, yrep, psis_object = psis1, intervals = intervals),
                 "'intervals' specified so ignoring 'yrep', 'psis_object', 'subset', if specified")
})

test_that("ppc_loo_intervals/ribbon work when 'intervals' has 3 columns", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  intervals <- t(apply(yrep, 2, quantile, probs = c(0.1, 0.5, 0.9)))
  expect_gg(ppc_loo_intervals(y, intervals = intervals))
  expect_gg(ppc_loo_ribbon(y, intervals = intervals))
})

test_that("errors if dimensions of yrep and lw don't match", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_error(
    ppc_loo_pit_overlay(y, yrep, lw[, 1:5]),
    "identical(dim(yrep), dim(lw)) is not TRUE",
    fixed = TRUE
  )
})

test_that("error if subset is bigger than num obs", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  expect_error(.psis_subset(psis1, 1:1000), "too many elements")
  expect_error(
    ppc_loo_intervals(y, yrep, psis_object = psis1, subset = 1:1000),
    "length(y) >= length(subset) is not TRUE",
    fixed = TRUE
  )
})


# Visual tests ------------------------------------------------------------

source(test_path("data-for-ppc-tests.R"))
set.seed(123)

test_that("ppc_loo_pit_overlay renders correctly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("loo")
  skip_on_r_oldrel()
  skip_if(packageVersion("rstantools") <= "2.4.0")

  p_base <- suppressMessages(ppc_loo_pit_overlay(vdiff_loo_y, vdiff_loo_yrep, vdiff_loo_lw))
  vdiffr::expect_doppelganger("ppc_loo_pit_overlay (default)", p_base)

  p_custom <- suppressMessages(ppc_loo_pit_overlay(
    vdiff_loo_y,
    vdiff_loo_yrep,
    vdiff_loo_lw,
    boundary_correction = FALSE
  ))
  vdiffr::expect_doppelganger("ppc_loo_pit_overlay (boundary)", p_custom)
})

test_that("ppc_loo_pit_qq renders correctly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("loo")
  skip_on_r_oldrel()
  skip_if(packageVersion("rstantools") <= "2.4.0")

  p_base <- ppc_loo_pit_qq(vdiff_loo_y, vdiff_loo_yrep, vdiff_loo_lw)
  vdiffr::expect_doppelganger("ppc_loo_pit_qq (default)", p_base)
})

test_that("ppc_loo_intervals renders correctly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("loo")
  skip_on_r_oldrel()

  psis_object <- suppressWarnings(loo::psis(-vdiff_loo_lw))
  p_base <- ppc_loo_intervals(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object
  )
  vdiffr::expect_doppelganger("ppc_loo_intervals (default)", p_base)

  p_custom <- ppc_loo_intervals(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object,
    prob = 0.6,
    prob_outer = 0.7
  )
  vdiffr::expect_doppelganger("ppc_loo_intervals (prob)", p_custom)

  p_custom <- ppc_loo_intervals(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object,
    order = "median"
  )
  vdiffr::expect_doppelganger("ppc_loo_intervals (order)", p_custom)
})

test_that("ppc_loo_ribbon renders correctly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("loo")
  skip_on_r_oldrel()

  psis_object <- suppressWarnings(loo::psis(-vdiff_loo_lw))
  p_base <- ppc_loo_ribbon(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object
  )
  vdiffr::expect_doppelganger("ppc_loo_ribbon (default)", p_base)

  p_custom <- ppc_loo_ribbon(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object,
    prob = 0.6,
    prob_outer = 0.7
  )
  vdiffr::expect_doppelganger("ppc_loo_ribbon (prob)", p_custom)

  p_custom <- ppc_loo_ribbon(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object,
    subset = 1:10
  )
  vdiffr::expect_doppelganger("ppc_loo_ribbon (subset)", p_custom)
})

test_that("ppc_loo_pit_ecdf with method correlated renders different tests correctly", {
  set.seed(2025)
  pit <- 1 - (1 - runif(300))^(1.2)

  p_cor_pot <- ppc_loo_pit_ecdf(
    pit = pit,
    method = "correlated"
  )
  vdiffr::expect_doppelganger("ppc_loo_pit_ecdf (correlated pot)", p_cor_pot)

  p_cor_prit <- ppc_loo_pit_ecdf(
    pit = pit,
    method = "correlated",
    test = "PRIT"
  )
  vdiffr::expect_doppelganger("ppc_loo_pit_ecdf (correlated prit)", p_cor_prit)

  p_cor_piet <- ppc_loo_pit_ecdf(
    pit = pit,
    method = "correlated",
    test = "PIET"
  )
  vdiffr::expect_doppelganger("ppc_loo_pit_ecdf (correlated piet)", p_cor_piet)
})

test_that("ppc_loo_pit_ecdf with plot_diff=TRUE and method correlated renders different tests correctly", {
  set.seed(2025)
  pit <- 1 - (1 - runif(300))^(1.2)

  p_cor_pot <- ppc_loo_pit_ecdf(
    pit = pit,
    method = "correlated",
    plot_diff = TRUE
  )
  vdiffr::expect_doppelganger("ppc_loo_pit_ecdf (diff, correlated pot)", p_cor_pot)

  p_cor_prit <- ppc_loo_pit_ecdf(
    pit = pit,
    method = "correlated",
    test = "PRIT",
    plot_diff = TRUE
  )
  vdiffr::expect_doppelganger("ppc_loo_pit_ecdf (diff, correlated prit)", p_cor_prit)

  p_cor_piet <- ppc_loo_pit_ecdf(
    pit = pit,
    method = "correlated",
    test = "PIET",
    plot_diff = TRUE
  )
  vdiffr::expect_doppelganger("ppc_loo_pit_ecdf (diff, correlated piet)", p_cor_piet)
})

test_that("ppc_loo_pit_ecdf renders different linewidths and colors correctly", {
  set.seed(2025)
  pit <- 1 - (1 - runif(300))^(1.2)

  p_cor_lw1 <- ppc_loo_pit_ecdf(
    pit = pit,
    method = "correlated",
    linewidth = 1.
  )
  vdiffr::expect_doppelganger("ppc_loo_pit_ecdf (linewidth = 1)", p_cor_lw1)

  p_cor_lw2 <- ppc_loo_pit_ecdf(
    pit = pit,
    method = "correlated",
    linewidth = 2.
  )
  vdiffr::expect_doppelganger("ppc_loo_pit_ecdf (linewidth = 2)", p_cor_lw2)

  p_cor_col <- ppc_loo_pit_ecdf(
    pit = pit,
    method = "correlated",
    color = c(ecdf = "darkblue", highlight = "red")
  )
  vdiffr::expect_doppelganger("ppc_loo_pit_ecdf (color change)", p_cor_col)
})

test_that("ppc_loo_pit_ecdf renders correctly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("loo")
  skip_on_r_oldrel()
  skip_if(packageVersion("rstantools") <= "2.4.0")

  psis_object <- suppressWarnings(loo::psis(-vdiff_loo_lw))
  p_base <- ppc_loo_pit_ecdf(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object
  )
  vdiffr::expect_doppelganger("ppc_loo_pit_ecdf (default)", p_base)

  p_custom <- ppc_loo_pit_ecdf(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object,
    K = 50
  )
  vdiffr::expect_doppelganger("ppc_loo_pit_ecdf (K)", p_custom)

  p_custom <- ppc_loo_pit_ecdf(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object,
    prob = 0.95
  )
  vdiffr::expect_doppelganger("ppc_loo_pit_ecdf (prob)", p_custom)

  p_custom <- ppc_loo_pit_ecdf(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object,
    plot_diff = TRUE,
    K = 100
  )
  vdiffr::expect_doppelganger("ppc_loo_pit_ecdf (ecdf difference)", p_custom)

  p_custom <- ppc_loo_pit_ecdf(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object,
    method = "correlated",
    plot_diff = TRUE,
    prob = 0.95
  )
  vdiffr::expect_doppelganger("ppc_loo_pit_ecdf (alpha=0.05)", p_custom)

  p_custom <- ppc_loo_pit_ecdf(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object,
    method = "correlated",
    plot_diff = TRUE,
    prob = 0.95,
    help_text = FALSE
  )
  vdiffr::expect_doppelganger("ppc_loo_pit_ecdf (no help_text)", p_custom)


  theme_set(bayesplot::theme_default(base_family = "sans", base_size = 12))
  p_custom <- ppc_loo_pit_ecdf(
    vdiff_loo_y,
    vdiff_loo_yrep,
    psis_object = psis_object,
    method = "correlated",
    plot_diff = TRUE
  )
  vdiffr::expect_doppelganger("ppc_loo_pit_ecdf (changed theme)", p_custom)
})

# Test PIT computation branches ------------------------------------------------
# use monkey-patching to test whether the correct branch of the
# PIT computation is taken

testthat::test_that("ppc_loo_pit_ecdf takes correct PIT computation branch", {
  skip_on_cran()
  skip_if_not_installed("loo")
  skip_on_r_oldrel()
  skip_if(packageVersion("rstantools") <= "2.4.0")

  compute_pit_values_patched <- .compute_pit_values

  body(compute_pit_values_patched)[[
    # Replace the PIT computation block with diagnostics.
    which(sapply(as.list(body(.compute_pit_values)), function(e) {
      if (!is.call(e)) return(FALSE)
      identical(e[[1]], as.name("if")) &&
        grepl("pareto_pit", paste(deparse(e[[2]]), collapse = " "))
    }))[1]
  ]] <- quote({
    if (isTRUE(pareto_pit) && is.null(pit)) {
      message("[PIT BRANCH] Pareto-smoothed LOO PIT")
      suggested_package("rstantools")
      y <- validate_y(y)
      yrep <- validate_predictions(yrep, length(y))
      stopifnot(isTRUE(loo_cv), identical(dim(yrep), dim(lw)))
      pit <- posterior::pareto_pit(x = yrep, y = y, weights = lw, log = TRUE)
      K <- K %||% length(pit)
    } else if (!is.null(pit)) {
      message("[PIT BRANCH] Pre-supplied PIT")
      pit <- validate_pit(pit)
      K <- K %||% length(pit)

      ignored <- c(
        if (!missing(y) && !is.null(y)) "y",
        if (!missing(yrep) && !is.null(yrep)) "yrep",
        if (isTRUE(loo_cv) && !is.null(lw)) "lw"
      )
      if (length(ignored) > 0) {
        inform(paste0("As 'pit' specified; ignoring: ",
          paste(ignored, collapse = ", "), "."))
      }
    } else {
      message("[PIT BRANCH] Standard LOO PIT")
      suggested_package("rstantools")
      y <- validate_y(y)
      yrep <- validate_predictions(yrep, length(y))
      stopifnot(isTRUE(loo_cv), identical(dim(yrep), dim(lw)))
      pit <- pmin(1, rstantools::loo_pit(object = yrep, y = y, lw = lw))
      K <- K %||% min(nrow(yrep) + 1, 1000)
    }
    list("group" = group, "pit" = pit, "K" = K)
  })

  pit_branch_probe <- function(y = NULL,
                               yrep = NULL,
                               lw = NULL,
                               pit = NULL,
                               method = NULL,
                               test = NULL,
                               pareto_pit = NULL) {
    method_args <- .pit_ecdf_resolve_method_args(
      method = method,
      pit = pit,
      prob = 0.99,
      interpolate_adj = NULL,
      test = test,
      gamma = NULL,
      linewidth = NULL,
      color = NULL,
      help_text = NULL,
      pareto_pit = pareto_pit,
      help_text_shrinkage = NULL
    )
    compute_pit_values_patched(
      y = y,
      yrep = yrep,
      lw = lw,
      psis_object =  psis_object,
      group = NULL,
      K = NULL,
      pareto_pit = method_args$pareto_pit,
      pit = pit,
      loo_cv = TRUE
    )
  }

  # | yrep | y | lw | pit | method      | test | pareto_pit | approach           |
  # |------|---|----|-----|-------------|------|------------|--------------------|
  # | x    | x | x  |     | independent | NULL | FALSE (D)  | compute loo-pit    |
  # | x    | x | x  |     | independent | NULL | TRUE       | compute pareto-pit |
  # |      |   |    | x   | independent | NULL | FALSE      |                    |
  # | x    | x | x  |     | correlated  | POT  | TRUE       | compute pareto-pit |
  # | x    | x | x  |     | correlated  | POT  | FALSE      | compute loo-pit    |
  # |      |   |    | x   | correlated  | POT  | FALSE      |                    |
  # | x    | x | x  |     | correlated  | PIET | TRUE       | compute pareto-pit |
  # |      |   |    | x   | correlated  | PIET | FALSE      |                    |
  # | x    | x | x  |     | correlated  | PRIT | FALSE      | compute loo-pit    |
  # |      |   |    | x   | correlated  | PRIT | FALSE      |                    |

  pits <- rstantools::loo_pit(vdiff_loo_yrep, vdiff_loo_y, vdiff_loo_lw)

  # method = independent ------------------------------------------
  expect_message(
    pit_branch_probe(
      y = vdiff_loo_y,
      yrep = vdiff_loo_yrep,
      method = "independent",
      lw = vdiff_loo_lw
    ),
    regexp = "\\[PIT BRANCH\\] Standard LOO PIT"
  )

  expect_message(
    pit_branch_probe(
      y = vdiff_loo_y,
      yrep = vdiff_loo_yrep,
      method = "independent",
      lw = vdiff_loo_lw,
      pareto_pit = TRUE
    ),
    regexp = "\\[PIT BRANCH\\] Pareto-smoothed LOO PIT"
  )

  expect_message(
    pit_branch_probe(
      method = "independent",
      pit = pits,
    ),
    regexp = "\\[PIT BRANCH\\] Pre-supplied PIT"
  )

  # method = correlated + POT test -------------------------------
  expect_message(
    pit_branch_probe(
      y = vdiff_loo_y,
      yrep = vdiff_loo_yrep,
      method = "correlated",
      lw = vdiff_loo_lw
    ),
    regexp = "\\[PIT BRANCH\\] Pareto-smoothed LOO PIT"
  )

  expect_message(
    pit_branch_probe(
      y = vdiff_loo_y,
      yrep = vdiff_loo_yrep,
      method = "correlated",
      lw = vdiff_loo_lw,
      pareto_pit = FALSE
    ),
    regexp = "\\[PIT BRANCH\\] Standard LOO PIT"
  )

  expect_message(
    pit_branch_probe(
      method = "correlated",
      pit = pits,
    ),
    regexp = "\\[PIT BRANCH\\] Pre-supplied PIT"
  )

  # method = correlated + PIET test -------------------------------
  expect_message(
    pit_branch_probe(
      y = vdiff_loo_y,
      yrep = vdiff_loo_yrep,
      method = "correlated",
      test = "PIET",
      lw = vdiff_loo_lw
    ),
    regexp = "\\[PIT BRANCH\\] Pareto-smoothed LOO PIT"
  )

  expect_message(
    pit_branch_probe(
      method = "correlated",
      test = "PIET",
      pit = pits,
    ),
    regexp = "\\[PIT BRANCH\\] Pre-supplied PIT"
  )

  # method = correlated + PRIT test -------------------------------
  expect_message(
    pit_branch_probe(
      y = vdiff_loo_y,
      yrep = vdiff_loo_yrep,
      method = "correlated",
      test = "PRIT",
      lw = vdiff_loo_lw
    ),
    regexp = "\\[PIT BRANCH\\] Standard LOO PIT"
  )

  expect_message(
    pit_branch_probe(
      method = "correlated",
      test = "PRIT",
      pit = pits,
    ),
    regexp = "\\[PIT BRANCH\\] Pre-supplied PIT"
  )
})


# ppc_loo_pit_data tests ---------------------------------------------------

test_that("ppc_loo_pit_data returns the expected structure for both boundary modes", {
  set.seed(123)
  pit_vals <- runif(50)
  n_samples <- 10
  expect_message(
    d_raw <- ppc_loo_pit_data(
      pit = pit_vals,
      boundary_correction = FALSE,
      samples = n_samples
    ),
    "pit"
  )
  expect_s3_class(d_raw, "data.frame")
  expect_named(
    d_raw,
    c("y_id", "y_name", "rep_id", "rep_label", "is_y", "is_y_label", "value")
  )
  y_rows <- d_raw[d_raw$is_y, ]
  yrep_rows <- d_raw[!d_raw$is_y, ]
  expect_equal(nrow(y_rows), length(pit_vals))
  expect_equal(nrow(yrep_rows), length(pit_vals) * n_samples)
  expect_equal(y_rows$value, pit_vals)

  grid_len <- 128
  expect_message(
    d_bc <- ppc_loo_pit_data(
      pit = pit_vals,
      boundary_correction = TRUE,
      samples = n_samples,
      grid_len = grid_len
    ),
    "pit"
  )
  expect_named(
    d_bc,
    c("y_id", "y_name", "rep_id", "rep_label", "is_y", "is_y_label", "value", "x")
  )
  y_rows <- d_bc[d_bc$is_y, ]
  yrep_rows <- d_bc[!d_bc$is_y, ]
  expect_equal(nrow(y_rows), grid_len)
  expect_equal(nrow(yrep_rows), grid_len * n_samples)
  expect_false(anyNA(d_bc$x))
})

test_that("ppc_loo_pit_data works with a single pit value", {
  d <- suppressMessages(ppc_loo_pit_data(pit = 0.5, boundary_correction = FALSE, samples = 3))
  y_rows <- d[d$is_y, ]
  expect_equal(nrow(y_rows), 1)
  expect_equal(y_rows$value, 0.5)
})

test_that("check pareto_pit argument is chosen as expected", {
  # pareto_pit defaults to TRUE if test = "POT", pareto_pit = NULL, pit = NULL
  pareto_pit = NULL
  pit = NULL
  test = "POT"
  expect_true(pareto_pit %||% (is.null(pit) && test %in% c("POT", "PIET")))

  # pareto_pit defaults to TRUE if test = "PIET", pareto_pit = NULL, pit = NULL
  pareto_pit = NULL
  pit = NULL
  test = "PIET"
  expect_true(pareto_pit %||% (is.null(pit) && test %in% c("POT", "PIET")))

  # pareto_pit defaults to FALSE if test = "PRIT", and
  # pareto_pit = NULL, pit = NULL
  pareto_pit = NULL
  pit = NULL
  test = "PRIT"
  expect_false(pareto_pit %||% (is.null(pit) && test %in% c("POT", "PIET")))

  # pareto_pit is TRUE if user sets pareto_pit = TRUE, and
  # test = "PRIT", pit = NULL
  pareto_pit = TRUE
  pit = NULL
  test = "PRIT"
  expect_true(pareto_pit %||% (is.null(pit) && test %in% c("POT", "PIET")))

  # pareto_pit is FALSE if pit != NULL irrespective of test
  for (test in c("POT", "PIET", "PRIT")) {
    pareto_pit = NULL
    pit = c(0.1, 0.2, 0.7)
    expect_false(pareto_pit %||% (is.null(pit) && test %in% c("POT", "PIET")))
  }

  # if pit != NULL and user sets pareto_pit = TRUE: reset pareto_pit = NULL
  # internally and warn user about change of behavior.
  pit <- runif(length(y))
  expect_error(
    object = ppc_loo_pit_ecdf(
      pit = pit, method = "correlated", test = "POT", pareto_pit = TRUE
    ),
    regexp = "`pareto_pit = TRUE` cannot be used together with a non-`NULL`"
  )
})