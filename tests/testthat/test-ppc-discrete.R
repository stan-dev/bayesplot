library(bayesplot)
suppressPackageStartupMessages(library(rstanarm))
context("PPC: discrete")


# bar plots ---------------------------------------------------------------
data("esoph", package = "datasets")
capture.output(
  fit <- stan_polr(tobgp ~ agegp, data = esoph, method = "probit",
                   prior = R2(0.2, "mean"), init_r = 0.1, seed = 12345,
                   algorithm = "fullrank") # for speed only
)
y <- as.integer(fit$y)
yrep_char <- posterior_predict(fit, draws = 10)
yrep <- sapply(data.frame(yrep_char, stringsAsFactors = TRUE), as.integer)

test_that("ppc_bars & ppc_bars_grouped return a ggplot object", {
  expect_gg(ppc_bars(y, yrep))
  expect_gg(ppc_bars(y, yrep, prob = 0))
  expect_gg(ppc_bars_grouped(y, yrep, group = esoph$agegp))
})

test_that("freq argument to ppc_bars works", {
  p_freq <- ggplot2::ggplot_build(ppc_bars(y, yrep, freq = TRUE))
  p_prop <- ggplot2::ggplot_build(ppc_bars(y, yrep, freq = FALSE))

  y_freq <- p_freq$data[[1]]$y
  y_prop <- p_prop$data[[1]]$y
  expect_equal(y_freq, as.integer(y_freq))
  expect_true(all(y_prop < 1) && all(y_prop > 0))
})

test_that("ppc_bars works with negative integers", {
  y <- round(rnorm(100, -10, 1))
  yrep <- round(matrix(rnorm(100 * 500, -10, 1), 500, 100))
  expect_gg(ppc_bars(y, yrep))
})

test_that("ppc_bars(_grouped) errors if y/yrep not discrete", {
  expect_error(ppc_bars(y + 0.5, yrep),
               "ppc_bars expects 'y' to be discrete")
  expect_error(ppc_bars(y, yrep + 0.5),
               "ppc_bars expects 'yrep' to be discrete")
  expect_error(ppc_bars_grouped(y + 0.5, yrep, group = esoph$agegp),
               "ppc_bars_grouped expects 'y' to be discrete")
  expect_error(ppc_bars_grouped(y, yrep + 0.5, group = esoph$agegp),
               "ppc_bars_grouped expects 'yrep' to be discrete")
})


# rootograms -----------------------------------------------------------
rm(list = ls())
source(test_path("data-for-ppc-tests.R"))

yrep3 <- matrix(yrep2, nrow = 5, ncol = ncol(yrep2), byrow = TRUE)

test_that("ppc_rootogram returns a ggplot object", {
  expect_gg(ppc_rootogram(y2, yrep2))
  expect_gg(ppc_rootogram(y2, yrep3, style = "hanging", prob = 0.5))
  expect_gg(ppc_rootogram(y2, yrep3, style = "suspended"))
})

test_that("ppc_rootogram errors if y/yrep not counts", {
  expect_error(ppc_rootogram(y, yrep),
               "ppc_rootogram expects counts as inputs to 'y'")
  expect_error(ppc_rootogram(y2, yrep[1:5, seq_along(y2)]),
               "ppc_rootogram expects counts as inputs to 'yrep'")
  expect_error(ppc_rootogram(y, yrep3),
               "ncol(yrep) must be equal to length(y)", fixed = TRUE)
})

