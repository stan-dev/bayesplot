library(bayesplot)
context("PPC: time-series")

source("data-for-ppc-tests.R")


x <- rnorm(length(y))

test_that("ppc_vs_x returns ggplot object", {
  expect_gg(ppc_vs_x(y, yrep, x))
  expect_gg(ppc_vs_x(y, yrep, x, prob = 0.5))
  expect_gg(ppc_vs_x(y, yrep, x, y_style = "points"))
  expect_gg(ppc_vs_x(y, yrep, x, y_style = "lines"))

  expect_gg(ppc_vs_x(y2, yrep2, x = rnorm(length(y2))))
})

test_that("ppc_vs_x_grouped returns ggplot object", {
  grp <- gl(3, 3, length = length(x))
  expect_gg(ppc_vs_x_grouped(y, yrep, x, group = grp))
})

