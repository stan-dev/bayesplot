library(bayesplot)
context("PPC: intervals & ribbon")

source("data-for-ppc-tests.R")

test_that("ppc_intervals returns ggplot object", {
  expect_gg(ppc_intervals(y, yrep))
  expect_gg(ppc_intervals(y, yrep, size = 2, fatten = 1))
  expect_gg(ppc_intervals(y, yrep, x = seq(1, 2 * length(y), by = 2)))
  expect_gg(ppc_intervals(y2, yrep2))
})
test_that("ppc_ribbon returns ggplot object", {
  expect_gg(ppc_ribbon(y, yrep, prob = 0.5))
  expect_gg(ppc_ribbon(y, yrep, alpha = 0, size = .5))
  expect_gg(ppc_ribbon(y2, yrep2, x = rnorm(length(y2)), prob = 0.5))
})


y <- rnorm(50)
yrep <- matrix(rnorm(500, 0, 2), ncol = 50)
x <- rep(1:10, each = 5)
group <- gl(5, 1, length = 50, labels = LETTERS[1:5])

test_that("ppc_intervals_grouped returns ggplot object", {
  expect_gg(ppc_intervals_grouped(y, yrep, x, group))
})
test_that("ppc_ribbon_grouped returns ggplot object", {
  expect_gg(ppc_ribbon_grouped(y, yrep, x, group, facet_args = list(scales = "fixed")))
})

test_that(".ppc_intervals_data returns correct structure", {
  d <- .ppc_intervals_data(y, yrep, x = 1:length(y))
  d_group <- .ppc_intervals_data(y, yrep, x, group)
  expect_named(d, c("x", "is_y", "lo", "mid", "hi"))
  expect_named(d_group, c("x", "group", "is_y","lo", "mid", "hi"))

  expect_error(.ppc_intervals_data(y, yrep, x = 1:length(y), prob = 0), "prob")
  expect_error(.ppc_intervals_data(y, yrep, x = 1:length(y), prob = 1.01), "prob")
})

