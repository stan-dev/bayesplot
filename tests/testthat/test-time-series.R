library(ppcheck)
context("time-series")

source("data-for-all-tests.R")

test_that("ppc_ts returns ggplot object", {
  expect_gg(ppc_ts(y, yrep))
  expect_gg(ppc_ts(y, yrep, prob = 0.5))
  expect_gg(ppc_ts(y, yrep, y_style = "points"))
  expect_gg(ppc_ts(y, yrep, y_style = "lines"))
  expect_gg(ppc_ts(y, yrep, time = 1:length(y)))
  expect_gg(ppc_ts(y, yrep, time = seq(1, 2 * length(y), by = 2)))

  expect_gg(ppc_ts(y2, yrep2))
})

y <- rnorm(50)
yrep <- matrix(rnorm(500, 0, 2), ncol = 50)
time <- rep(1:10, each = 5)
group <- gl(5, 1, length = 50, labels = LETTERS[1:5])

test_that("ppc_ts_grouped returns ggplot object", {
  expect_gg(ppc_ts_grouped(y, yrep, time, group))
})

test_that("ppc_ts_data returns correct structure", {
  d <- ppc_ts_data(y, yrep, time = 1:length(y))
  d_group <- ppc_ts_data(y, yrep, time, group)
  expect_identical(colnames(d), c("time", "is_y", "median", "lower", "upper"))
  expect_identical(colnames(d_group), c("time", "group", "is_y", "median", "lower", "upper"))

  expect_error(ppc_ts_data(y, yrep, time = 1:length(y), prob = 0), "prob")
  expect_error(ppc_ts_data(y, yrep, time = 1:length(y), prob = 1.01), "prob")
})

