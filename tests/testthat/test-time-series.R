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
