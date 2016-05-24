library(ppcheck)
context("time-series")

test_that("ppc_ts returns ggplot object", {
  expect_gg(ppc_ts(y, yrep))
  expect_gg(ppc_ts(y, yrep, prob = 0.5))
})

test_that("ppc_ts_grouped returns ggplot object", {
  expect_gg(ppc_ts_grouped(y, yrep, group))
  expect_gg(ppc_ts_grouped(y, yrep, as.factor(group), prob = 0.9))
})
