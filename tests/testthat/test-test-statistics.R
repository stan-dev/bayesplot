library(ppcheck)
context("test-statistics")

q25 <- function(x) quantile(x, 0.25)

test_that("ppc_stat returns ggplot object", {
  expect_gg(ppc_stat(y, yrep))
  expect_gg(ppc_stat(y, yrep, stat = "sd"))
  expect_gg(ppc_stat(y, yrep, stat = "q25"))
})

test_that("ppc_stat_2d returns ggplot object", {
  expect_gg(ppc_stat_2d(y, yrep))
  expect_gg(ppc_stat_2d(y, yrep, stat = c("q25", "median")))
})

test_that("ppc_stat_grouped returns ggplot object", {
  expect_gg(ppc_stat_grouped(y, yrep, group))
  expect_gg(ppc_stat_grouped(y, yrep, as.numeric(group)))
  expect_gg(ppc_stat_grouped(y, yrep, as.integer(group), stat = "sd"))
})
