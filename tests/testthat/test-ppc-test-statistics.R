library(bayesplot)
context("PPC: test-statistics")

source("data-for-ppc-tests.R")

q25 <- function(x) quantile(x, 0.25)
prop0 <- function(x) mean(x == 0)

test_that("ppc_stat returns ggplot object", {
  expect_gg(ppc_stat(y, yrep))
  expect_gg(ppc_stat(y, yrep, stat = "sd"))
  expect_gg(ppc_stat(y, yrep, stat = "q25"))

  expect_gg(ppc_stat(y2, yrep2))
  expect_gg(ppc_stat(y2, yrep2, stat = "prop0"))
})

test_that("ppc_stat_2d returns ggplot object", {
  expect_gg(ppc_stat_2d(y, yrep))
  expect_gg(ppc_stat_2d(y, yrep, stat = c("q25", "median")))

  expect_gg(ppc_stat_2d(y2, yrep2))
})

test_that("ppc_stat_grouped returns ggplot object", {
  expect_gg(ppc_stat_grouped(y, yrep, group))
  expect_gg(ppc_stat_grouped(y, yrep, as.numeric(group)))
  expect_gg(ppc_stat_grouped(y, yrep, as.integer(group), stat = "sd"))

  expect_gg(ppc_stat_grouped(y2, yrep2, group2))
})
