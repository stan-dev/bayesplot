library(ppcheck)
context("test-statistics")

test_that("ppc_stat returns ggplot object", {
  q25 <- function(x) quantile(x, 0.25)
  expect_gg(ppc_stat(y, yrep))
  expect_gg(ppc_stat(y, yrep, stat = "sd"))
  expect_gg(ppc_stat(y, yrep, stat = "q25"))
  expect_gg(ppc_stat_2d(y, yrep))
  expect_gg(ppc_stat_2d(y, yrep, stat = c("mean", "median")))
})
