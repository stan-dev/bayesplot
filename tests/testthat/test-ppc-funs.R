library(ppcheck)

expect_gg <- function(x) expect_is(x, "ggplot")

y <- rnorm(100)
yrep <- matrix(rnorm(2500), ncol = 100)


context("distributions")
test_that("ppc_dist returns ggplot object", {
  expect_gg(ppc_dens_overlay(y, yrep))
  expect_gg(ppc_hist(y, yrep[1:8, ]))
})

context("scatterplots")
test_that("ppc_scatter returns ggplot object", {
  expect_gg(ppc_scatter_average(y, yrep))
  expect_gg(ppc_scatter_multiple(y, yrep[1:3, ]))
})

context("test-statistics")
test_that("ppc_stat returns ggplot object", {
  q25 <- function(x) quantile(x, 0.25)
  expect_gg(ppc_stat(y, yrep))
  expect_gg(ppc_stat(y, yrep, stat = "sd"))
  expect_gg(ppc_stat(y, yrep, stat = "q25"))
  expect_gg(ppc_stat_2d(y, yrep))
  expect_gg(ppc_stat_2d(y, yrep, stat = c("mean", "median")))
})

context("residuals")
test_that("ppc_resid returns ggplot object", {
  expect_gg(ppc_resid(y, yrep[1:5, ]))
})
