library(ppcheck)

expect_gg <- function(x) expect_is(x, "ggplot")

y <- rnorm(100)
yrep <- matrix(rnorm(2500), ncol = 100)


context("ppc_dens_overlay and ppc_hist")
test_that("ppc_dist returns ggplot object", {
  expect_gg(ppc_dens_overlay(y, yrep))
  expect_gg(ppc_hist(y, yrep[1:8, ]))
})

context("ppc_scatter")
test_that("ppc_scatter returns ggplot object", {
  expect_gg(ppc_scatter(y, yrep))
  expect_gg(ppc_scatter(y, yrep[1:3, ], average = FALSE))
})

context("ppc_stat")
test_that("ppc_stat returns ggplot object", {
  q25 <- function(x) quantile(x, 0.25)
  expect_gg(ppc_stat(y, yrep))
  expect_gg(ppc_stat(y, yrep, stat = "sd"))
  expect_gg(ppc_stat(y, yrep, stat = "q25"))
  expect_gg(ppc_stat_2d(y, yrep))
  expect_gg(ppc_stat_2d(y, yrep, stat = c("mean", "median")))
})

context("ppc_resid")
test_that("ppc_resid returns ggplot object", {
  expect_gg(ppc_resid(y, yrep[1:5, ]))
})
