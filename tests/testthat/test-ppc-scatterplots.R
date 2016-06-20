library(bayesplot)
context("PPC: scatterplots")

source("data-for-ppc-tests.R")

expect_gg <- function(x) expect_is(x, "ggplot")

test_that("ppc_scatter returns ggplot object", {
  expect_gg(ppc_scatter(y, yrep[1,, drop = FALSE]))
  expect_gg(ppc_scatter(y, yrep[1:3, ]))
  expect_gg(ppc_scatter(y2, yrep2))
})

test_that("ppc_scatter_avg returns ggplot object", {
  expect_gg(ppc_scatter_avg(y, yrep))
  expect_gg(ppc_scatter_avg(y2, yrep2))
})

test_that("ppc_scatter_avg_grouped returns a ggplot object", {
  expect_gg(ppc_scatter_avg_grouped(y, yrep, group))
  expect_gg(ppc_scatter_avg_grouped(y, yrep, as.numeric(group)))
  expect_gg(ppc_scatter_avg_grouped(y, yrep, as.integer(group)))
  expect_gg(ppc_scatter_avg_grouped(y2, yrep2, group2))
})
