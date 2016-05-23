library(ppcheck)
context("scatterplots")

test_that("ppc_scatter returns ggplot object", {
  expect_gg(ppc_scatter(y, yrep[1,, drop = FALSE]))
  expect_gg(ppc_scatter(y, yrep[1:3, ]))
})

test_that("ppc_scatter_avg returns ggplot object", {
  expect_gg(ppc_scatter_avg(y, yrep))
})

test_that("ppc_scatter_avg_grouped returns a ggplot object", {
  expect_gg(ppc_scatter_avg_grouped(y, yrep, group))
  expect_gg(ppc_scatter_avg_grouped(y, yrep, as.numeric(group)))
  expect_gg(ppc_scatter_avg_grouped(y, yrep, as.integer(group)))
})
