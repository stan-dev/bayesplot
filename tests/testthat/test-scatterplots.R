library(ppcheck)
context("scatterplots")

test_that("ppc_scatter returns ggplot object", {
  expect_gg(ppc_scatter_average(y, yrep))
  expect_gg(ppc_scatter(y, yrep[1:3, ]))
})
