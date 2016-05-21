library(ppcheck)
context("distributions")

test_that("ppc_dist returns ggplot object", {
  expect_gg(ppc_dens_overlay(y, yrep))
  expect_gg(ppc_hist(y, yrep[1:8, ]))
})
