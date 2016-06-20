library(bayesplot)
context("PPC: residuals")

source("data-for-ppc-tests.R")

test_that("ppc_resid returns ggplot object", {
  expect_gg(ppc_resid(y, yrep[1:5, ]))
  expect_gg(ppc_resid(y, yrep[1,, drop = FALSE]))

  expect_gg(ppc_resid(y2, yrep2))
})

test_that("ppc_resid_binned returns ggplot object", {
  load("data-for-binomial.rda")
  expect_gg(ppc_resid_binned(y, Ey))
  expect_gg(ppc_resid_binned(y[1:5], Ey[, 1:5]))
  expect_gg(ppc_resid_binned(rep(y, 2), cbind(Ey, Ey)))
})
