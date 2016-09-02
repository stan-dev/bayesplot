library(bayesplot)
context("PPC: residuals")

source("data-for-ppc-tests.R")

test_that("ppc_resid_hist and ppc_resid_scatter return ggplot object", {
  expect_gg(ppc_resid_hist(y, yrep[1:5, ]))
  expect_gg(ppc_resid_scatter(y, yrep[1:5, ]))

  expect_gg(ppc_resid_hist(y, yrep[1,, drop = FALSE]))
  expect_gg(ppc_resid_scatter(y, yrep[1,, drop = FALSE]))

  expect_gg(ppc_resid_hist(y2, yrep2))
  expect_gg(ppc_resid_scatter(y2, yrep2))
})

test_that("ppc_resid_scatter_avg returns ggplot2 object", {
  expect_gg(ppc_resid_scatter_avg(y, yrep))
  expect_gg(ppc_resid_scatter_avg(y, yrep[1:5, ]))
})

test_that("ppc_resid_scatter_avg same as ppc_resid_scatter if nrow(yrep) = 1", {
  expect_equal(ppc_resid_scatter_avg(y2, yrep2),
               ppc_resid_scatter(y2, yrep2))
  expect_equal(ppc_resid_scatter_avg(y, yrep[1,, drop=FALSE]),
               ppc_resid_scatter(y, yrep[1,, drop = FALSE]))
})

test_that("ppc_resid_binned returns ggplot object", {
  load("data-for-binomial.rda")
  expect_gg(ppc_resid_binned(y, Ey))
  expect_gg(ppc_resid_binned(y[1:5], Ey[, 1:5]))
  expect_gg(ppc_resid_binned(rep(y, 2), cbind(Ey, Ey)))
})
