library(bayesplot)
context("PPC: rootograms")

source("data-for-ppc-tests.R")

test_that("ppc_rootogram returns a ggplot object", {
  expect_gg(ppc_rootogram(y2, yrep2))
  yrep2 <- matrix(yrep2, nrow = 5, ncol = ncol(yrep2), byrow = TRUE)
  expect_gg(ppc_rootogram(y2, yrep2, style = "hanging", prob = 0.5))
  expect_gg(ppc_rootogram(y2, yrep2, style = "suspended"))

  expect_error(ppc_rootogram(y, yrep2),
               "ncol(yrep) must be equal to length(y)", fixed = TRUE)

  expect_error(ppc_rootogram(y, yrep),
               "ppc_rootogram expects counts as inputs to 'y'")
  expect_error(ppc_rootogram(y2, yrep[1:5, seq_along(y2)]),
               "ppc_rootogram expects counts as inputs to 'yrep'")
})
