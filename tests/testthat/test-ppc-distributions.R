library(bayesplot)
context("PPC: distributions")

source("data-for-ppc-tests.R")

test_that("ppc_dens_overlay returns a ggplot object", {
  expect_gg(ppc_dens_overlay(y, yrep))
  expect_gg(ppc_dens_overlay(y2, yrep2, size = 0.5, alpha = 0.2))
})

test_that("ppc_ecdf_overlay returns a ggplot object", {
  expect_gg(ppc_ecdf_overlay(y, yrep, size = 0.5, alpha = 0.2))
  expect_gg(ppc_ecdf_overlay(y2, yrep2))
})

test_that("ppc_dens,pp_hist,ppc_freqpoly,ppc_boxplot return ggplot objects", {
  expect_gg(ppc_hist(y, yrep[1,, drop = FALSE]))
  expect_gg(ppc_hist(y, yrep[1:8, ]))
  expect_gg(ppc_hist(y2, yrep2))

  expect_gg(ppc_boxplot(y, yrep[1,, drop = FALSE]))
  expect_gg(ppc_boxplot(y, yrep[1:8, ]))
  expect_gg(ppc_boxplot(y2, yrep2, notch = FALSE))

  expect_gg(ppc_dens(y, yrep[1:8, ]))
  expect_gg(ppc_dens(y2, yrep2))

  expect_gg(ppc_freqpoly(y, yrep[1:8, ], binwidth = 2, size = 2, alpha = 0.1))
  expect_gg(ppc_freqpoly(y2, yrep2))

  expect_gg(p <- ppc_hist(y, yrep[1:8, ], binwidth = 3))
  facet_var <- "rep_id"
  expect_equal(as.character(p$facet$params$facets[1]), facet_var)
})

test_that("ppc_freqpoly_grouped returns a ggplot object", {
  expect_gg(ppc_freqpoly_grouped(y, yrep[1:4, ], group))
  expect_gg(ppc_freqpoly_grouped(y, yrep[1:4, ], group,
                                 freq = TRUE, alpha = 0.5))

  expect_error(ppc_freqpoly_grouped(y2, yrep2, group2),
               "'group' must have more than one unique value")
})

test_that("ppc_violin_grouped returns a ggplot object", {
  expect_gg(ppc_violin_grouped(y, yrep, group))
  expect_gg(ppc_violin_grouped(y, yrep, as.numeric(group)))
  expect_gg(ppc_violin_grouped(y, yrep, as.integer(group)))
  expect_gg(ppc_violin_grouped(y, yrep, group, y_draw = "both", y_jitter = 0.3))

  expect_error(ppc_violin_grouped(y2, yrep2, group2),
               "'group' must have more than one unique value")
})
