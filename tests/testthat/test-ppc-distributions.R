library(bayesplot)
context("PPC: distributions")

source("data-for-ppc-tests.R")

test_that("ppc_dens_overlay returns a ggplot object", {
  expect_gg(ppc_dens_overlay(y, yrep))
  expect_gg(ppc_dens_overlay(y2, yrep2))
})

test_that("ppc_dens and pp_hist return ggplot objects", {
  expect_gg(ppc_hist(y, yrep[1,, drop = FALSE]))
  expect_gg(ppc_hist(y, yrep[1:8, ]))
  expect_gg(ppc_hist(y2, yrep2))

  expect_gg(ppc_dens(y, yrep[1:8, ]))
  expect_gg(ppc_dens(y2, yrep2))

  expect_gg(p <- ppc_hist(y, yrep[1:8, ], binwidth = 3))
  facet_var <- "rep_id"
  labels <- list(fill = "is_y", colour = "is_y", x = "value", y = "density")
  expect_equal(as.character(p$facet$params$facets[1]), facet_var)
  expect_equal(p$labels, labels)
})

test_that("ppc_violin_grouped returns a ggplot object", {
  expect_gg(ppc_violin_grouped(y, yrep, group))
  expect_gg(ppc_violin_grouped(y, yrep, as.numeric(group)))
  expect_gg(ppc_violin_grouped(y, yrep, as.integer(group)))

  expect_error(ppc_violin_grouped(y2, yrep2, group2),
               "'group' must have more than one unique value")
})
