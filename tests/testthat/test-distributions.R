library(ppcheck)
context("distributions")

test_that("ppc_dist returns ggplot object", {
  expect_gg(ppc_dens_overlay(y, yrep))
  expect_gg(ppc_hist(y, yrep[1:8, ]))

  expect_gg(p <- ppc_hist(y, yrep[1:8, ], binwidth = 3))
  facet_var <- "list(rep_id)"
  labels <- list(fill = "is_y", colour = "is_y", x = "value", y = "density")
  expect_equal(as.character(p$facet)[1], facet_var)
  expect_equal(p$labels, labels)
})
