library(bayesplot)
context("available_mcmc and available_ppc")


test_that("available_mcmc works", {
  a <- available_mcmc()
  expect_s3_class(a, "bayesplot_function_list")
  expect_s3_class(a, "character")

  all_mcmc_plots <- sort(grep("^mcmc_", getNamespaceExports("bayesplot"), value = TRUE))
  all_mcmc_plots <- grep("_data", all_mcmc_plots, invert = TRUE, value = TRUE)
  expect_identical(as.character(a), all_mcmc_plots)

  b <- available_mcmc("trace|dens", plots = FALSE)
  expect_s3_class(b, "bayesplot_function_list")
  expect_identical(
    as.character(b),
    sort(grep("^mcmc_dens|^mcmc_trace", getNamespaceExports("bayesplot"), value = TRUE))
  )

  expect_length(available_mcmc(pattern = "99999"), 0)
})

test_that("available_ppc works", {
  a <- available_ppc()
  expect_s3_class(a, "bayesplot_function_list")
  expect_s3_class(a, "character")

  all_ppc_plots <- sort(grep("^ppc_", getNamespaceExports("bayesplot"), value = TRUE))
  all_ppc_plots <- grep("_data", all_ppc_plots, invert = TRUE, value = TRUE)
  all_ppc_plots <- setdiff(all_ppc_plots, "ppc_loo_pit") # remove deprecated
  expect_identical(as.character(a), all_ppc_plots)

  b <- available_ppc("grouped")
  expect_s3_class(b, "bayesplot_function_list")
  expect_identical(
    as.character(b),
    sort(grep("^ppc_.*_grouped$", getNamespaceExports("bayesplot"), value = TRUE))
  )

  c <- available_ppc("grouped", invert = TRUE)
  expect_false(any(grepl("grouped", c)))

  expect_length(available_ppc(pattern = "99999"), 0)
})

test_that("available_ppd works", {
  a <- available_ppd()
  expect_s3_class(a, "bayesplot_function_list")
  expect_s3_class(a, "character")

  all_ppd_plots <- sort(grep("^ppd_", getNamespaceExports("bayesplot"), value = TRUE))
  all_ppd_plots <- grep("_data", all_ppd_plots, invert = TRUE, value = TRUE)
  expect_identical(as.character(a), all_ppd_plots)

  a <- available_ppd(plots_only = FALSE)
  expect_identical(as.character(a), sort(grep("^ppd_", getNamespaceExports("bayesplot"), value = TRUE)))
})

test_that("print.bayesplot_function_list works", {
  expect_output(print(available_ppd()), "bayesplot PPD module:")
  expect_output(print(available_ppc()), "bayesplot PPC module:")
  expect_output(print(available_mcmc()), "bayesplot MCMC module:")

  expect_output(print(available_ppc("ribbon")), "(matching pattern 'ribbon')")
  expect_output(print(available_mcmc("trace")), "trace_highlight")

  expect_output(print(available_ppc("grouped", invert = TRUE)),
                "excluding pattern 'grouped'")
})
