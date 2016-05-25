library(ppcheck)
context("misc functions")

source("data-for-all-tests.R")

# melt_yrep ---------------------------------------------------------------
expect_correct_structure <- function(yrep) {
  y <- rnorm(ncol(yrep))
  yrep <- validate_yrep(yrep, y)

  x <- melt_yrep(yrep)
  expect_equal(ncol(x), 3)
  expect_equal(nrow(x), prod(dim(yrep)))
  expect_identical(colnames(x), c("rep_id", "y_id", "value"))
  expect_s3_class(x$rep_id, "factor")
  expect_type(x$y_id, "integer")
  expect_type(x$value, "double")
}

test_that("melt_yrep returns correct structure", {
  expect_correct_structure(yrep)
  expect_correct_structure(yrep2)

  load("data-for-binomial.rda")
  expect_correct_structure(Ey)
  expect_correct_structure(validate_yrep(yrep, y))
})
