library(ppcheck)
context("misc functions")

# melt_yrep ---------------------------------------------------------------
expect_correct_structure <- function(yrep) {
  x <- melt_yrep(yrep)
  expect_equal(ncol(x), 3)
  expect_equal(nrow(x), prod(dim(yrep)))
  expect_identical(colnames(x), c("rep_id", "y_id", "value"))
  expect_is(x$rep_id, "factor")
  expect_is(x$y_id, "integer")
  expect_is(x$value, "numeric")
}

test_that("melt_yrep returns correct structure", {
  expect_correct_structure(yrep1)
  expect_correct_structure(yrep2)

  load("data-for-binomial.rda")
  expect_correct_structure(Ey)
  expect_correct_structure(validate_yrep(yrep, y))
})
