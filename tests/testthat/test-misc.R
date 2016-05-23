library(ppcheck)
context("convenience functions")

# validating y, yrep, and group -----------------------------------------
test_that("validate_y works", {
  expect_identical(validate_y(y1), y1)
  expect_identical(validate_y(as.array(y1)), y1)

  expect_error(validate_y(yrep), "vector")
  expect_error(validate_y(LETTERS), "numeric")
  expect_error(validate_y(as.array(yrep)), "vector or a 1-D array")
  expect_error(validate_y(c(y1, NA)), "NAs not allowed")
})
test_that("validate_yrep works", {
  expect_identical(validate_yrep(yrep1, y1), yrep1)
  expect_identical(validate_yrep(yrep2, y2), yrep2)

  expect_error(validate_yrep(as.matrix(LETTERS), y1), "numeric")
  expect_error(validate_yrep(y1, y1), "matrix")
  expect_error(validate_yrep(rbind(yrep1, NA), y1), "NAs not allowed")
  expect_error(validate_yrep(yrep1, yrep1), "vector")
  expect_error(validate_yrep(y1, y1), "matrix")
  expect_error(validate_yrep(yrep2, y1), "not equal to")
  expect_error(validate_yrep(yrep1, y2), "not equal to")
})
test_that("validate_group works", {
  expect_identical(validate_group(group = 1:3, y = 1:3), 1:3)
  expect_identical(validate_group(group = as.numeric(1:3), y = 4:6), as.numeric(1:3))
  expect_identical(validate_group(group, y), group)
  expect_identical(validate_group(group = letters[1:3], y = 1:3), factor(letters[1:3]))

  expect_error(validate_group(group = array(1:3), y = 1:3), "vector")
  expect_error(validate_group(group = c(1,2,NA), y = 1:3), "NAs not allowed")
  expect_error(validate_group(group = 1:4, y = 1:3), "not equal to")
})


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
