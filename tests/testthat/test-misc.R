library(ppcheck)

y1 <- rnorm(100)
y2 <- rnorm(30)
yrep1 <- matrix(rnorm(100), ncol = 100)
yrep2 <- matrix(rnorm(3000), ncol = 30)


context("validate_y_and_yrep")

test_that("validate_y_and_yrep works", {
  expect_true(validate_y_and_yrep(y1, yrep1))
  expect_true(validate_y_and_yrep(y2, yrep2))

  expect_error(validate_y_and_yrep(yrep1, yrep1), "vector")
  expect_error(validate_y_and_yrep(y1, y1), "matrix")

  expect_error(validate_y_and_yrep(y1, yrep2), "not equal to")
  expect_error(validate_y_and_yrep(y2, yrep1), "not equal to")

  y1[1] <- NA
  yrep2[1,1] <- NA
  expect_error(validate_y_and_yrep(y1, yrep1), "NAs not allowed in 'y'")
  expect_error(validate_y_and_yrep(y2, yrep2), "NAs not allowed in 'yrep'")
})


context("melt_yrep")

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
})


context("call_geom")

test_that("call_geom works", {
  expect_error(call_geom(density, list(1)), "character")
  expect_error(call_geom("path", 3), "list")
  expect_error(call_geom("paths", list(size = 2)), "could not find function")
  expect_is(call_geom("path", list(size = 2)), "LayerInstance")
})
