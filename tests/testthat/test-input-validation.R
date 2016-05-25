library(ppcheck)
context("Input validation")


# validating y ------------------------------------------------------------
test_that("validate_y works", {
  expect_identical(validate_y(y1), y1)
  expect_identical(validate_y(as.array(y1)), y1)

  expect_error(validate_y(yrep), "vector")
  expect_error(validate_y(LETTERS), "numeric")
  expect_error(validate_y(as.array(yrep)), "vector or a 1-D array")
  expect_error(validate_y(c(y1, NA)), "NAs not allowed")
})

# validating yrep ----------------------------------------------------------
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

# validating group --------------------------------------------------------
test_that("validate_group works", {
  expect_identical(validate_group(group = 1:3, y = 1:3), 1:3)
  expect_identical(validate_group(group = as.numeric(1:3), y = 4:6), as.numeric(1:3))
  expect_identical(validate_group(group, y), group)
  expect_identical(validate_group(group = letters[1:3], y = 1:3), factor(letters[1:3]))

  expect_error(validate_group(group = array(1:3), y = 1:3), "vector")
  expect_error(validate_group(group = c(1,2,NA), y = 1:3), "NAs not allowed")
  expect_error(validate_group(group = 1:4, y = 1:3), "not equal to")
})

# validating time --------------------------------------------------------
test_that("validate_time works", {
  expect_identical(validate_time(time = 1:3, y = 1:3), 1:3)
  expect_identical(validate_time(time = as.numeric(1:3), y = 4:6), as.numeric(1:3))
  expect_identical(validate_time(y = rnorm(3)), 1:3)

  tt <- rnorm(length(y))
  expect_identical(validate_time(tt, y), tt)
  expect_identical(validate_time(as.array(tt), y), tt)

  expect_error(validate_time(time = letters[1:3], y = 1:3), "numeric")
  expect_error(validate_time(time = yrep, y = 1:3), "vector")
  expect_error(validate_time(time = c(1,2,NA), y = 1:3), "NAs not allowed")
})
