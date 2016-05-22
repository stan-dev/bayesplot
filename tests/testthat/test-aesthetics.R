library(ppcheck)
context("schemes and themes")

test_that("getting and setting color scheme works", {
  expect_identical(get_color_scheme(), scheme_reds())
  set_color_scheme("blues")
  expect_identical(get_color_scheme(), scheme_blues())
  set_color_scheme("greens")
  expect_identical(get_color_scheme(), scheme_greens())
  set_color_scheme("greys")
  expect_identical(get_color_scheme(), scheme_greys())
  set_color_scheme("purples")
  expect_identical(get_color_scheme(), scheme_purples())
})

test_that("theme_ppc creates ggplot theme", {
  thm1 <- theme_ppc()
  thm2 <- theme_ppc(y_text = FALSE, legend_position = "right")
  expect_is(thm1, "theme")
  expect_is(thm2, "theme")
})
