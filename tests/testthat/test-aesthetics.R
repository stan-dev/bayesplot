library(ppcheck)
context("schemes and themes")

test_that("getting and setting the color scheme works", {
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

test_that("ppc_color returns correct color values", {
  scheme <- set_color_scheme("greens")
  levs <- scheme_level_names()

  expect_identical(ppc_color(levs), unlist(scheme[levs], use.names = FALSE))
  for (lev in levs)
    expect_identical(ppc_color(lev), scheme[[lev]], info = lev)
})

test_that("theme_ppc creates ggplot theme", {
  thm1 <- theme_ppc()
  expect_s3_class(thm1, "theme")

  thm2 <- theme_ppc(y_text = FALSE, x_lab = FALSE, legend_position = "right")
  expect_s3_class(thm2, "theme")
  expect_s3_class(thm2$axis.title.x, "element_blank")
  expect_s3_class(thm2$axis.text.x, "element_text")
  expect_s3_class(thm2$axis.title.y, "element_blank")
  expect_s3_class(thm2$axis.text.y, "element_blank")
  expect_identical(thm2$legend.position, "right")

  thm3 <- theme_ppc(y_text = TRUE, y_lab = FALSE, x_text = FALSE)
  expect_s3_class(thm3, "theme")
  expect_s3_class(thm3$axis.title.x, "element_blank")
  expect_s3_class(thm3$axis.text.x, "element_blank")
  expect_s3_class(thm3$axis.title.y, "element_blank")
  expect_s3_class(thm3$axis.text.y, "element_text")
  expect_identical(thm3$legend.position, "none")
})
