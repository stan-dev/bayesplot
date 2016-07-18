library(bayesplot)
context("Aesthetics")

test_that("getting and setting the color scheme works", {
  expect_identical(get_color_scheme(), prepare_colors("red"))
  for (clr in names(master_color_list)) {
    set_color_scheme(clr)
    expect_identical(get_color_scheme(), prepare_colors(clr),
                     info = clr)
  }
})

test_that("ppc_color returns correct color values", {
  scheme <- set_color_scheme("green")
  levs <- scheme_level_names()

  ans <- unlist(prepare_colors("green")[levs], use.names = FALSE)
  expect_identical(get_color(levs), ans)
  for (lev in levs)
    expect_identical(get_color(lev), scheme[[lev]], info = lev)
})

test_that("theme_default creates ggplot theme", {
  thm1 <- theme_default()
  expect_s3_class(thm1, "theme")

  thm2 <- theme_default(y_text = FALSE, x_lab = FALSE, legend_position = "right")
  expect_s3_class(thm2, "theme")
  expect_s3_class(thm2$axis.title.x, "element_blank")
  expect_s3_class(thm2$axis.text.x, "element_text")
  expect_s3_class(thm2$axis.title.y, "element_blank")
  expect_s3_class(thm2$axis.text.y, "element_blank")
  expect_identical(thm2$legend.position, "right")

  thm3 <- theme_default(y_text = TRUE, y_lab = FALSE, x_text = FALSE)
  expect_s3_class(thm3, "theme")
  expect_s3_class(thm3$axis.title.x, "element_blank")
  expect_s3_class(thm3$axis.text.x, "element_blank")
  expect_s3_class(thm3$axis.title.y, "element_blank")
  expect_s3_class(thm3$axis.text.y, "element_text")
  expect_identical(thm3$legend.position, "none")
})
