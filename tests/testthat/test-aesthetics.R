library(bayesplot)
context("Aesthetics")

test_that("getting and setting the color scheme works", {
  expect_identical(get_color_scheme(), prepare_colors("red"))
  for (clr in names(master_color_list)) {
    set_color_scheme(clr)
    expect_identical(get_color_scheme(), prepare_colors(clr),
                     info = clr)
  }

  set_color_scheme("blue")
  expect_equal(get_color_scheme("teal"), prepare_colors("teal"))
})

orange_scheme_bad <-
  orange_scheme_ok <-
  c("not_a_color1",
    "#ffcc80",
    "#ffad33",
    "#e68a00",
    "#995c00",
    "not_a_color2")
orange_scheme_ok[c(1, 6)] <- c("#ffebcc", "#663d00")

test_that("set_color_scheme throws correct errors for custom schemes ", {
  expect_error(set_color_scheme(orange_scheme_bad),
               "not found: not_a_color1, not_a_color2")
  expect_error(set_color_scheme(c("red", "blue")),
               "should be a character vector of length 1 or 6")
})

test_that("custom color schemes work", {
  set_color_scheme(orange_scheme_ok)
  expect_named(get_color_scheme())
  expect_equivalent(unlist(get_color_scheme()), orange_scheme_ok)

  random_scheme <- colors()[sample(length(colors()), 6)]
  set_color_scheme(random_scheme)
  expect_equivalent(unlist(get_color_scheme()), random_scheme)
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
