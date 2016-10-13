library(bayesplot)
context("Aesthetics")


# color scheme stuff ------------------------------------------------------
test_that("getting and setting the color scheme works", {
  color_scheme_set("red")
  expect_equivalent(color_scheme_get(), prepare_colors("red"))
  expect_named(prepare_colors("blue"), scheme_level_names())
  expect_named(color_scheme_get(), scheme_level_names())
  for (clr in names(master_color_list)) {
    color_scheme_set(clr)
    expect_equivalent(color_scheme_get(), prepare_colors(clr),
                      info = clr)
    expect_named(color_scheme_get(), scheme_level_names())
  }

  expect_output(print(color_scheme_get("mix-blue-green")),
                "mix-blue-green")
  expect_gg(plot(color_scheme_get("mix-blue-green")))

  color_scheme_set("blue")
  expect_equivalent(color_scheme_get("teal"), prepare_colors("teal"))
})

test_that("setting mixed scheme works", {
  color_scheme_set("mix-gray-blue")
  expect_equivalent(color_scheme_get(), mixed_scheme("gray", "blue"))

  color_scheme_set("mix-blue-gray")
  expect_equivalent(color_scheme_get(), mixed_scheme("blue", "gray"))

  expect_error(color_scheme_set("mix-green-reds"),
               "should be one of")
  expect_error(color_scheme_set("mix-greens-red"),
               "should be one of")
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

test_that("color_scheme_set throws correct errors for custom schemes ", {
  expect_error(color_scheme_set(orange_scheme_bad),
               "not found: not_a_color1, not_a_color2")
  expect_error(color_scheme_set(c("red", "blue")),
               "should be a character vector of length 1 or 6")
})

test_that("mixed_scheme internal function doesn't error", {
  x <- mixed_scheme("green", "red")
  expect_equal(length(x), 6)
  expect_true(all(sapply(x, is.character)))
})

test_that("custom color schemes work", {
  color_scheme_set(orange_scheme_ok)
  expect_named(color_scheme_get())
  expect_equivalent(unlist(color_scheme_get()), orange_scheme_ok)

  random_scheme <- colors()[sample(length(colors()), 6)]
  color_scheme_set(random_scheme)
  expect_equivalent(unlist(color_scheme_get()), random_scheme)
})

test_that("get_color returns correct color values", {
  scheme <- color_scheme_set("green")
  levs <- scheme_level_names()

  ans <- unlist(prepare_colors("green")[levs], use.names = FALSE)
  expect_identical(get_color(levs), ans)
  for (lev in levs)
    expect_identical(get_color(lev), scheme[[lev]], info = lev)
})

test_that("color_scheme_view returns correct ggplot object", {
  color_scheme_set("red")

  a <- color_scheme_view()
  b <- color_scheme_view("green")
  expect_gg(a)
  expect_gg(b)
  expect_identical(a$plot_env$x, color_scheme_get())
  expect_identical(b$plot_env$x, color_scheme_get("green"))
})

test_that("color_scheme_view returns gtable if length(scheme) >= 1", {
  expect_gtable(color_scheme_view(c("red", "gray")))
  expect_gtable(color_scheme_view(c("red", "gray", "blue")))
})



# ggplot theme ------------------------------------------------------------
test_that("theme_default creates ggplot theme", {
  thm1 <- theme_default()
  expect_type(thm1, "list")
  expect_s3_class(thm1, "theme")

  thm2 <- theme_default(base_size = 13)
  expect_type(thm2, "list")
  expect_s3_class(thm2, "theme")
  expect_equal(thm2[["text"]][["size"]], 13)
})
