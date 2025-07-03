library(bayesplot)
library(ggplot2)
context("Convenience functions (for ggplot objects)")


# abline_01, vline_ and hline_ ------------------------------------------
test_that("abline_01 returns the correct object", {
  a <- abline_01(color = "green", linetype = 2)
  b <- geom_abline(intercept = 0, slope = 1, color = "green", linetype = 2, na.rm = TRUE)
  a$constructor <- b$constructor <- NULL
  expect_equal(a, b, check.environment = FALSE)
})
test_that("vline_* and hline_* return correct objects", {
  a <- vline_0(color = "red")
  b <- geom_vline(xintercept = 0, color = "red", na.rm = TRUE)
  a$constructor <- b$constructor <- NULL
  expect_equal(a, b, check.environment = FALSE)

  a <- hline_0(linewidth = 2, linetype = 3)
  b <- geom_hline(yintercept = 0, linewidth = 2, linetype = 3, na.rm = TRUE)
  a$constructor <- b$constructor <- NULL
  expect_equal(a, b, check.environment = FALSE)

  a <- vline_at(c(3,4), na.rm = FALSE)
  b <- geom_vline(xintercept = c(3,4))
  a$constructor <- b$constructor <- NULL
  expect_equal(a, b, check.environment = FALSE)

  a <- hline_at(c(3,4), na.rm = FALSE)
  b <- geom_hline(yintercept = c(3,4))
  a$constructor <- b$constructor <- NULL
  expect_equal(a, b, check.environment = FALSE)
})
test_that("vline_at with 'fun' works", {
  x <- example_mcmc_draws(chains = 1)
  a <- vline_at(x, colMeans)
  b <- geom_vline(xintercept = colMeans(x), na.rm = TRUE)
  a$constructor <- b$constructor <- NULL
  expect_equal(a, b, check.environment = FALSE)
})
test_that("calc_v (internal function) works", {
  a <- 1:4
  expect_identical(calc_v(a, "mean"), 2.5)
  expect_identical(calc_v(a, median), 2.5)
  expect_equal(calc_v(c(a, NA), mean), NA_real_)
  expect_identical(calc_v(c(a, NA), min, list(na.rm = TRUE)), 1L)
  expect_error(calc_v(fun = "mean"), "'v' can't be missing")
})

# lbub --------------------------------------------------------------------
test_that("lbub works", {
  f1 <- lbub(p = 0.5)
  f2 <- lbub(p = 0.5, med = FALSE)

  expect_type(f1, "closure")
  expect_type(f2, "closure")
  expect_identical(
    f1(1:50),
    setNames(c(13.25, 25.5, 37.75), c("25%", "50%", "75%"))
  )
  expect_identical(
    f2(1:50),
    setNames(c(13.25, 37.75), c("25%", "75%"))
  )
})

# plot and facet backgrounds ----------------------------------------------
test_that("grid_lines returns correct theme object", {
  thm <- theme_default() + grid_lines(size = 1.5, color = "purple")
  expect_equal(thm$panel.grid.major, element_line(linewidth = 1.5, color = "purple"))
  expect_equal(thm$panel.grid.minor, element_line(linewidth = 0.75, color = "purple"))
})
test_that("panel_bg returns correct theme object", {
  bg1 <- panel_bg()
  bg2 <- panel_bg(fill = "blue", linetype = 2)

  expect_identical(bg1, theme(panel.background = element_rect()))
  expect_identical(bg2, theme(panel.background = element_rect(fill = "blue", linetype = 2)))
  expect_identical(panel_bg(on = FALSE), theme(panel.background = element_blank()))
})
test_that("plot_bg returns correct theme object", {
  bg1 <- plot_bg()
  bg2 <- plot_bg(fill = "blue", linetype = 2)

  expect_identical(bg1, theme(plot.background = element_rect()))
  expect_identical(bg2, theme(plot.background = element_rect(fill = "blue", linetype = 2)))
  expect_identical(plot_bg(on = FALSE), theme(plot.background = element_blank()))
})
test_that("facet_bg returns correct theme object", {
  bg1 <- facet_bg()
  bg2 <- facet_bg(fill = "blue", linetype = 2)

  expect_identical(bg1, theme(strip.background = element_rect()))
  expect_identical(bg2, theme(strip.background = element_rect(fill = "blue", linetype = 2)))
  expect_identical(facet_bg(on = FALSE), theme(strip.background = element_blank()))
})

# legend position and text ------------------------------------------------
test_that("legend_none returns correct theme object", {
  none <- legend_none()
  expect_s3_class(none, "theme")
  expect_equivalent(none, list(legend.position = "none"))
  expect_false(attr(none, "complete"))
})
test_that("legend_move returns correct theme object", {
  left <- legend_move("left")
  expect_s3_class(left, "theme")
  expect_equivalent(left, list(legend.position = "left"))
  expect_false(attr(left, "complete"))

  pos <- legend_move(c(0.25, 0.5))
  expect_s3_class(pos, "theme")
  expect_equivalent(
    pos$legend.position.inside %||% pos$legend.position,
    c(0.25, 0.5)
  )
  expect_false(attr(pos, "complete"))
})
test_that("legend_text returns correct theme object", {
  expect_equal(
    legend_text(size = 16, color = "purple"),
    theme(legend.text = element_text(color = "purple", size = 16))
  )
})

# axis and facet text --------------------------------------------------
test_that("xaxis_text returns correct theme object", {
  expect_identical(xaxis_text(FALSE), theme(axis.text.x = element_blank()))
  expect_equal(
    xaxis_text(face = "bold", angle = 30),
    theme(axis.text.x = element_text(face = "bold", angle = 30))
  )
})
test_that("yaxis_text returns correct theme object", {
  expect_identical(yaxis_text(FALSE), theme(axis.text.y = element_blank()))
  expect_equivalent(
    yaxis_text(face = "bold", angle = 30),
    theme(axis.text.y = element_text(face = "bold", angle = 30))
  )
})
test_that("facet_text returns correct theme object", {
  expect_identical(facet_text(FALSE), theme(strip.text = element_blank()))
  expect_equal(
    facet_text(size = 12, color = "blue"),
    theme(strip.text = element_text(color = "blue", size = 12))
  )
})

# axis titles -------------------------------------------------------------
test_that("xaxis_title returns correct theme object", {
  expect_identical(xaxis_title(FALSE), xlab(NULL))
  expect_equal(
    xaxis_title(face = "bold", angle = 30),
    theme(axis.title.x = element_text(face = "bold", angle = 30))
  )
})
test_that("yaxis_title returns correct theme object", {
  expect_identical(yaxis_title(FALSE), ylab(NULL))
  expect_equal(
    yaxis_title(face = "bold", angle = 30),
    theme(axis.title.y = element_text(face = "bold", angle = 30))
  )
})

# tick marks --------------------------------------------------
test_that("xaxis_ticks returns correct theme object", {
  expect_identical(xaxis_ticks(FALSE), theme(axis.ticks.x = element_blank()))
  expect_equal(
    xaxis_ticks(linewidth = 0.5, color = "red"),
    theme(axis.ticks.x = element_line(linewidth = 0.5, color = "red"))
  )
})
test_that("yaxis_ticks returns correct theme object", {
  expect_identical(yaxis_ticks(FALSE), theme(axis.ticks.y = element_blank()))
  expect_equal(
    yaxis_ticks(linewidth = 0.5, color = "red"),
    theme(axis.ticks.y = element_line(linewidth = 0.5, color = "red"))
  )
})


# overlay functions -------------------------------------------------------
test_that("overlay_function returns the correct object", {
  expect_error(overlay_function(), 'argument "fun" is missing')
  a <- overlay_function(fun = "dnorm")
  b <- stat_function(fun = "dnorm", inherit.aes = FALSE)
  a$constructor <- b$constructor <- NULL
  expect_equal(a, b, check.environment = FALSE)
})


# tagged functions  -------------------------------------------------------

test_that("as_tagged_function handles bare function (symbol)", {
  fn <- as_tagged_function(mean)
  expect_type(fn, "closure")
  expect_equal(fn(1:10), mean(1:10))
  expect_equal(attr(fn, "tagged_expr"), rlang::expr(mean))

  # primitive functions are wrapped then tagged
  fn <- as_tagged_function(max)
  expect_equal(fn(1:10), 10)
  expect_equal(attr(fn, "tagged_expr"), rlang::expr(max))
})

test_that("as_tagged_function handles string input", {
  fn <- as_tagged_function("mean")
  expect_type(fn, "closure")
  expect_equal(fn(1:10), mean(1:10))
  expect_equal(attr(fn, "tagged_expr"), rlang::sym("mean"))
})

test_that("as_tagged_function handles anonymous functions", {
  fn <- as_tagged_function(function(x) mean(x^2))
  expect_type(fn, "closure")
  expect_equal(fn(1:3), mean((1:3)^2))
  expect_equal(attr(fn, "tagged_expr"), rlang::expr( function(x) mean(x^2)))

  fn <- as_tagged_function(~mean(.x^2))
  expect_type(fn, "closure")
  expect_equal(fn(1:3), mean((1:3)^2))
  expect_equal(attr(fn, "tagged_expr"), rlang::expr( ~mean(.x^2)))
})

test_that("as_tagged_function handles NULL with fallback name", {
  fn <- as_tagged_function(NULL, fallback = "my_func")
  expect_type(fn, "closure")
  expect_equal(fn(1:5), 1:5)
  expect_equal(attr(fn, "tagged_expr"), rlang::sym("my_func"))
})

test_that("as_tagged_function doesn't lose previous tags", {
  fn1 <- as_tagged_function(mean)
  fn2 <- as_tagged_function(fn1)
  expect_identical(fn1, fn2)
  expect_equal(attr(fn2, "tagged_expr"), rlang::expr(mean))

  f_outer <- function(stat_outer) {
    stat_outer <- as_tagged_function({{ stat_outer }})
    f_inner(stat_outer)
  }
  f_inner <- function(stat_inner) {
    stat_inner <- as_tagged_function({{ stat_inner }})
    stat_inner
  }

  # We don't want the tagged expressions to be stat_outer or stat_inner
  my_function_name <- identity
  f_inner(my_function_name) |>
    attr("tagged_expr") |>
    deparse() |>
    expect_equal("my_function_name")

  f_outer(my_function_name) |>
    attr("tagged_expr") |>
    deparse() |>
    expect_equal("my_function_name")

  # All the non-standard evaluation still provides a callable function
  f_outer(my_function_name)(1:10) |>
    expect_equal(1:10)
})
