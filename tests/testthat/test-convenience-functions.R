library(bayesplot)
context("Convenience functions (for ggplot objects)")


# vline_ and hline_ -------------------------------------------------------
test_that("vline_* and hline_* return correct objects", {
  v0 <- vline_0(color = "red")
  h0 <- hline_0(size = 2, linetype = 3)
  v34 <- vline_at(c(3,4))
  h34 <- hline_at(c(3,4))

  expect_s3_class(v0, c("LayerInstance", "Layer", "ggproto"))
  expect_s3_class(h0, c("LayerInstance", "Layer", "ggproto"))
  expect_s3_class(v34, c("LayerInstance", "Layer", "ggproto"))
  expect_s3_class(h34, c("LayerInstance", "Layer", "ggproto"))

  expect_identical(v0$data, data.frame(xintercept = 0))
  expect_identical(h0$data, data.frame(yintercept = 0))
  expect_identical(v34$data, data.frame(xintercept = c(3,4)))
  expect_identical(h34$data, data.frame(yintercept = c(3,4)))

  expect_identical(v0$aes_params, list(colour = "red"))
  expect_identical(h0$aes_params, list(size = 2, linetype = 3))
  expect_identical(v34$aes_params, structure(list(), names = character(0)))
  expect_identical(h34$aes_params, v34$aes_params)
})

test_that("vline_at with 'fun' works", {
  x <- example_mcmc_draws(chains = 1)
  vMeans <- vline_at(x, colMeans)
  expect_identical(vMeans$data, data.frame(xintercept = colMeans(x)))
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


# legends -----------------------------------------------------------------
test_that("no_legend and move_legend return correct theme objects", {
  none <- no_legend()
  expect_s3_class(none, "theme")
  expect_equivalent(none, list(legend.position = "none"))
  expect_false(attr(none, "complete"))

  left <- move_legend("left")
  expect_s3_class(left, "theme")
  expect_equivalent(left, list(legend.position = "left"))
  expect_false(attr(left, "complete"))

  pos <- move_legend(c(0.25, 0.5))
  expect_s3_class(pos, "theme")
  expect_equivalent(pos, list(legend.position = c(0.25, 0.5)))
  expect_false(attr(pos, "complete"))
})


# facet and axis text --------------------------------------------------
test_that("facet_text, xaxis_text, yaxis_text return correct theme objects", {
  expect_s3_class(facet_text(FALSE)[["strip.text"]], "element_blank")
  expect_s3_class(xaxis_text(FALSE)[["axis.text.x"]], "element_blank")
  expect_s3_class(yaxis_text(FALSE)[["axis.text.y"]], "element_blank")

  ftxt <- facet_text(size = 12, color = "blue")
  expect_s3_class(ftxt, "theme")
  expect_equivalent(ftxt,
    list(strip.text = ggplot2::element_text(color = "blue", size = 12))
  )
  expect_false(attr(ftxt, "complete"))

  xtxt <- xaxis_text(face = "bold", angle = 30)
  expect_s3_class(xtxt, "theme")
  expect_equivalent(xtxt,
    list(axis.text.x = ggplot2::element_text(face = "bold", angle = 30))
  )
  expect_false(attr(xtxt, "complete"))

  ytxt <- xaxis_text(face = "bold", angle = 30)
  expect_s3_class(ytxt, "theme")
  expect_equivalent(ytxt,
    list(axis.text.y = ggplot2::element_text(face = "bold", angle = 30))
  )
  expect_false(attr(ytxt, "complete"))
})


# axis titles -------------------------------------------------------------
test_that("xaxis_title, yaxis_title return correct theme objects", {
  xttl <- xaxis_title(face = "bold", angle = 30)
  expect_s3_class(xttl, "theme")
  expect_false(attr(xttl, "complete"))

  yttl <- yaxis_title(size = 14)
  expect_s3_class(yttl, "theme")
  expect_false(attr(yttl, "complete"))
})


# tick marks --------------------------------------------------
test_that("xaxis_ticks, yaxis_ticks return correct theme objects", {
  expect_s3_class(xaxis_ticks(FALSE)[["axis.ticks.x"]], "element_blank")
  expect_s3_class(yaxis_ticks(FALSE)[["axis.ticks.y"]], "element_blank")

  xticks <- xaxis_ticks(size = 0.5, color = "red")
  expect_s3_class(xticks, "theme")
  expect_equivalent(xticks,
    list(axis.ticks.x = ggplot2::element_line(size = 0.5, color = "red"))
  )
  expect_false(attr(xticks, "complete"))

  yticks <- yaxis_ticks(linetype = 2)
  expect_s3_class(yticks, "theme")
  expect_equivalent(yticks,
    list(axis.ticks.y = ggplot2::element_line(linetype = 2))
  )
  expect_false(attr(yticks, "complete"))
})
