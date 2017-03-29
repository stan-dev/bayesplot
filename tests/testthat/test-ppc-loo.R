library(bayesplot)
suppressPackageStartupMessages(library(rstanarm))
suppressPackageStartupMessages(library(loo))
context("PPC: loo")

options(useFancyQuotes = FALSE)

ITER <- 1000
CHAINS <- 3
capture.output(
  fit <- stan_glm(mpg ~ wt + am, data = mtcars,
                  iter = ITER, chains = CHAINS, refresh = 0)
)
y <- fit$y
yrep <- posterior_predict(fit)

suppressWarnings(
  lw <- psislw(-log_lik(fit), cores = 2)$lw_smooth
)
suppressWarnings(
  pits <- rstantools::loo_pit(yrep, y, lw)
)


test_that("ppc_loo_pit returns ggplot object", {
  expect_gg(p1 <- ppc_loo_pit(y, yrep, lw))
  expect_equal(p1$labels$x, "Uniform")

  expect_gg(p2 <- ppc_loo_pit(y, yrep, lw, compare = "normal"))
  expect_equal(p2$labels$x, "Normal")
})

test_that("ppc_loo_pit works when pit specified instead of y,yrep,lw", {
  expect_gg(ppc_loo_pit(pit = pits))
  expect_warning(
    ppc_loo_pit(y = y, yrep = yrep, lw = lw, pit = pits),
    "Ignoring 'y','yrep','lw' because 'pit' was specified"
  )
})

test_that("ppc_loo_intervals returns ggplot object", {
  expect_gg(ppc_loo_intervals(y, yrep, lw))
  expect_gg(g <- ppc_loo_intervals(y, yrep, lw, order = "median"))
  expect_s3_class(g$data$x, "factor")
  expect_equal(nlevels(g$data$x), length(g$data$x))
})
test_that("ppc_loo_ribbon returns ggplot object", {
  expect_gg(ppc_loo_ribbon(y, yrep, lw, prob = 0.7, alpha = 0.1))
})


test_that("errors if dimensions of yrep and lw don't match", {
  expect_error(
    ppc_loo_pit(y, yrep, lw[, 1:5]),
    "identical(dim(yrep), dim(lw)) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    ppc_loo_intervals(y, yrep, lw[, 1:5]),
    "identical(dim(yrep), dim(lw)) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    ppc_loo_intervals(y, yrep, lw[, 1]),
    "identical(dim(yrep), dim(lw)) is not TRUE",
    fixed = TRUE
  )
})


