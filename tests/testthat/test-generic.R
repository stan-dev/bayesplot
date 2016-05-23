library(ppcheck)
context("pp_check generic")

test_that("pp_check method can be defined", {
  pp_check.foo <- function(object, ..., type = c("multiple", "overlaid")) {
    y <- object[["y"]]
    yrep <- object[["yrep"]]
    switch(match.arg(type),
           multiple = ppc_hist(y, yrep[1:min(8, nrow(yrep)),, drop = FALSE]),
           overlaid = ppc_dens_overlay(y, yrep)
    )
  }

  x <- structure(
    list(y = rnorm(50), yrep = matrix(rnorm(500), ncol = 50)),
    class = "foo"
  )

  expect_gg(pp_check(x))
  expect_gg(pp_check(x, type = "overlaid"))
})


