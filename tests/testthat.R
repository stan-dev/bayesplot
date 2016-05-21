library(testthat)
library(ppcheck)


expect_gg <- function(x) expect_is(x, "ggplot")
y <- rnorm(100)
yrep <- matrix(rnorm(2500), ncol = 100)
y1 <- rnorm(100)
yrep1 <- matrix(rnorm(100), ncol = 100)
y2 <- rnorm(30)
yrep2 <- matrix(rnorm(3000), ncol = 30)

test_check("ppcheck")
