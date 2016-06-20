y <- rnorm(100)
yrep <- matrix(rnorm(2500), ncol = 100)
group <- gl(4, 25, labels = LETTERS[1:4])

y2 <- rpois(30, 1)
yrep2 <- matrix(rpois(30, 1), ncol = 30)
group2 <- rep(1, 30)
