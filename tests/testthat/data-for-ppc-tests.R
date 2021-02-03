set.seed(8420)
y <- rnorm(100)
yrep <- matrix(rnorm(2500), ncol = 100)
group <- gl(4, 25, labels = LETTERS[1:4])

y2 <- rpois(30, 1)
yrep2 <- matrix(rpois(30, 1), ncol = 30)
group2 <- rep(1, 30)


# for vdiffr visual tests
set.seed(11172017)
vdiff_y <- rnorm(100)
vdiff_yrep <- matrix(rnorm(2500), ncol = 100)
vdiff_group <- gl(4, 25, labels = LETTERS[1:4])

vdiff_y2 <- rpois(30, 1)
vdiff_yrep2 <- matrix(rpois(30 * 10, 1), ncol = 30, nrow = 10)
vdiff_group2 <- rep_len(c(1,2), length.out = 30)
set.seed(seed = NULL)
